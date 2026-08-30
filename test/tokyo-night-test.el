;;; tokyo-night-test.el --- Tests for tokyo-night -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Buttercup test suite for the Tokyo Night theme family.
;;
;; Face assertions read directly from the `theme-face' property rather
;; than going through `face-attribute' - in batch mode, faces aren't
;; recomputed to reflect theme specs, so `face-attribute' would miss
;; what the theme actually sets.  `theme-face' is the source of truth.
;;

;;; Code:

(require 'buttercup)
(require 'tokyo-night)

;; Make theme files loadable.
(let ((dir (file-name-directory
            (or load-file-name buffer-file-name default-directory))))
  (add-to-list 'custom-theme-load-path
               (expand-file-name ".." dir)))

(defvar tokyo-night-test--variants
  '(tokyo-night tokyo-night-storm tokyo-night-moon tokyo-night-day)
  "All theme variants exercised by the suite.")

(defun tokyo-night-test--palette (variant)
  "Return the colors-alist for VARIANT.
Handles the naming quirk that the default Night theme uses
`tokyo-night-colors-alist' rather than `tokyo-night-night-colors-alist'."
  (symbol-value
   (if (eq variant 'tokyo-night)
       'tokyo-night-colors-alist
     (intern (format "%s-colors-alist" variant)))))

(defun tokyo-night-test--luminance (hex)
  "Return the WCAG relative luminance of the color HEX."
  (let ((channels (mapcar
                   (lambda (offset)
                     (let ((v (/ (string-to-number
                                  (substring hex offset (+ offset 2)) 16)
                                 255.0)))
                       (if (<= v 0.04045)
                           (/ v 12.92)
                         (expt (/ (+ v 0.055) 1.055) 2.4))))
                   '(1 3 5))))
    (+ (* 0.2126 (nth 0 channels))
       (* 0.7152 (nth 1 channels))
       (* 0.0722 (nth 2 channels)))))

(defun tokyo-night-test--contrast (a b)
  "Return the WCAG contrast ratio between the colors A and B."
  (let* ((la (tokyo-night-test--luminance a))
         (lb (tokyo-night-test--luminance b))
         (lighter (max la lb))
         (darker (min la lb)))
    (/ (+ lighter 0.05) (+ darker 0.05))))

(defun tokyo-night-test--reload (variant)
  "Disable any active Tokyo Night theme and (re-)load VARIANT.
Reloading re-evaluates the theme file, which picks up any let-bound
`tokyo-night-scale-headings' the caller wants to exercise."
  (dolist (v tokyo-night-test--variants)
    (when (custom-theme-enabled-p v)
      (disable-theme v))
    (put v 'theme-settings nil)
    (setq custom-known-themes (delq v custom-known-themes)))
  (load-theme variant t))

(defun tokyo-night-test--face-attr (face variant attr)
  "Return ATTR from FACE's theme-face spec for VARIANT, or nil.
Reads directly from the theme-face property so we don't depend on
frame-side face recomputation (which is unreliable in batch)."
  (let* ((theme-face (get face 'theme-face))
         (entry     (assoc variant theme-face))
         (specs     (cadr entry))
         (first     (car specs))
         (props     (cadr first)))
    (plist-get props attr)))

;;; Heading scaling

(describe "tokyo-night-scale-headings"
  (after-each
    (dolist (v tokyo-night-test--variants)
      (when (custom-theme-enabled-p v)
        (disable-theme v))))

  (describe "when enabled (default)"
    (before-each
      (let ((tokyo-night-scale-headings t))
        (tokyo-night-test--reload 'tokyo-night)))

    (it "scales outline-1..3"
      (expect (tokyo-night-test--face-attr 'outline-1 'tokyo-night :height) :to-equal 1.3)
      (expect (tokyo-night-test--face-attr 'outline-2 'tokyo-night :height) :to-equal 1.2)
      (expect (tokyo-night-test--face-attr 'outline-3 'tokyo-night :height) :to-equal 1.1))

    (it "leaves outline-4..8 without a :height"
      (dolist (face '(outline-4 outline-5 outline-6 outline-7 outline-8))
        (expect (tokyo-night-test--face-attr face 'tokyo-night :height) :to-be nil)))

    (it "scales org-document-title via h-doc"
      (expect (tokyo-night-test--face-attr 'org-document-title 'tokyo-night :height) :to-equal 1.4))

    (it "scales shr-h1..3"
      (expect (tokyo-night-test--face-attr 'shr-h1 'tokyo-night :height) :to-equal 1.3)
      (expect (tokyo-night-test--face-attr 'shr-h2 'tokyo-night :height) :to-equal 1.2)
      (expect (tokyo-night-test--face-attr 'shr-h3 'tokyo-night :height) :to-equal 1.1)))

  (describe "when disabled"
    (before-each
      (let ((tokyo-night-scale-headings nil))
        (tokyo-night-test--reload 'tokyo-night)))

    (it "leaves outline-1..3 at 1.0"
      (expect (tokyo-night-test--face-attr 'outline-1 'tokyo-night :height) :to-equal 1.0)
      (expect (tokyo-night-test--face-attr 'outline-2 'tokyo-night :height) :to-equal 1.0)
      (expect (tokyo-night-test--face-attr 'outline-3 'tokyo-night :height) :to-equal 1.0))

    (it "leaves org-document-title at 1.0"
      (expect (tokyo-night-test--face-attr 'org-document-title 'tokyo-night :height) :to-equal 1.0))

    (it "leaves shr top levels at 1.0"
      (dolist (face '(shr-h1 shr-h2 shr-h3))
        (expect (tokyo-night-test--face-attr face 'tokyo-night :height) :to-equal 1.0)))))

;;; Palette integrity

(describe "color palettes"
  (it "define the same set of color keys across all variants"
    (let ((night (sort (mapcar #'car (tokyo-night-test--palette 'tokyo-night))       #'string<))
          (storm (sort (mapcar #'car (tokyo-night-test--palette 'tokyo-night-storm)) #'string<))
          (moon  (sort (mapcar #'car (tokyo-night-test--palette 'tokyo-night-moon))  #'string<))
          (day   (sort (mapcar #'car (tokyo-night-test--palette 'tokyo-night-day))   #'string<)))
      (expect storm :to-equal night)
      (expect moon  :to-equal night)
      (expect day   :to-equal night)))

  (it "have hex-formatted color values"
    (dolist (variant tokyo-night-test--variants)
      (dolist (entry (tokyo-night-test--palette variant))
        (expect (cdr entry) :to-match "\\`#[0-9a-fA-F]\\{6\\}\\'"))))

  ;; `tokyo-bg-highlight' backs `highlight' and friends, so it has to read as
  ;; a highlight.  Storm once carried Night's value on a much lighter
  ;; background and came out at 1.08, near enough to invisible.
  (it "lift tokyo-bg-highlight clear of tokyo-bg in every variant"
    (dolist (variant tokyo-night-test--variants)
      (let* ((palette (tokyo-night-test--palette variant))
             (bg (cdr (assoc "tokyo-bg" palette)))
             (highlight (cdr (assoc "tokyo-bg-highlight" palette))))
        (expect (tokyo-night-test--contrast bg highlight)
                :to-be-greater-than 1.15)))))

;;; Code-block backgrounds
;;
;; Guards against the regression seen in a sibling theme where
;; markdown-code-face was silent on :background and code blocks picked
;; up a dark fallback on light variants.

(describe "markdown-code-face background"
  (after-each
    (dolist (v tokyo-night-test--variants)
      (when (custom-theme-enabled-p v)
        (disable-theme v))))

  (dolist (variant tokyo-night-test--variants)
    (it (format "sets an explicit :background in %s" variant)
      (tokyo-night-test--reload variant)
      (expect (tokyo-night-test--face-attr 'markdown-code-face variant :background)
              :not :to-be nil))))

;;; Variant loading smoke tests

(describe "theme loading"
  (after-each
    (dolist (v tokyo-night-test--variants)
      (when (custom-theme-enabled-p v)
        (disable-theme v))))

  (dolist (variant tokyo-night-test--variants)
    (it (format "loads %s without error" variant)
      (expect (load-theme variant t) :to-be-truthy)
      (expect (custom-theme-enabled-p variant) :to-be-truthy))))

;;; tokyo-night-test.el ends here
