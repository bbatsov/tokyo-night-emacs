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

(defconst tokyo-night-test--source-file
  (expand-file-name
   "../tokyo-night.el"
   (file-name-directory (or load-file-name buffer-file-name default-directory)))
  "Path to the theme source, for the checks that read it as text.")

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

(defun tokyo-night-test--blend (fg bg alpha)
  "Blend FG into BG by ALPHA, the way upstream derives its shades."
  (apply #'format "#%02x%02x%02x"
         (mapcar (lambda (offset)
                   (let ((f (string-to-number (substring fg offset (+ offset 2)) 16))
                         (b (string-to-number (substring bg offset (+ offset 2)) 16)))
                     (floor (+ 0.5 (+ (* alpha f) (* (- 1 alpha) b))))))
                 '(1 3 5))))

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
      (expect (tokyo-night-test--face-attr 'shr-h3 'tokyo-night :height) :to-equal 1.1))

    (it "scales asciidoc titles"
      (expect (tokyo-night-test--face-attr 'asciidoc-document-title-face 'tokyo-night :height) :to-equal 1.4)
      (expect (tokyo-night-test--face-attr 'asciidoc-title-1-face 'tokyo-night :height) :to-equal 1.3)
      (expect (tokyo-night-test--face-attr 'asciidoc-title-2-face 'tokyo-night :height) :to-equal 1.2)
      (expect (tokyo-night-test--face-attr 'asciidoc-title-3-face 'tokyo-night :height) :to-equal 1.1)))

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
        (expect (tokyo-night-test--face-attr face 'tokyo-night :height) :to-equal 1.0))))

  (describe "with custom scale factors"
    (before-each
      (let ((tokyo-night-scale-headings t)
            (tokyo-night-height-1 2.0)
            (tokyo-night-height-doc-title 2.5))
        (tokyo-night-test--reload 'tokyo-night)))

    (it "honors the per-level height factors"
      (expect (tokyo-night-test--face-attr 'outline-1 'tokyo-night :height) :to-equal 2.0)
      (expect (tokyo-night-test--face-attr 'markdown-header-face-1 'tokyo-night :height) :to-equal 2.0)
      (expect (tokyo-night-test--face-attr 'org-document-title 'tokyo-night :height) :to-equal 2.5))))

;;; Italic toggles

(describe "italic toggles"
  (after-each
    (dolist (v tokyo-night-test--variants)
      (when (custom-theme-enabled-p v)
        (disable-theme v))))

  (it "renders comments and keywords italic by default"
    (tokyo-night-test--reload 'tokyo-night)
    (expect (tokyo-night-test--face-attr 'font-lock-comment-face 'tokyo-night :slant) :to-equal 'italic)
    (expect (tokyo-night-test--face-attr 'font-lock-keyword-face 'tokyo-night :slant) :to-equal 'italic))

  (it "drops the comment italic when disabled"
    (let ((tokyo-night-italic-comments nil))
      (tokyo-night-test--reload 'tokyo-night))
    (expect (tokyo-night-test--face-attr 'font-lock-comment-face 'tokyo-night :slant) :to-equal 'normal)
    (expect (tokyo-night-test--face-attr 'font-lock-comment-delimiter-face 'tokyo-night :slant) :to-equal 'normal)
    ;; keywords stay italic
    (expect (tokyo-night-test--face-attr 'font-lock-keyword-face 'tokyo-night :slant) :to-equal 'italic))

  (it "drops the keyword italic when disabled"
    (let ((tokyo-night-italic-keywords nil))
      (tokyo-night-test--reload 'tokyo-night))
    (expect (tokyo-night-test--face-attr 'font-lock-keyword-face 'tokyo-night :slant) :to-equal 'normal)
    ;; comments stay italic
    (expect (tokyo-night-test--face-attr 'font-lock-comment-face 'tokyo-night :slant) :to-equal 'italic)))

;;; Variable-pitch headings

(describe "variable-pitch headings"
  (after-each
    (dolist (v tokyo-night-test--variants)
      (when (custom-theme-enabled-p v)
        (disable-theme v))))

  (it "leaves headings fixed-pitch by default"
    (tokyo-night-test--reload 'tokyo-night)
    (expect (tokyo-night-test--face-attr 'outline-1 'tokyo-night :inherit) :to-equal 'default)
    (expect (tokyo-night-test--face-attr 'markdown-header-face-1 'tokyo-night :inherit) :to-equal 'default))

  (it "switches headings to variable-pitch when enabled"
    (let ((tokyo-night-use-variable-pitch t))
      (tokyo-night-test--reload 'tokyo-night))
    (dolist (face '(outline-1 org-document-title markdown-header-face-1
                    asciidoc-title-1-face shr-h1 info-title-1))
      (expect (tokyo-night-test--face-attr face 'tokyo-night :inherit) :to-equal 'variable-pitch))))

;;; Flat mode line

(describe "flat mode line"
  (after-each
    (dolist (v tokyo-night-test--variants)
      (when (custom-theme-enabled-p v)
        (disable-theme v))))

  (it "boxes the mode line by default"
    (tokyo-night-test--reload 'tokyo-night)
    (expect (tokyo-night-test--face-attr 'mode-line 'tokyo-night :box) :not :to-be nil))

  (it "drops the box when flat"
    (let ((tokyo-night-flat-mode-line t))
      (tokyo-night-test--reload 'tokyo-night))
    (expect (tokyo-night-test--face-attr 'mode-line 'tokyo-night :box) :to-be nil)
    (expect (tokyo-night-test--face-attr 'mode-line-inactive 'tokyo-night :box) :to-be nil)))

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
  ;; The diff backgrounds are derived rather than hand-picked, so that a
  ;; value tuned against one variant's background can't be copied onto
  ;; another's and quietly go flat.  See DESIGN.md.
  (it "derive the diff backgrounds from the variant's own background"
    (dolist (variant tokyo-night-test--variants)
      (let* ((palette (tokyo-night-test--palette variant))
             (colour (lambda (name) (cdr (assoc name palette))))
             (bg (funcall colour "tokyo-bg")))
        (dolist (recipe '(("tokyo-diff-add-bg" "tokyo-green"     0.22)
                          ("tokyo-diff-del-bg" "tokyo-red-dark"  0.25)
                          ("tokyo-diff-chg-bg" "tokyo-blue-dark" 0.30)))
          (expect (funcall colour (nth 0 recipe))
                  :to-equal
                  (tokyo-night-test--blend (funcall colour (nth 1 recipe))
                                           bg (nth 2 recipe)))))))

  (it "lift the diff backgrounds clear of tokyo-bg in every variant"
    (dolist (variant tokyo-night-test--variants)
      (let* ((palette (tokyo-night-test--palette variant))
             (bg (cdr (assoc "tokyo-bg" palette))))
        (dolist (name '("tokyo-diff-add-bg" "tokyo-diff-del-bg" "tokyo-diff-chg-bg"))
          (expect (tokyo-night-test--contrast bg (cdr (assoc name palette)))
                  :to-be-greater-than 1.15)))))

  (it "lift tokyo-bg-highlight clear of tokyo-bg in every variant"
    (dolist (variant tokyo-night-test--variants)
      (let* ((palette (tokyo-night-test--palette variant))
             (bg (cdr (assoc "tokyo-bg" palette)))
             (highlight (cdr (assoc "tokyo-bg-highlight" palette))))
        (expect (tokyo-night-test--contrast bg highlight)
                :to-be-greater-than 1.15)))))

;;; Text has to be readable on its own background
;;
;; The palette tests below compare backgrounds against `tokyo-bg'.  That says
;; nothing about whether a face's own text reads on its own background, which
;; is how `match' ended up as blue on blue in Day and `diff-removed' as a
;; muted red on a red wash.

(defconst tokyo-night-test--dim-colors
  '("tokyo-comment" "tokyo-dark3" "tokyo-dark5" "tokyo-fg-gutter" "tokyo-line-nr"
    "tokyo-whitespace" "tokyo-git-ignored" "tokyo-indent" "tokyo-bracket"
    "tokyo-terminal-blk")
  "Palette entries DESIGN.md hands to de-emphasized text.
A face reaching for one of these is asking to recede, so it opts out of
the contrast floor rather than needing an entry in an exception list.")

(defconst tokyo-night-test--legibility-floor 3.0
  "Contrast a face's own text must reach against its own background.")

(defconst tokyo-night-test--day-legibility-floor 2.1
  "The same floor for Day, which upstream builds to be less contrasty.
Day inverts Night's HSLuv lightness and then lightens whatever came out
dark, scaled by upstream's `day_brightness', which its own docs describe
as running \"from dull to vibrant colors\".  Vibrant costs contrast: Day's
accents sit at 3-4:1 against the background where the dark variants reach
6-10:1, so pairings that are comfortable there land near 2:1 here.  That
is Tokyo Night's decision rather than drift, so this floor pins the
current worst value instead of demanding the dark variants' 3.0.  See
DESIGN.md before reaching for the palette to close the gap.")

(defun tokyo-night-test--dim-p (variant color)
  "Return non-nil if COLOR is one of VARIANT's de-emphasized palette entries."
  (let ((palette (tokyo-night-test--palette variant)))
    (seq-some (lambda (name) (equal color (cdr (assoc name palette))))
              tokyo-night-test--dim-colors)))

(describe "text on its own background"
  (after-each
    (dolist (v tokyo-night-test--variants)
      (when (custom-theme-enabled-p v)
        (disable-theme v))))

  (dolist (variant tokyo-night-test--variants)
    (it (format "stays readable in %s" variant)
      (tokyo-night-test--reload variant)
      (let ((floor (if (eq variant 'tokyo-night-day)
                       tokyo-night-test--day-legibility-floor
                     tokyo-night-test--legibility-floor))
            (illegible '()))
        (mapatoms
         (lambda (sym)
           (let ((fg (tokyo-night-test--face-attr sym variant :foreground))
                 (bg (tokyo-night-test--face-attr sym variant :background)))
             (when (and (stringp fg) (stringp bg)
                        ;; the ansi and term color faces set foreground and
                        ;; background to the same value on purpose, so a
                        ;; terminal can use either
                        (not (string-match-p "\\`\\(ansi\\|term\\)-color-" (symbol-name sym)))
                        (not (tokyo-night-test--dim-p variant fg))
                        (< (tokyo-night-test--contrast fg bg) floor))
               (push (list sym (tokyo-night-test--contrast fg bg)) illegible)))))
        (expect illegible :to-equal '())))))

;;; Backgrounds that match the buffer background
;;
;; Setting `:background' to the variant's own `tokyo-bg' is not the same as
;; leaving it unset.  Unset lets whatever is underneath show through, such as
;; `hl-line' or `region'; setting it explicitly punches a hole through them.
;; So it lifts nothing and suppresses highlighting that should be visible.
;; A handful of faces genuinely want to sit on the buffer background, and
;; they are named here so any new arrival has to be a deliberate choice.

(defconst tokyo-night-test--flat-background-faces
  '(default                          ; definitional
    fringe                           ; blends into the buffer it borders
    term                             ; the terminal's own background
    line-number                      ; the number column sits on the buffer
    line-number-current-line
    line-number-major-tick
    line-number-minor-tick
    centaur-tabs-selected            ; a selected tab joins the buffer below it
    centaur-tabs-selected-modified
    tab-bar-tab
    tab-bar-tab-group-current
    tab-line-tab
    tab-line-tab-current)
  "Faces allowed to set `:background' to the variant's own `tokyo-bg'.")

(describe "faces sitting on the buffer background"
  (after-each
    (dolist (v tokyo-night-test--variants)
      (when (custom-theme-enabled-p v)
        (disable-theme v))))

  (dolist (variant tokyo-night-test--variants)
    (it (format "only lets the allowed faces match tokyo-bg in %s" variant)
      (tokyo-night-test--reload variant)
      (let* ((palette (tokyo-night-test--palette variant))
             (bg (cdr (assoc "tokyo-bg" palette)))
             (offenders '()))
        (mapatoms
         (lambda (sym)
           (when (and (get sym 'theme-face)
                      (assoc variant (get sym 'theme-face))
                      (equal (tokyo-night-test--face-attr sym variant :background) bg)
                      (not (memq sym tokyo-night-test--flat-background-faces)))
             (push sym offenders))))
        (expect offenders :to-equal '())))))

;;; Smartparens mirrors the built-in paren faces
;;
;; DESIGN.md asks for these to look identical, and they drifted once
;; already: 1.0.0 gave `show-paren-mismatch' a visible background and left
;; `sp-show-pair-mismatch-face' sitting on plain `tokyo-bg'.

(describe "smartparens pair faces"
  (after-each
    (dolist (v tokyo-night-test--variants)
      (when (custom-theme-enabled-p v)
        (disable-theme v))))

  (dolist (pair '((sp-show-pair-match-face    . show-paren-match)
                  (sp-show-pair-mismatch-face . show-paren-mismatch)))
    (dolist (variant tokyo-night-test--variants)
      (it (format "gives %s the same colors as %s in %s"
                  (car pair) (cdr pair) variant)
        (tokyo-night-test--reload variant)
        (dolist (attr '(:foreground :background))
          (expect (tokyo-night-test--face-attr (car pair) variant attr)
                  :to-equal
                  (tokyo-night-test--face-attr (cdr pair) variant attr)))))))

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

;;; The shape of the source itself
;;
;; Cheap guards over things that are true today and would rot silently: 1300
;; face definitions is more than anyone reviews by hand.

(defun tokyo-night-test--face-body ()
  "Return the part of the source holding the face definitions."
  (with-temp-buffer
    (insert-file-contents tokyo-night-test--source-file)
    (goto-char (point-min))
    (search-forward "basic coloring")
    (buffer-substring-no-properties (point) (point-max))))

(defun tokyo-night-test--matches (regexp string &optional group)
  "Return every GROUP match of REGEXP in STRING."
  (let ((start 0) (found '()))
    (while (string-match regexp string start)
      (push (match-string (or group 1) string) found)
      (setq start (match-end 0)))
    (nreverse found)))

(describe "the source"
  (it "defines each face exactly once"
    (let* ((faces (tokyo-night-test--matches
                   "`(\\([^ ()]+\\) ((,class" (tokyo-night-test--face-body)))
           (seen (make-hash-table :test 'equal))
           (dupes '()))
      (dolist (face faces)
        (when (gethash face seen) (push face dupes))
        (puthash face t seen))
      (expect (delete-dups dupes) :to-equal '())))

  (it "takes every color from the palette rather than hardcoding it"
    (expect (tokyo-night-test--matches
             ":\\(?:fore\\|back\\)ground \\(\"#[0-9a-fA-F]\\{6\\}\"\\)"
             (tokyo-night-test--face-body))
            :to-equal '()))

  ;; The palette is public: `tokyo-night-get-color' and
  ;; `tokyo-night-with-colors' hand it to users, so an entry no face uses is
  ;; still worth carrying.  It just has to be deliberate rather than a
  ;; leftover, which is what this list is for.
  (it "uses every palette entry, bar the ones kept only for the API"
    (let* ((palette-only '("tokyo-blue7"))
           (body (tokyo-night-test--face-body))
           (used (tokyo-night-test--matches ",\\(tokyo-[a-z0-9-]+\\)" body))
           (unused (seq-remove (lambda (name)
                                 (or (member name used) (member name palette-only)))
                               (mapcar #'car (tokyo-night-test--palette 'tokyo-night)))))
      (expect unused :to-equal '())))

  (it "only refers to colors the palette defines"
    (let* ((defined (mapcar #'car (tokyo-night-test--palette 'tokyo-night)))
           (used (delete-dups (tokyo-night-test--matches
                               ",\\(tokyo-[a-z0-9-]+\\)" (tokyo-night-test--face-body)))))
      (expect (seq-remove (lambda (name) (member name defined)) used)
              :to-equal '()))))

;;; Options in combination
;;
;; Each toggle is covered on its own above.  Heading scale and variable pitch
;; are the pair that touches the same faces, so they are the pair worth
;; checking together.

(describe "scale-headings and use-variable-pitch together"
  (after-each
    (dolist (v tokyo-night-test--variants)
      (when (custom-theme-enabled-p v)
        (disable-theme v))))

  (it "scales and switches pitch when both are on"
    (let ((tokyo-night-use-variable-pitch t)
          (tokyo-night-scale-headings t))
      (tokyo-night-test--reload 'tokyo-night))
    (expect (tokyo-night-test--face-attr 'outline-1 'tokyo-night :inherit)
            :to-equal 'variable-pitch)
    (expect (tokyo-night-test--face-attr 'outline-1 'tokyo-night :height)
            :to-equal tokyo-night-height-1))

  (it "keeps the pitch switch when scaling is off"
    (let ((tokyo-night-use-variable-pitch t)
          (tokyo-night-scale-headings nil))
      (tokyo-night-test--reload 'tokyo-night))
    (expect (tokyo-night-test--face-attr 'outline-1 'tokyo-night :inherit)
            :to-equal 'variable-pitch)
    (expect (tokyo-night-test--face-attr 'outline-1 'tokyo-night :height)
            :to-equal 1.0))

  (it "keeps scaling when the pitch switch is off"
    (let ((tokyo-night-use-variable-pitch nil)
          (tokyo-night-scale-headings t))
      (tokyo-night-test--reload 'tokyo-night))
    (expect (tokyo-night-test--face-attr 'outline-1 'tokyo-night :inherit)
            :to-equal 'default)
    (expect (tokyo-night-test--face-attr 'outline-1 'tokyo-night :height)
            :to-equal tokyo-night-height-1))

  (it "orders the heading levels from largest to smallest"
    (let ((tokyo-night-scale-headings t))
      (tokyo-night-test--reload 'tokyo-night))
    (let ((heights (mapcar (lambda (face)
                             (tokyo-night-test--face-attr face 'tokyo-night :height))
                           '(outline-1 outline-2 outline-3))))
      (expect heights :to-equal (reverse (sort (copy-sequence heights) #'<)))
      (expect (car (last heights)) :to-be-greater-than 1.0))))

;;; Package headers
;;
;; package-lint used to cover this and was dropped, since the only thing it
;; reported for this project was a false positive about a face it mistook for
;; a removed function.  These are the parts of its job worth keeping.

(defconst tokyo-night-test--source-files
  (let ((dir (file-name-directory tokyo-night-test--source-file)))
    (mapcar (lambda (name) (expand-file-name name dir))
            '("tokyo-night.el" "tokyo-night-theme.el" "tokyo-night-storm-theme.el"
              "tokyo-night-moon-theme.el" "tokyo-night-day-theme.el")))
  "Every file that ships in the package.")

(defun tokyo-night-test--file-text (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(describe "package headers"
  (dolist (file tokyo-night-test--source-files)
    (let ((name (file-name-nondirectory file)))
      (it (format "%s opens with a summary and a lexical-binding cookie" name)
        (let ((first-line (car (split-string (tokyo-night-test--file-text file) "\n"))))
          (expect first-line :to-match
                  (rx-to-string '(seq ";;; " (1+ nonl) " --- " (1+ nonl)
                                      "-*- lexical-binding: t; -*-")))))

      (it (format "%s closes with the conventional footer" name)
        (expect (string-trim-right (tokyo-night-test--file-text file))
                :to-match (rx-to-string `(seq ";;; " ,name " ends here" eos))))))

  (it "declares the headers a package needs"
    (let ((text (tokyo-night-test--file-text tokyo-night-test--source-file)))
      (dolist (header '("Author" "URL" "Version" "Package-Requires" "Keywords"))
        (expect (string-match-p (concat "^;; " header ": ") text) :not :to-be nil))))

  (it "declares a Package-Requires that reads back as an alist"
    (let* ((text (tokyo-night-test--file-text tokyo-night-test--source-file))
           (_ (string-match "^;; Package-Requires: \\(.*\\)$" text))
           (deps (car (read-from-string (match-string 1 text)))))
      (expect (assq 'emacs deps) :not :to-be nil)
      (expect (version-to-list (cadr (assq 'emacs deps))) :not :to-be nil)))

  (it "gives Version something version-to-list accepts"
    (let ((text (tokyo-night-test--file-text tokyo-night-test--source-file)))
      (string-match "^;; Version: \\(.*\\)$" text)
      (expect (version-to-list (string-trim (match-string 1 text))) :not :to-be nil))))

;;; Emphasis restraint
;;
;; DESIGN.md: "Avoid combining too many attributes (bold + italic + underline
;; + color).  One or two is usually enough."

(describe "emphasis"
  (after-each
    (dolist (v tokyo-night-test--variants)
      (when (custom-theme-enabled-p v)
        (disable-theme v))))

  (it "never stacks three emphasis attributes on one face"
    (tokyo-night-test--reload 'tokyo-night)
    (let ((overwrought '()))
      (mapatoms
       (lambda (sym)
         (when (assoc 'tokyo-night (get sym 'theme-face))
           (let ((n (seq-count
                     (lambda (attr)
                       (tokyo-night-test--face-attr sym 'tokyo-night attr))
                     '(:weight :slant :underline :box :overline :strike-through))))
             (when (> n 2) (push sym overwrought))))))
      (expect overwrought :to-equal '()))))

;;; Public API

(describe "the public API"
  (after-each
    (dolist (v tokyo-night-test--variants)
      (when (custom-theme-enabled-p v)
        (disable-theme v)))
    (setq tokyo-night-override-colors-alist '()))

  (it "reads a color from the active variant"
    (tokyo-night-test--reload 'tokyo-night-storm)
    (expect (tokyo-night-get-color "tokyo-bg") :to-equal "#24283b"))

  (it "reads a color from a variant that isn't active"
    (tokyo-night-test--reload 'tokyo-night-storm)
    (expect (tokyo-night-get-color "tokyo-bg" 'tokyo-night-day) :to-equal "#e1e2e7"))

  ;; `enable-theme-functions' only exists from Emacs 29.  Without this, every
  ;; command that reads the active variant fails on 27 and 28 with "No Tokyo
  ;; theme is active", which is every Emacs in the support range below 29.
  (it "knows the active variant without enable-theme-functions"
    (let ((enable-theme-functions nil)
          (disable-theme-functions nil))
      (setq tokyo-night--current nil)
      (tokyo-night-test--reload 'tokyo-night-moon)
      (expect tokyo-night--current :to-be 'tokyo-night-moon)
      (expect (tokyo-night-get-color "tokyo-bg") :to-equal "#222436")))

  (it "binds every palette color inside tokyo-night-with-colors"
    (tokyo-night-test--reload 'tokyo-night-storm)
    (expect (tokyo-night-with-colors (list tokyo-bg tokyo-blue))
            :to-equal (list "#24283b" (tokyo-night-get-color "tokyo-blue"))))

  (it "lets tokyo-night-override-colors-alist win"
    (tokyo-night-test--reload 'tokyo-night-storm)
    (setq tokyo-night-override-colors-alist '(("tokyo-bg" . "#000000")))
    (expect (tokyo-night-get-color "tokyo-bg") :to-equal "#000000")
    (expect (tokyo-night-with-colors tokyo-bg) :to-equal "#000000"))

  (it "reapplies an override on reload"
    (tokyo-night-test--reload 'tokyo-night-storm)
    (setq tokyo-night-override-colors-alist '(("tokyo-bg" . "#010203")))
    (tokyo-night-reload)
    (expect (tokyo-night-test--face-attr 'default 'tokyo-night-storm :background)
            :to-equal "#010203"))

  (it "refuses to reload when no variant is active"
    (setq tokyo-night--current nil)
    (expect (tokyo-night-reload) :to-throw 'user-error))

  (it "renders the palette buffer without error"
    (tokyo-night-test--reload 'tokyo-night-storm)
    (tokyo-night-list-colors)
    (expect (get-buffer "*Tokyo Palette: tokyo-night-storm*") :not :to-be nil)))

;;; Switching between variants

(describe "tokyo-night-select"
  (after-each
    (dolist (v tokyo-night-test--variants)
      (when (custom-theme-enabled-p v)
        (disable-theme v))))

  (it "leaves exactly one variant enabled"
    (tokyo-night-test--reload 'tokyo-night)
    (spy-on 'completing-read :and-return-value "tokyo-night-moon")
    (tokyo-night-select)
    (expect (seq-filter #'custom-theme-enabled-p tokyo-night-test--variants)
            :to-equal '(tokyo-night-moon)))

  (it "tracks the variant it switched to"
    (tokyo-night-test--reload 'tokyo-night)
    (spy-on 'completing-read :and-return-value "tokyo-night-day")
    (tokyo-night-select)
    (expect tokyo-night--current :to-be 'tokyo-night-day))

  (it "runs tokyo-night-after-load-hook with the chosen variant"
    (let* ((seen '())
           (tokyo-night-after-load-hook (list (lambda (theme) (push theme seen)))))
      (spy-on 'completing-read :and-return-value "tokyo-night-storm")
      (tokyo-night-select)
      (expect seen :to-equal '(tokyo-night-storm)))))

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

;;; Package face coverage
;;
;; One entry per package: representative faces that the theme must set.
;; Guards against sections silently disappearing during refactors.

(defconst tokyo-night-test--package-faces
  '((anzu anzu-mode-line anzu-match-1 anzu-match-2 anzu-match-3
          anzu-replace-highlight anzu-replace-to)
    (jinx jinx-misspelled jinx-highlight jinx-save jinx-key jinx-annotation)
    (completion-preview completion-preview completion-preview-common
                        completion-preview-exact)
    (asciidoc-mode asciidoc-document-title-face asciidoc-title-1-face
                   asciidoc-title-5-face asciidoc-markup-face
                   asciidoc-code-face asciidoc-link-face asciidoc-url-face
                   asciidoc-metadata-key-face asciidoc-highlight-face
                   asciidoc-admonition-note-label-face
                   asciidoc-admonition-note-face
                   asciidoc-admonition-tip-label-face
                   asciidoc-admonition-important-label-face
                   asciidoc-admonition-caution-label-face
                   asciidoc-admonition-warning-label-face
                   asciidoc-admonition-warning-face)
    (cider cider-repl-prompt-face cider-repl-stdout-face
           cider-repl-stderr-face cider-error-highlight-face
           cider-warning-highlight-face cider-stacktrace-error-class-face
           cider-stacktrace-fn-face cider-deprecated-face
           cider-instrumented-face cider-traced-face
           cider-fringe-bad-face cider-reader-conditional-face
           nrepl-message-1-face nrepl-message-8-face)
    (vundo vundo-node vundo-stem vundo-branch-stem vundo-highlight
           vundo-saved vundo-last-saved vundo-diff-highlight)
    (volatile-highlights vhl/default-face)
    (easy-kill easy-kill-selection easy-kill-origin)
    (clojure-mode clojure-keyword-face clojure-character-face
                  clojure-discard-face)
    (corfu corfu-popupinfo)
    (copilot copilot-overlay-face)
    (git-timemachine git-timemachine-commit
                     git-timemachine-minibuffer-author-face
                     git-timemachine-minibuffer-detail-face)
    (haskell-mode haskell-keyword-face haskell-type-face
                  haskell-constructor-face haskell-definition-face
                  haskell-operator-face haskell-pragma-face
                  haskell-hole-face haskell-error-face haskell-warning-face
                  haskell-interactive-face-prompt
                  haskell-interactive-face-compile-error
                  haskell-interactive-face-result)
    (keycast keycast-key keycast-command)
    (dictionary dictionary-word-entry-face dictionary-word-definition-face
                dictionary-reference-face dictionary-button-face)
    (mistty mistty-fringe-face)
    (erlang erlang-font-lock-exported-function-name-face
            erlang-edoc-heading erlang-edoc-tag erlang-edoc-macro
            erlang-edoc-verbatim erlang-edoc-todo)
    (inf-ruby inf-ruby-result-overlay-face)
    (breadcrumb breadcrumb-face breadcrumb-imenu-leaf-face
                breadcrumb-imenu-crumbs-face breadcrumb-imenu-base-face
                breadcrumb-project-leaf-face breadcrumb-project-crumbs-face
                breadcrumb-project-base-face)
    (gptel gptel-context-highlight-face gptel-context-deletion-face
           gptel-rewrite-highlight-face gptel-response-highlight
           gptel-response-fringe-highlight))
  "Alist of (PACKAGE . FACES) the theme is expected to cover.")

(describe "package face coverage"
  (before-all
    (tokyo-night-test--reload 'tokyo-night))
  (after-all
    (disable-theme 'tokyo-night))

  (dolist (entry tokyo-night-test--package-faces)
    (let ((package (car entry))
          (faces (cdr entry)))
      (it (format "themes %s" package)
        (dolist (face faces)
          (expect (assq 'tokyo-night (get face 'theme-face))
                  :to-be-truthy)))))

  (it "gives jinx-misspelled the same underline as flyspell-incorrect"
    (expect (tokyo-night-test--face-attr 'jinx-misspelled 'tokyo-night :underline)
            :to-equal
            (tokyo-night-test--face-attr 'flyspell-incorrect 'tokyo-night :underline)))

  (it "styles inf-ruby's result overlay like cider's"
    (dolist (attr '(:foreground :background :box))
      (expect (tokyo-night-test--face-attr 'inf-ruby-result-overlay-face 'tokyo-night attr)
              :to-equal
              (tokyo-night-test--face-attr 'cider-result-overlay-face 'tokyo-night attr)))))

;;; tokyo-night-test.el ends here
