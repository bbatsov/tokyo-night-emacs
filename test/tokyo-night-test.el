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
