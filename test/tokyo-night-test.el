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
        (expect (cdr entry) :to-match "\\`#[0-9a-fA-F]\\{6\\}\\'")))))

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
