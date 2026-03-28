;;; tokyo-themes.el --- Shared infrastructure for Tokyo Night themes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: https://github.com/bbatsov/emacs-tokyo-themes
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces themes

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Shared color palettes and face-setting infrastructure for the Tokyo Night
;; family of themes.  This file is not a theme itself -- it is required by
;; the individual theme files (tokyo-night-theme.el, etc.).
;;
;; Based on folke's Tokyo Night for Neovim:
;; https://github.com/folke/tokyonight.nvim

;;; Code:

(require 'cl-lib)

(defgroup tokyo-themes nil
  "Tokyo Night theme family."
  :group 'faces
  :prefix "tokyo-night-"
  :link '(url-link :tag "GitHub" "https://github.com/bbatsov/emacs-tokyo-themes")
  :tag "Tokyo Night")

(defcustom tokyo-themes-override-colors-alist '()
  "Alist of color overrides applied to all variants.
Each entry should be a cons cell (NAME . VALUE) where NAME is a
color name from any variant's palette and VALUE is the
replacement hex color string."
  :type '(alist :key-type string :value-type string)
  :group 'tokyo-themes)

;;; Color Palettes

(defvar tokyo-night-colors-alist
  '(;; Background shades
    ("tn-bg-darkest"    . "#0C0E14")
    ("tn-bg-dark"       . "#16161e")
    ("tn-bg"            . "#1a1b26")
    ("tn-bg-highlight"  . "#292e42")
    ("tn-bg-line"       . "#1e202e")

    ;; Foreground shades
    ("tn-fg"            . "#c0caf5")
    ("tn-fg-dark"       . "#a9b1d6")
    ("tn-fg-muted"      . "#9aa5ce")
    ("tn-fg-gutter"     . "#3b4261")

    ;; UI elements
    ("tn-line-nr"       . "#363b54")
    ("tn-line-nr-cur"   . "#787c99")
    ("tn-indent"        . "#232433")
    ("tn-selection"     . "#515c7e")
    ("tn-dark3"         . "#545c7e")
    ("tn-dark5"         . "#737aa2")
    ("tn-terminal-blk"  . "#414868")
    ("tn-bracket"       . "#42465d")
    ("tn-whitespace"    . "#363b54")

    ;; Core accent colors
    ("tn-red"           . "#f7768e")
    ("tn-red-dark"      . "#db4b4b")
    ("tn-orange"        . "#ff9e64")
    ("tn-yellow"        . "#e0af68")
    ("tn-green"         . "#9ece6a")
    ("tn-teal"          . "#73daca")
    ("tn-teal-dark"     . "#41a6b5")
    ("tn-cyan"          . "#7dcfff")
    ("tn-cyan-bright"   . "#2ac3de")
    ("tn-cyan-support"  . "#0db9d7")
    ("tn-cyan-pale"     . "#89ddff")
    ("tn-cyan-ice"      . "#b4f9f8")
    ("tn-blue"          . "#7aa2f7")
    ("tn-blue-dark"     . "#3d59a1")
    ("tn-blue7"         . "#394b70")
    ("tn-magenta"       . "#bb9af7")
    ("tn-magenta-dark"  . "#9d7cd8")
    ("tn-magenta-hot"   . "#ff007c")

    ;; Comment
    ("tn-comment"       . "#565f89")

    ;; Git / Diff
    ("tn-git-add"       . "#449dab")
    ("tn-git-change"    . "#6183bb")
    ("tn-git-delete"    . "#914c54")
    ("tn-git-ignored"   . "#515670")
    ("tn-diff-add-bg"   . "#1a3a3a")
    ("tn-diff-del-bg"   . "#3a2020")
    ("tn-diff-chg-bg"   . "#1a2a4a")

    ;; Headings
    ("tn-heading1"      . "#89ddff")
    ("tn-heading2"      . "#61bdf2")
    ("tn-heading3"      . "#7aa2f7")
    ("tn-heading4"      . "#6d91de")
    ("tn-heading5"      . "#9aa5ce")
    ("tn-heading6"      . "#747ca1"))
  "The Tokyo Night (night) color palette.
Darkest background variant.")

(defvar tokyo-storm-colors-alist
  '(;; Background shades
    ("tn-bg-darkest"    . "#1b1e2d")
    ("tn-bg-dark"       . "#1f2335")
    ("tn-bg"            . "#24283b")
    ("tn-bg-highlight"  . "#292e42")
    ("tn-bg-line"       . "#262b3e")

    ;; Foreground shades
    ("tn-fg"            . "#c0caf5")
    ("tn-fg-dark"       . "#a9b1d6")
    ("tn-fg-muted"      . "#9aa5ce")
    ("tn-fg-gutter"     . "#3b4261")

    ;; UI elements
    ("tn-line-nr"       . "#363b54")
    ("tn-line-nr-cur"   . "#787c99")
    ("tn-indent"        . "#2c3048")
    ("tn-selection"     . "#515c7e")
    ("tn-dark3"         . "#545c7e")
    ("tn-dark5"         . "#737aa2")
    ("tn-terminal-blk"  . "#414868")
    ("tn-bracket"       . "#42465d")
    ("tn-whitespace"    . "#363b54")

    ;; Core accent colors
    ("tn-red"           . "#f7768e")
    ("tn-red-dark"      . "#db4b4b")
    ("tn-orange"        . "#ff9e64")
    ("tn-yellow"        . "#e0af68")
    ("tn-green"         . "#9ece6a")
    ("tn-teal"          . "#73daca")
    ("tn-teal-dark"     . "#41a6b5")
    ("tn-cyan"          . "#7dcfff")
    ("tn-cyan-bright"   . "#2ac3de")
    ("tn-cyan-support"  . "#0db9d7")
    ("tn-cyan-pale"     . "#89ddff")
    ("tn-cyan-ice"      . "#b4f9f8")
    ("tn-blue"          . "#7aa2f7")
    ("tn-blue-dark"     . "#3d59a1")
    ("tn-blue7"         . "#394b70")
    ("tn-magenta"       . "#bb9af7")
    ("tn-magenta-dark"  . "#9d7cd8")
    ("tn-magenta-hot"   . "#ff007c")

    ;; Comment
    ("tn-comment"       . "#565f89")

    ;; Git / Diff
    ("tn-git-add"       . "#449dab")
    ("tn-git-change"    . "#6183bb")
    ("tn-git-delete"    . "#914c54")
    ("tn-git-ignored"   . "#515670")
    ("tn-diff-add-bg"   . "#1a3a3a")
    ("tn-diff-del-bg"   . "#3a2020")
    ("tn-diff-chg-bg"   . "#1a2a4a")

    ;; Headings
    ("tn-heading1"      . "#89ddff")
    ("tn-heading2"      . "#61bdf2")
    ("tn-heading3"      . "#7aa2f7")
    ("tn-heading4"      . "#6d91de")
    ("tn-heading5"      . "#9aa5ce")
    ("tn-heading6"      . "#747ca1"))
  "The Tokyo Night (storm) color palette.
Medium background variant, same accents as night.")

(defvar tokyo-moon-colors-alist
  '(;; Background shades
    ("tn-bg-darkest"    . "#191b29")
    ("tn-bg-dark"       . "#1e2030")
    ("tn-bg"            . "#222436")
    ("tn-bg-highlight"  . "#2f334d")
    ("tn-bg-line"       . "#282b41")

    ;; Foreground shades
    ("tn-fg"            . "#c8d3f5")
    ("tn-fg-dark"       . "#828bb8")
    ("tn-fg-muted"      . "#9da8d2")
    ("tn-fg-gutter"     . "#3b4261")

    ;; UI elements
    ("tn-line-nr"       . "#3b4261")
    ("tn-line-nr-cur"   . "#828bb8")
    ("tn-indent"        . "#2a2d44")
    ("tn-selection"     . "#515c7e")
    ("tn-dark3"         . "#545c7e")
    ("tn-dark5"         . "#737aa2")
    ("tn-terminal-blk"  . "#444a73")
    ("tn-bracket"       . "#464b6e")
    ("tn-whitespace"    . "#3b4261")

    ;; Core accent colors
    ("tn-red"           . "#ff757f")
    ("tn-red-dark"      . "#c53b53")
    ("tn-orange"        . "#ff966c")
    ("tn-yellow"        . "#ffc777")
    ("tn-green"         . "#c3e88d")
    ("tn-teal"          . "#4fd6be")
    ("tn-teal-dark"     . "#41a6b5")
    ("tn-cyan"          . "#86e1fc")
    ("tn-cyan-bright"   . "#65bcff")
    ("tn-cyan-support"  . "#0db9d7")
    ("tn-cyan-pale"     . "#89ddff")
    ("tn-cyan-ice"      . "#b4f9f8")
    ("tn-blue"          . "#82aaff")
    ("tn-blue-dark"     . "#3e68d7")
    ("tn-blue7"         . "#394b70")
    ("tn-magenta"       . "#c099ff")
    ("tn-magenta-dark"  . "#fca7ea")
    ("tn-magenta-hot"   . "#ff007c")

    ;; Comment
    ("tn-comment"       . "#636da6")

    ;; Git / Diff
    ("tn-git-add"       . "#b8db87")
    ("tn-git-change"    . "#7ca1f2")
    ("tn-git-delete"    . "#e26a75")
    ("tn-git-ignored"   . "#545c7e")
    ("tn-diff-add-bg"   . "#273330")
    ("tn-diff-del-bg"   . "#3a2028")
    ("tn-diff-chg-bg"   . "#1e2a4a")

    ;; Headings
    ("tn-heading1"      . "#89ddff")
    ("tn-heading2"      . "#76c4ff")
    ("tn-heading3"      . "#82aaff")
    ("tn-heading4"      . "#7895d4")
    ("tn-heading5"      . "#828bb8")
    ("tn-heading6"      . "#737aa2"))
  "The Tokyo Night (moon) color palette.
Blue-tinted dark variant with unique accents.")

(defvar tokyo-day-colors-alist
  '(;; Background shades
    ("tn-bg-darkest"    . "#c1c9df")
    ("tn-bg-dark"       . "#d0d5e3")
    ("tn-bg"            . "#e1e2e7")
    ("tn-bg-highlight"  . "#c4c8da")
    ("tn-bg-line"       . "#d8dae3")

    ;; Foreground shades
    ("tn-fg"            . "#3760bf")
    ("tn-fg-dark"       . "#6172b0")
    ("tn-fg-muted"      . "#506399")
    ("tn-fg-gutter"     . "#a8aecb")

    ;; UI elements
    ("tn-line-nr"       . "#a8aecb")
    ("tn-line-nr-cur"   . "#68709a")
    ("tn-indent"        . "#d4d6de")
    ("tn-selection"     . "#b6bfe2")
    ("tn-dark3"         . "#8990b3")
    ("tn-dark5"         . "#68709a")
    ("tn-terminal-blk"  . "#a1a6c5")
    ("tn-bracket"       . "#b8bcd0")
    ("tn-whitespace"    . "#b8bdd0")

    ;; Core accent colors
    ("tn-red"           . "#f52a65")
    ("tn-red-dark"      . "#c64343")
    ("tn-orange"        . "#b15c00")
    ("tn-yellow"        . "#8c6c3e")
    ("tn-green"         . "#587539")
    ("tn-teal"          . "#387068")
    ("tn-teal-dark"     . "#38919f")
    ("tn-cyan"          . "#007197")
    ("tn-cyan-bright"   . "#188092")
    ("tn-cyan-support"  . "#07879d")
    ("tn-cyan-pale"     . "#006a83")
    ("tn-cyan-ice"      . "#2e5857")
    ("tn-blue"          . "#2e7de9")
    ("tn-blue-dark"     . "#7890dd")
    ("tn-blue7"         . "#92a6d5")
    ("tn-magenta"       . "#9854f1")
    ("tn-magenta-dark"  . "#7847bd")
    ("tn-magenta-hot"   . "#d20065")

    ;; Comment
    ("tn-comment"       . "#848cb5")

    ;; Git / Diff
    ("tn-git-add"       . "#4197a4")
    ("tn-git-change"    . "#506d9c")
    ("tn-git-delete"    . "#c47981")
    ("tn-git-ignored"   . "#8990b3")
    ("tn-diff-add-bg"   . "#cee8d0")
    ("tn-diff-del-bg"   . "#ecd0d4")
    ("tn-diff-chg-bg"   . "#d0dcea")

    ;; Headings
    ("tn-heading1"      . "#006a83")
    ("tn-heading2"      . "#1a74b0")
    ("tn-heading3"      . "#2e7de9")
    ("tn-heading4"      . "#5a74b0")
    ("tn-heading5"      . "#6172b0")
    ("tn-heading6"      . "#68709a"))
  "The Tokyo Night (day) color palette.
Light variant.")

;;; Face Application

(defun tokyo--apply-theme (theme-name colors-alist)
  "Apply the Tokyo Night face definitions to THEME-NAME using COLORS-ALIST."
  (let* ((merged (append tokyo-themes-override-colors-alist colors-alist))
         (class '((class color) (min-colors 89))))
    (cl-flet ((c (name) (cdr (assoc name merged))))
      (let ((tn-bg-darkest    (c "tn-bg-darkest"))
            (tn-bg-dark       (c "tn-bg-dark"))
            (tn-bg            (c "tn-bg"))
            (tn-bg-highlight  (c "tn-bg-highlight"))
            (tn-bg-line       (c "tn-bg-line"))
            (tn-fg            (c "tn-fg"))
            (tn-fg-dark       (c "tn-fg-dark"))
            (tn-fg-muted      (c "tn-fg-muted"))
            (tn-fg-gutter     (c "tn-fg-gutter"))
            (tn-line-nr       (c "tn-line-nr"))
            (tn-line-nr-cur   (c "tn-line-nr-cur"))
            (tn-indent        (c "tn-indent"))
            (tn-selection     (c "tn-selection"))
            (tn-dark3         (c "tn-dark3"))
            (tn-dark5         (c "tn-dark5"))
            (tn-terminal-blk  (c "tn-terminal-blk"))
            (tn-bracket       (c "tn-bracket"))
            (tn-whitespace    (c "tn-whitespace"))
            (tn-red           (c "tn-red"))
            (tn-red-dark      (c "tn-red-dark"))
            (tn-orange        (c "tn-orange"))
            (tn-yellow        (c "tn-yellow"))
            (tn-green         (c "tn-green"))
            (tn-teal          (c "tn-teal"))
            (tn-teal-dark     (c "tn-teal-dark"))
            (tn-cyan          (c "tn-cyan"))
            (tn-cyan-bright   (c "tn-cyan-bright"))
            (tn-cyan-support  (c "tn-cyan-support"))
            (tn-cyan-pale     (c "tn-cyan-pale"))
            (tn-cyan-ice      (c "tn-cyan-ice"))
            (tn-blue          (c "tn-blue"))
            (tn-blue-dark     (c "tn-blue-dark"))
            (tn-blue7         (c "tn-blue7"))
            (tn-magenta       (c "tn-magenta"))
            (tn-magenta-dark  (c "tn-magenta-dark"))
            (tn-magenta-hot   (c "tn-magenta-hot"))
            (tn-comment       (c "tn-comment"))
            (tn-git-add       (c "tn-git-add"))
            (tn-git-change    (c "tn-git-change"))
            (tn-git-delete    (c "tn-git-delete"))
            (tn-git-ignored   (c "tn-git-ignored"))
            (tn-diff-add-bg   (c "tn-diff-add-bg"))
            (tn-diff-del-bg   (c "tn-diff-del-bg"))
            (tn-diff-chg-bg   (c "tn-diff-chg-bg"))
            (tn-heading1      (c "tn-heading1"))
            (tn-heading2      (c "tn-heading2"))
            (tn-heading3      (c "tn-heading3"))
            (tn-heading4      (c "tn-heading4"))
            (tn-heading5      (c "tn-heading5"))
            (tn-heading6      (c "tn-heading6")))

        (custom-theme-set-faces
         theme-name

;;;; Built-in faces
;;;;; basic coloring
         `(default ((,class (:foreground ,tn-fg :background ,tn-bg))))
         `(cursor ((,class (:background ,tn-fg))))
         `(fringe ((,class (:background ,tn-bg :foreground ,tn-fg-gutter))))
         `(header-line ((,class (:background ,tn-bg-dark :foreground ,tn-fg-dark))))
         `(highlight ((,class (:background ,tn-bg-highlight))))
         `(success ((,class (:foreground ,tn-green :weight bold))))
         `(warning ((,class (:foreground ,tn-yellow :weight bold))))
         `(error ((,class (:foreground ,tn-red-dark :weight bold))))
         `(link ((,class (:foreground ,tn-teal :underline t))))
         `(link-visited ((,class (:foreground ,tn-magenta :underline t))))
         `(button ((,class (:foreground ,tn-teal :underline t))))
         `(minibuffer-prompt ((,class (:foreground ,tn-cyan))))
         `(escape-glyph ((,class (:foreground ,tn-cyan-pale))))
         `(homoglyph ((,class (:foreground ,tn-cyan-pale))))
         `(tooltip ((,class (:foreground ,tn-fg :background ,tn-bg-dark))))
         `(menu ((,class (:foreground ,tn-fg :background ,tn-bg-dark))))
         `(shadow ((,class (:foreground ,tn-comment))))
         `(region ((,class (:background ,tn-selection :extend t))))
         `(secondary-selection ((,class (:background ,tn-bg-highlight))))
         `(trailing-whitespace ((,class (:background ,tn-red))))
         `(vertical-border ((,class (:foreground ,tn-terminal-blk))))
         `(window-divider ((,class (:foreground ,tn-terminal-blk))))
         `(window-divider-first-pixel ((,class (:foreground ,tn-terminal-blk))))
         `(window-divider-last-pixel ((,class (:foreground ,tn-terminal-blk))))
         `(widget-field ((,class (:background ,tn-bg-highlight :extend t))))

;;;;; fill-column-indicator
         `(fill-column-indicator ((,class (:foreground ,tn-indent :weight normal))))

;;;;; mode-line
         `(mode-line ((,class (:foreground ,tn-fg :background ,tn-bg-dark
                                           :box (:line-width -1 :color ,tn-terminal-blk)))))
         `(mode-line-active ((,class (:inherit mode-line))))
         `(mode-line-inactive ((,class (:foreground ,tn-dark5 :background ,tn-bg-darkest
                                                    :box (:line-width -1 :color ,tn-bg-dark)))))
         `(mode-line-buffer-id ((,class (:foreground ,tn-blue :weight bold))))
         `(mode-line-emphasis ((,class (:foreground ,tn-fg :weight bold))))
         `(mode-line-highlight ((,class (:foreground ,tn-magenta))))

;;;;; ansi-colors
         `(ansi-color-black ((,class (:foreground ,tn-line-nr :background ,tn-line-nr))))
         `(ansi-color-red ((,class (:foreground ,tn-red :background ,tn-red))))
         `(ansi-color-green ((,class (:foreground ,tn-teal :background ,tn-teal))))
         `(ansi-color-yellow ((,class (:foreground ,tn-yellow :background ,tn-yellow))))
         `(ansi-color-blue ((,class (:foreground ,tn-blue :background ,tn-blue))))
         `(ansi-color-magenta ((,class (:foreground ,tn-magenta :background ,tn-magenta))))
         `(ansi-color-cyan ((,class (:foreground ,tn-cyan :background ,tn-cyan))))
         `(ansi-color-white ((,class (:foreground ,tn-line-nr-cur :background ,tn-line-nr-cur))))
         `(ansi-color-bright-black ((,class (:foreground ,tn-terminal-blk :background ,tn-terminal-blk))))
         `(ansi-color-bright-red ((,class (:foreground ,tn-red :background ,tn-red))))
         `(ansi-color-bright-green ((,class (:foreground ,tn-teal :background ,tn-teal))))
         `(ansi-color-bright-yellow ((,class (:foreground ,tn-yellow :background ,tn-yellow))))
         `(ansi-color-bright-blue ((,class (:foreground ,tn-blue :background ,tn-blue))))
         `(ansi-color-bright-magenta ((,class (:foreground ,tn-magenta :background ,tn-magenta))))
         `(ansi-color-bright-cyan ((,class (:foreground ,tn-cyan :background ,tn-cyan))))
         `(ansi-color-bright-white ((,class (:foreground ,tn-fg-dark :background ,tn-fg-dark))))

;;;;; font-lock
         `(font-lock-builtin-face ((,class (:foreground ,tn-cyan-bright))))
         `(font-lock-comment-face ((,class (:foreground ,tn-comment :slant italic))))
         `(font-lock-comment-delimiter-face ((,class (:foreground ,tn-comment :slant italic))))
         `(font-lock-constant-face ((,class (:foreground ,tn-orange))))
         `(font-lock-doc-face ((,class (:foreground ,tn-comment))))
         `(font-lock-doc-markup-face ((,class (:foreground ,tn-dark5))))
         `(font-lock-function-name-face ((,class (:foreground ,tn-blue))))
         `(font-lock-function-call-face ((,class (:foreground ,tn-blue))))
         `(font-lock-keyword-face ((,class (:foreground ,tn-magenta :slant italic))))
         `(font-lock-negation-char-face ((,class (:foreground ,tn-cyan-pale))))
         `(font-lock-number-face ((,class (:foreground ,tn-orange))))
         `(font-lock-operator-face ((,class (:foreground ,tn-cyan-pale))))
         `(font-lock-preprocessor-face ((,class (:foreground ,tn-teal))))
         `(font-lock-regexp-grouping-construct ((,class (:foreground ,tn-cyan-ice :weight bold))))
         `(font-lock-regexp-grouping-backslash ((,class (:foreground ,tn-cyan-ice :weight bold))))
         `(font-lock-string-face ((,class (:foreground ,tn-green))))
         `(font-lock-type-face ((,class (:foreground ,tn-cyan-bright))))
         `(font-lock-variable-name-face ((,class (:foreground ,tn-fg))))
         `(font-lock-variable-use-face ((,class (:foreground ,tn-fg))))
         `(font-lock-warning-face ((,class (:foreground ,tn-yellow :weight bold))))
         `(font-lock-property-name-face ((,class (:foreground ,tn-teal))))
         `(font-lock-property-use-face ((,class (:foreground ,tn-teal))))
         `(font-lock-bracket-face ((,class (:foreground ,tn-fg-dark))))
         `(font-lock-delimiter-face ((,class (:foreground ,tn-fg-dark))))
         `(font-lock-escape-face ((,class (:foreground ,tn-cyan-pale))))
         `(font-lock-misc-punctuation-face ((,class (:foreground ,tn-cyan-pale))))

;;;;; line numbers
         `(line-number ((,class (:foreground ,tn-line-nr :background ,tn-bg))))
         `(line-number-current-line ((,class (:foreground ,tn-line-nr-cur :background ,tn-bg :weight bold))))
         `(line-number-major-tick ((,class (:foreground ,tn-dark5 :background ,tn-bg))))
         `(line-number-minor-tick ((,class (:foreground ,tn-fg-gutter :background ,tn-bg))))

;;;;; isearch / replace
         `(isearch ((,class (:foreground ,tn-bg :background ,tn-magenta-hot :weight bold))))
         `(isearch-fail ((,class (:foreground ,tn-red-dark :background ,tn-bg-highlight))))
         `(isearch-group-1 ((,class (:foreground ,tn-bg :background ,tn-blue))))
         `(isearch-group-2 ((,class (:foreground ,tn-bg :background ,tn-teal))))
         `(lazy-highlight ((,class (:foreground ,tn-fg :background ,tn-blue-dark))))
         `(match ((,class (:foreground ,tn-green :background ,tn-bg :weight bold))))
         `(query-replace ((,class (:foreground ,tn-bg :background ,tn-orange :weight bold))))

;;;;; show-paren
         `(show-paren-match ((,class (:foreground ,tn-cyan-pale :background ,tn-bracket :weight bold))))
         `(show-paren-match-expression ((,class (:background ,tn-bg-highlight))))
         `(show-paren-mismatch ((,class (:foreground ,tn-red :background ,tn-bg :weight bold :underline t))))

;;;;; completions
         `(completions-annotations ((,class (:foreground ,tn-comment))))
         `(completions-common-part ((,class (:foreground ,tn-blue :weight bold))))
         `(completions-first-difference ((,class (:foreground ,tn-orange))))
         `(completions-highlight ((,class (:background ,tn-bg-highlight))))
         `(completions-group-title ((,class (:foreground ,tn-magenta :weight bold :slant italic))))
         `(completions-group-separator ((,class (:foreground ,tn-comment :strike-through t))))

;;;;; compilation
         `(compilation-error ((,class (:foreground ,tn-red-dark :weight bold))))
         `(compilation-warning ((,class (:foreground ,tn-yellow :weight bold))))
         `(compilation-info ((,class (:foreground ,tn-teal))))
         `(compilation-mode-line-exit ((,class (:foreground ,tn-green :weight bold))))
         `(compilation-mode-line-fail ((,class (:foreground ,tn-red :weight bold))))
         `(compilation-mode-line-run ((,class (:foreground ,tn-blue :weight bold))))
         `(compilation-line-number ((,class (:foreground ,tn-dark5))))
         `(compilation-column-number ((,class (:foreground ,tn-dark5))))

;;;;; customize
         `(custom-variable-tag ((,class (:foreground ,tn-blue :weight bold))))
         `(custom-group-tag ((,class (:foreground ,tn-blue :weight bold :height 1.2))))
         `(custom-group-tag-1 ((,class (:foreground ,tn-magenta :weight bold :height 1.2))))
         `(custom-state ((,class (:foreground ,tn-green))))
         `(custom-button ((,class (:foreground ,tn-fg :background ,tn-bg-highlight
                                               :box (:line-width 2 :color ,tn-terminal-blk :style released-button)))))
         `(custom-button-mouse ((,class (:foreground ,tn-fg :background ,tn-selection
                                                     :box (:line-width 2 :color ,tn-terminal-blk :style released-button)))))
         `(custom-button-pressed ((,class (:foreground ,tn-fg :background ,tn-bg-highlight
                                                       :box (:line-width 2 :color ,tn-terminal-blk :style pressed-button)))))

;;;;; diff
         `(diff-added ((,class (:foreground ,tn-git-add :background ,tn-diff-add-bg :extend t))))
         `(diff-removed ((,class (:foreground ,tn-git-delete :background ,tn-diff-del-bg :extend t))))
         `(diff-changed ((,class (:foreground ,tn-git-change :background ,tn-diff-chg-bg :extend t))))
         `(diff-refine-added ((,class (:foreground ,tn-green :background ,tn-diff-add-bg :weight bold))))
         `(diff-refine-removed ((,class (:foreground ,tn-red :background ,tn-diff-del-bg :weight bold))))
         `(diff-refine-changed ((,class (:foreground ,tn-blue :background ,tn-diff-chg-bg :weight bold))))
         `(diff-header ((,class (:foreground ,tn-fg-dark :background ,tn-bg-dark :extend t))))
         `(diff-file-header ((,class (:foreground ,tn-blue :background ,tn-bg-dark :weight bold :extend t))))
         `(diff-hunk-header ((,class (:foreground ,tn-magenta :background ,tn-bg-dark :extend t))))
         `(diff-indicator-added ((,class (:foreground ,tn-git-add))))
         `(diff-indicator-removed ((,class (:foreground ,tn-git-delete))))
         `(diff-indicator-changed ((,class (:foreground ,tn-git-change))))
         `(diff-nonexistent ((,class (:foreground ,tn-comment))))

;;;;; dired
         `(dired-directory ((,class (:foreground ,tn-blue :weight bold))))
         `(dired-flagged ((,class (:foreground ,tn-red))))
         `(dired-header ((,class (:foreground ,tn-magenta :weight bold))))
         `(dired-ignored ((,class (:foreground ,tn-comment))))
         `(dired-mark ((,class (:foreground ,tn-orange :weight bold))))
         `(dired-marked ((,class (:foreground ,tn-magenta :weight bold))))
         `(dired-perm-write ((,class (:foreground ,tn-fg-dark))))
         `(dired-symlink ((,class (:foreground ,tn-cyan))))
         `(dired-warning ((,class (:foreground ,tn-yellow :weight bold))))
         `(dired-broken-symlink ((,class (:foreground ,tn-red :weight bold))))

;;;;; ediff
         `(ediff-current-diff-A ((,class (:background ,tn-diff-del-bg :extend t))))
         `(ediff-current-diff-B ((,class (:background ,tn-diff-add-bg :extend t))))
         `(ediff-current-diff-C ((,class (:background ,tn-diff-chg-bg :extend t))))
         `(ediff-fine-diff-A ((,class (:foreground ,tn-red :background ,tn-diff-del-bg :weight bold))))
         `(ediff-fine-diff-B ((,class (:foreground ,tn-green :background ,tn-diff-add-bg :weight bold))))
         `(ediff-fine-diff-C ((,class (:foreground ,tn-blue :background ,tn-diff-chg-bg :weight bold))))
         `(ediff-even-diff-A ((,class (:background ,tn-bg-highlight))))
         `(ediff-even-diff-B ((,class (:background ,tn-bg-highlight))))
         `(ediff-even-diff-C ((,class (:background ,tn-bg-highlight))))
         `(ediff-odd-diff-A ((,class (:background ,tn-bg-highlight))))
         `(ediff-odd-diff-B ((,class (:background ,tn-bg-highlight))))
         `(ediff-odd-diff-C ((,class (:background ,tn-bg-highlight))))

;;;;; eshell
         `(eshell-prompt ((,class (:foreground ,tn-blue :weight bold))))
         `(eshell-ls-archive ((,class (:foreground ,tn-magenta))))
         `(eshell-ls-backup ((,class (:foreground ,tn-comment))))
         `(eshell-ls-clutter ((,class (:foreground ,tn-comment))))
         `(eshell-ls-directory ((,class (:foreground ,tn-blue :weight bold))))
         `(eshell-ls-executable ((,class (:foreground ,tn-green))))
         `(eshell-ls-missing ((,class (:foreground ,tn-red))))
         `(eshell-ls-product ((,class (:foreground ,tn-fg-dark))))
         `(eshell-ls-readonly ((,class (:foreground ,tn-dark5))))
         `(eshell-ls-special ((,class (:foreground ,tn-orange :weight bold))))
         `(eshell-ls-symlink ((,class (:foreground ,tn-cyan))))
         `(eshell-ls-unreadable ((,class (:foreground ,tn-comment))))

;;;;; flymake
         `(flymake-error ((,class (:underline (:style wave :color ,tn-red-dark)))))
         `(flymake-warning ((,class (:underline (:style wave :color ,tn-yellow)))))
         `(flymake-note ((,class (:underline (:style wave :color ,tn-teal)))))

;;;;; flyspell
         `(flyspell-duplicate ((,class (:underline (:style wave :color ,tn-yellow)))))
         `(flyspell-incorrect ((,class (:underline (:style wave :color ,tn-red-dark)))))

;;;;; gnus
         `(gnus-group-mail-1 ((,class (:foreground ,tn-blue :weight bold))))
         `(gnus-group-mail-1-empty ((,class (:foreground ,tn-blue))))
         `(gnus-group-mail-2 ((,class (:foreground ,tn-cyan :weight bold))))
         `(gnus-group-mail-2-empty ((,class (:foreground ,tn-cyan))))
         `(gnus-group-mail-3 ((,class (:foreground ,tn-magenta :weight bold))))
         `(gnus-group-mail-3-empty ((,class (:foreground ,tn-magenta))))
         `(gnus-group-mail-low ((,class (:foreground ,tn-comment :weight bold))))
         `(gnus-group-mail-low-empty ((,class (:foreground ,tn-comment))))
         `(gnus-group-news-1 ((,class (:foreground ,tn-blue :weight bold))))
         `(gnus-group-news-1-empty ((,class (:foreground ,tn-blue))))
         `(gnus-group-news-2 ((,class (:foreground ,tn-cyan :weight bold))))
         `(gnus-group-news-2-empty ((,class (:foreground ,tn-cyan))))
         `(gnus-group-news-low ((,class (:foreground ,tn-comment :weight bold))))
         `(gnus-group-news-low-empty ((,class (:foreground ,tn-comment))))
         `(gnus-header-content ((,class (:foreground ,tn-fg-dark))))
         `(gnus-header-from ((,class (:foreground ,tn-blue :weight bold))))
         `(gnus-header-name ((,class (:foreground ,tn-magenta))))
         `(gnus-header-newsgroups ((,class (:foreground ,tn-teal :weight bold))))
         `(gnus-header-subject ((,class (:foreground ,tn-fg :weight bold))))
         `(gnus-summary-cancelled ((,class (:foreground ,tn-red :background ,tn-bg))))
         `(gnus-summary-normal-ancient ((,class (:foreground ,tn-comment))))
         `(gnus-summary-normal-read ((,class (:foreground ,tn-dark5))))
         `(gnus-summary-normal-ticked ((,class (:foreground ,tn-fg-dark :slant italic))))
         `(gnus-summary-normal-unread ((,class (:foreground ,tn-fg :weight bold))))
         `(gnus-summary-selected ((,class (:foreground ,tn-blue :weight bold :underline t))))

;;;;; grep
         `(grep-context-face ((,class (:foreground ,tn-fg-dark))))
         `(grep-error-face ((,class (:foreground ,tn-red-dark :weight bold :underline t))))
         `(grep-hit-face ((,class (:foreground ,tn-blue))))
         `(grep-match-face ((,class (:foreground ,tn-orange :weight bold))))

;;;;; hi-lock
         `(hi-blue ((,class (:foreground ,tn-bg :background ,tn-blue))))
         `(hi-green ((,class (:foreground ,tn-bg :background ,tn-green))))
         `(hi-pink ((,class (:foreground ,tn-bg :background ,tn-red))))
         `(hi-yellow ((,class (:foreground ,tn-bg :background ,tn-yellow))))
         `(hi-blue-b ((,class (:foreground ,tn-blue :weight bold))))
         `(hi-green-b ((,class (:foreground ,tn-green :weight bold))))
         `(hi-red-b ((,class (:foreground ,tn-red :weight bold))))

;;;;; hl-line
         `(hl-line ((,class (:background ,tn-bg-line :extend t))))

;;;;; hl-todo
         `(hl-todo ((,class (:foreground ,tn-magenta-hot :weight bold))))

;;;;; icomplete
         `(icomplete-first-match ((,class (:foreground ,tn-green :weight bold))))
         `(icomplete-selected-match ((,class (:background ,tn-bg-highlight))))

;;;;; ido
         `(ido-first-match ((,class (:foreground ,tn-green :weight bold))))
         `(ido-only-match ((,class (:foreground ,tn-teal :weight bold))))
         `(ido-subdir ((,class (:foreground ,tn-blue))))
         `(ido-incomplete-regexp ((,class (:foreground ,tn-red-dark))))
         `(ido-indicator ((,class (:foreground ,tn-yellow :background ,tn-bg))))
         `(ido-virtual ((,class (:foreground ,tn-comment))))

;;;;; info
         `(Info-quoted ((,class (:foreground ,tn-orange :inherit fixed-pitch-serif))))
         `(info-header-node ((,class (:foreground ,tn-blue :weight bold))))
         `(info-header-xref ((,class (:foreground ,tn-teal))))
         `(info-menu-header ((,class (:foreground ,tn-fg :weight bold))))
         `(info-menu-star ((,class (:foreground ,tn-red))))
         `(info-node ((,class (:foreground ,tn-blue :weight bold))))
         `(info-title-1 ((,class (:foreground ,tn-heading1 :weight bold :height 1.3))))
         `(info-title-2 ((,class (:foreground ,tn-heading2 :weight bold :height 1.2))))
         `(info-title-3 ((,class (:foreground ,tn-heading3 :weight bold :height 1.1))))
         `(info-title-4 ((,class (:foreground ,tn-heading4 :weight bold))))
         `(info-xref ((,class (:foreground ,tn-teal :underline t))))
         `(info-xref-visited ((,class (:foreground ,tn-magenta :underline t))))

;;;;; message (email composition)
         `(message-cited-text-1 ((,class (:foreground ,tn-teal))))
         `(message-cited-text-2 ((,class (:foreground ,tn-green))))
         `(message-cited-text-3 ((,class (:foreground ,tn-comment))))
         `(message-cited-text-4 ((,class (:foreground ,tn-dark5))))
         `(message-header-cc ((,class (:foreground ,tn-blue))))
         `(message-header-name ((,class (:foreground ,tn-magenta))))
         `(message-header-newsgroups ((,class (:foreground ,tn-teal :weight bold))))
         `(message-header-other ((,class (:foreground ,tn-fg-dark))))
         `(message-header-subject ((,class (:foreground ,tn-fg :weight bold))))
         `(message-header-to ((,class (:foreground ,tn-blue :weight bold))))
         `(message-header-xheader ((,class (:foreground ,tn-dark5))))
         `(message-mml ((,class (:foreground ,tn-green))))
         `(message-separator ((,class (:foreground ,tn-comment))))

;;;;; org-mode
         `(org-archived ((,class (:foreground ,tn-comment))))
         `(org-block ((,class (:background ,tn-bg-dark :extend t))))
         `(org-block-begin-line ((,class (:foreground ,tn-comment :background ,tn-bg-dark :extend t :slant italic))))
         `(org-block-end-line ((,class (:inherit org-block-begin-line))))
         `(org-checkbox ((,class (:foreground ,tn-blue :weight bold))))
         `(org-checkbox-statistics-done ((,class (:foreground ,tn-green))))
         `(org-checkbox-statistics-todo ((,class (:foreground ,tn-orange))))
         `(org-code ((,class (:foreground ,tn-teal))))
         `(org-date ((,class (:foreground ,tn-cyan :underline t))))
         `(org-document-info ((,class (:foreground ,tn-fg-dark))))
         `(org-document-info-keyword ((,class (:foreground ,tn-comment))))
         `(org-document-title ((,class (:foreground ,tn-fg :weight bold :height 1.4))))
         `(org-done ((,class (:foreground ,tn-green :weight bold))))
         `(org-drawer ((,class (:foreground ,tn-comment))))
         `(org-ellipsis ((,class (:foreground ,tn-comment :underline nil))))
         `(org-footnote ((,class (:foreground ,tn-teal))))
         `(org-formula ((,class (:foreground ,tn-orange))))
         `(org-headline-done ((,class (:foreground ,tn-comment))))
         `(org-hide ((,class (:foreground ,tn-bg))))
         `(org-level-1 ((,class (:foreground ,tn-heading1 :weight bold))))
         `(org-level-2 ((,class (:foreground ,tn-heading2 :weight bold))))
         `(org-level-3 ((,class (:foreground ,tn-heading3 :weight bold))))
         `(org-level-4 ((,class (:foreground ,tn-heading4 :weight bold))))
         `(org-level-5 ((,class (:foreground ,tn-heading5 :weight bold))))
         `(org-level-6 ((,class (:foreground ,tn-heading6 :weight bold))))
         `(org-level-7 ((,class (:foreground ,tn-fg-dark :weight bold))))
         `(org-level-8 ((,class (:foreground ,tn-dark5 :weight bold))))
         `(org-link ((,class (:foreground ,tn-teal :underline t))))
         `(org-meta-line ((,class (:foreground ,tn-comment))))
         `(org-priority ((,class (:foreground ,tn-orange))))
         `(org-property-value ((,class (:foreground ,tn-fg-dark))))
         `(org-quote ((,class (:foreground ,tn-fg-muted :slant italic :extend t))))
         `(org-scheduled ((,class (:foreground ,tn-green))))
         `(org-scheduled-previously ((,class (:foreground ,tn-orange))))
         `(org-scheduled-today ((,class (:foreground ,tn-green))))
         `(org-special-keyword ((,class (:foreground ,tn-comment))))
         `(org-table ((,class (:foreground ,tn-fg-muted))))
         `(org-tag ((,class (:foreground ,tn-comment :weight normal))))
         `(org-target ((,class (:underline t))))
         `(org-time-grid ((,class (:foreground ,tn-dark5))))
         `(org-todo ((,class (:foreground ,tn-orange :weight bold))))
         `(org-upcoming-deadline ((,class (:foreground ,tn-red))))
         `(org-verbatim ((,class (:foreground ,tn-green))))
         `(org-verse ((,class (:inherit org-quote))))
         `(org-warning ((,class (:foreground ,tn-yellow :weight bold))))
         `(org-agenda-date ((,class (:foreground ,tn-blue))))
         `(org-agenda-date-today ((,class (:foreground ,tn-blue :weight bold :slant italic))))
         `(org-agenda-date-weekend ((,class (:foreground ,tn-comment))))
         `(org-agenda-date-weekend-today ((,class (:foreground ,tn-comment :weight bold))))
         `(org-agenda-done ((,class (:foreground ,tn-green))))
         `(org-agenda-structure ((,class (:foreground ,tn-magenta :weight bold))))
         `(org-agenda-current-time ((,class (:foreground ,tn-cyan))))

;;;;; outline
         `(outline-1 ((,class (:foreground ,tn-heading1 :weight bold))))
         `(outline-2 ((,class (:foreground ,tn-heading2 :weight bold))))
         `(outline-3 ((,class (:foreground ,tn-heading3 :weight bold))))
         `(outline-4 ((,class (:foreground ,tn-heading4 :weight bold))))
         `(outline-5 ((,class (:foreground ,tn-heading5 :weight bold))))
         `(outline-6 ((,class (:foreground ,tn-heading6 :weight bold))))
         `(outline-7 ((,class (:foreground ,tn-fg-dark :weight bold))))
         `(outline-8 ((,class (:foreground ,tn-dark5 :weight bold))))

;;;;; re-builder
         `(reb-match-0 ((,class (:foreground ,tn-bg :background ,tn-blue))))
         `(reb-match-1 ((,class (:foreground ,tn-bg :background ,tn-teal))))
         `(reb-match-2 ((,class (:foreground ,tn-bg :background ,tn-magenta))))
         `(reb-match-3 ((,class (:foreground ,tn-bg :background ,tn-orange))))

;;;;; ruler-mode
         `(ruler-mode-default ((,class (:foreground ,tn-comment :background ,tn-bg-dark))))
         `(ruler-mode-column-number ((,class (:foreground ,tn-fg-dark))))
         `(ruler-mode-current-column ((,class (:foreground ,tn-yellow :weight bold))))
         `(ruler-mode-fill-column ((,class (:foreground ,tn-red))))

;;;;; sh-mode
         `(sh-heredoc ((,class (:foreground ,tn-green :slant italic))))
         `(sh-quoted-exec ((,class (:foreground ,tn-orange))))

;;;;; shr (eww/elfeed HTML rendering)
         `(shr-h1 ((,class (:foreground ,tn-heading1 :weight bold :height 1.3))))
         `(shr-h2 ((,class (:foreground ,tn-heading2 :weight bold :height 1.2))))
         `(shr-h3 ((,class (:foreground ,tn-heading3 :weight bold :height 1.1))))
         `(shr-h4 ((,class (:foreground ,tn-heading4 :weight bold))))
         `(shr-h5 ((,class (:foreground ,tn-heading5 :weight bold))))
         `(shr-h6 ((,class (:foreground ,tn-heading6 :weight bold))))
         `(shr-link ((,class (:foreground ,tn-teal :underline t))))
         `(shr-selected-link ((,class (:foreground ,tn-orange :underline t))))
         `(shr-code ((,class (:foreground ,tn-teal :background ,tn-bg-dark))))
         `(shr-mark ((,class (:foreground ,tn-bg :background ,tn-yellow))))

;;;;; tab-bar / tab-line
         `(tab-bar ((,class (:foreground ,tn-fg-dark :background ,tn-bg-darkest))))
         `(tab-bar-tab ((,class (:foreground ,tn-fg :background ,tn-bg :weight bold))))
         `(tab-bar-tab-inactive ((,class (:foreground ,tn-dark5 :background ,tn-bg-dark))))
         `(tab-bar-tab-ungrouped ((,class (:foreground ,tn-comment :background ,tn-bg-dark))))
         `(tab-bar-tab-group-current ((,class (:foreground ,tn-blue :background ,tn-bg :weight bold))))
         `(tab-bar-tab-group-inactive ((,class (:foreground ,tn-dark5 :background ,tn-bg-dark))))
         `(tab-line ((,class (:foreground ,tn-fg-dark :background ,tn-bg-darkest))))
         `(tab-line-tab ((,class (:foreground ,tn-fg :background ,tn-bg :weight bold))))
         `(tab-line-tab-current ((,class (:foreground ,tn-fg :background ,tn-bg :weight bold))))
         `(tab-line-tab-inactive ((,class (:foreground ,tn-dark5 :background ,tn-bg-dark))))
         `(tab-line-highlight ((,class (:background ,tn-bg-highlight))))

;;;;; term / ansi-term / vterm
         `(term ((,class (:foreground ,tn-fg :background ,tn-bg))))
         `(term-color-black ((,class (:foreground ,tn-line-nr :background ,tn-line-nr))))
         `(term-color-red ((,class (:foreground ,tn-red :background ,tn-red))))
         `(term-color-green ((,class (:foreground ,tn-teal :background ,tn-teal))))
         `(term-color-yellow ((,class (:foreground ,tn-yellow :background ,tn-yellow))))
         `(term-color-blue ((,class (:foreground ,tn-blue :background ,tn-blue))))
         `(term-color-magenta ((,class (:foreground ,tn-magenta :background ,tn-magenta))))
         `(term-color-cyan ((,class (:foreground ,tn-cyan :background ,tn-cyan))))
         `(term-color-white ((,class (:foreground ,tn-line-nr-cur :background ,tn-line-nr-cur))))

;;;;; whitespace-mode
         `(whitespace-empty ((,class (:foreground ,tn-red :background ,tn-bg))))
         `(whitespace-hspace ((,class (:foreground ,tn-whitespace))))
         `(whitespace-indentation ((,class (:foreground ,tn-whitespace))))
         `(whitespace-line ((,class (:foreground ,tn-red :background ,tn-bg-highlight))))
         `(whitespace-newline ((,class (:foreground ,tn-whitespace))))
         `(whitespace-space ((,class (:foreground ,tn-whitespace))))
         `(whitespace-space-after-tab ((,class (:foreground ,tn-whitespace))))
         `(whitespace-space-before-tab ((,class (:foreground ,tn-orange))))
         `(whitespace-tab ((,class (:foreground ,tn-whitespace))))
         `(whitespace-trailing ((,class (:foreground ,tn-red :background ,tn-diff-del-bg))))
         `(whitespace-big-indent ((,class (:foreground ,tn-orange :background ,tn-bg-highlight))))

;;;;; woman
         `(woman-bold ((,class (:foreground ,tn-blue :weight bold))))
         `(woman-italic ((,class (:foreground ,tn-magenta :slant italic))))

;;;;; xref
         `(xref-file-header ((,class (:foreground ,tn-blue :weight bold))))
         `(xref-line-number ((,class (:foreground ,tn-dark5))))
         `(xref-match ((,class (:foreground ,tn-orange :weight bold))))

;;;; Built-in packages
;;;;; bookmark
         `(bookmark-face ((,class (:foreground ,tn-yellow :background ,tn-bg))))

;;;;; calendar
         `(calendar-today ((,class (:foreground ,tn-blue :weight bold :underline t))))
         `(calendar-weekend-header ((,class (:foreground ,tn-red))))
         `(calendar-weekday-header ((,class (:foreground ,tn-teal))))
         `(calendar-month-header ((,class (:foreground ,tn-magenta :weight bold))))
         `(holiday ((,class (:foreground ,tn-orange))))
         `(diary ((,class (:foreground ,tn-yellow))))

;;;;; eglot
         `(eglot-highlight-symbol-face ((,class (:background ,tn-bg-highlight :weight bold))))
         `(eglot-diagnostic-tag-unnecessary-face ((,class (:foreground ,tn-comment :underline (:style wave :color ,tn-dark5)))))
         `(eglot-diagnostic-tag-deprecated-face ((,class (:foreground ,tn-comment :strike-through ,tn-dark5))))
         `(eglot-inlay-hint-face ((,class (:foreground ,tn-dark5 :height 0.9))))

;;;;; eldoc
         `(eldoc-highlight-function-argument ((,class (:foreground ,tn-yellow :weight bold))))

;;;;; epa (EasyPG)
         `(epa-field-body ((,class (:foreground ,tn-fg-dark :slant italic))))
         `(epa-field-name ((,class (:foreground ,tn-blue :weight bold))))
         `(epa-mark ((,class (:foreground ,tn-orange :weight bold))))
         `(epa-string ((,class (:foreground ,tn-green))))
         `(epa-validity-disabled ((,class (:foreground ,tn-red-dark :slant italic))))
         `(epa-validity-high ((,class (:foreground ,tn-green :weight bold))))
         `(epa-validity-low ((,class (:foreground ,tn-comment))))
         `(epa-validity-medium ((,class (:foreground ,tn-yellow))))

;;;;; eww
         `(eww-invalid-certificate ((,class (:foreground ,tn-red-dark :weight bold))))
         `(eww-valid-certificate ((,class (:foreground ,tn-green :weight bold))))
         `(eww-form-text ((,class (:foreground ,tn-fg :background ,tn-bg-highlight
                                               :box (:line-width -1 :color ,tn-terminal-blk)))))
         `(eww-form-textarea ((,class (:foreground ,tn-fg :background ,tn-bg-highlight))))
         `(eww-form-checkbox ((,class (:foreground ,tn-blue :weight bold))))
         `(eww-form-select ((,class (:foreground ,tn-fg :background ,tn-bg-highlight
                                                 :box (:line-width -1 :color ,tn-terminal-blk)))))
         `(eww-form-submit ((,class (:foreground ,tn-fg :background ,tn-terminal-blk
                                                 :box (:line-width -1 :color ,tn-dark5)))))

;;;;; man
         `(Man-overstrike ((,class (:foreground ,tn-blue :weight bold))))
         `(Man-underline ((,class (:foreground ,tn-teal :underline t))))
         `(Man-reverse ((,class (:foreground ,tn-bg :background ,tn-fg))))

;;;;; proced
         `(proced-mark ((,class (:foreground ,tn-orange :weight bold))))
         `(proced-marked ((,class (:foreground ,tn-magenta :weight bold))))
         `(proced-sort-header ((,class (:foreground ,tn-blue :weight bold :underline t))))

;;;;; pulse
         `(pulse-highlight-start-face ((,class (:background ,tn-blue-dark))))

;;;;; speedbar
         `(speedbar-button-face ((,class (:foreground ,tn-green))))
         `(speedbar-directory-face ((,class (:foreground ,tn-blue :weight bold))))
         `(speedbar-file-face ((,class (:foreground ,tn-fg))))
         `(speedbar-highlight-face ((,class (:background ,tn-bg-highlight))))
         `(speedbar-selected-face ((,class (:foreground ,tn-orange :weight bold))))
         `(speedbar-separator-face ((,class (:foreground ,tn-comment :background ,tn-bg-dark))))
         `(speedbar-tag-face ((,class (:foreground ,tn-teal))))

;;;;; vc
         `(vc-state-base ((,class (:foreground ,tn-green))))
         `(vc-conflict-state ((,class (:foreground ,tn-red :weight bold))))
         `(vc-edited-state ((,class (:foreground ,tn-yellow))))
         `(vc-locally-added-state ((,class (:foreground ,tn-git-add))))
         `(vc-locked-state ((,class (:foreground ,tn-orange :weight bold))))
         `(vc-missing-state ((,class (:foreground ,tn-red-dark))))
         `(vc-needs-update-state ((,class (:foreground ,tn-orange))))
         `(vc-removed-state ((,class (:foreground ,tn-red))))
         `(vc-up-to-date-state ((,class (:foreground ,tn-green))))

;;;;; diff-hl
         `(diff-hl-change ((,class (:foreground ,tn-git-change :background ,tn-blue7))))
         `(diff-hl-delete ((,class (:foreground ,tn-git-delete :background ,tn-diff-del-bg))))
         `(diff-hl-insert ((,class (:foreground ,tn-git-add :background ,tn-diff-add-bg))))

;;;;; smerge
         `(smerge-base ((,class (:background ,tn-diff-chg-bg :extend t))))
         `(smerge-markers ((,class (:foreground ,tn-comment :background ,tn-bg-dark :extend t))))
         `(smerge-upper ((,class (:background ,tn-diff-del-bg :extend t))))
         `(smerge-lower ((,class (:background ,tn-diff-add-bg :extend t))))
         `(smerge-refined-added ((,class (:foreground ,tn-green :background ,tn-diff-add-bg :weight bold))))
         `(smerge-refined-removed ((,class (:foreground ,tn-red :background ,tn-diff-del-bg :weight bold))))
         `(smerge-refined-changed ((,class (:foreground ,tn-blue :background ,tn-diff-chg-bg :weight bold))))

         )

        (custom-theme-set-variables
         theme-name

;;;;; ansi-color
         `(ansi-color-names-vector
           [,tn-line-nr ,tn-red ,tn-teal ,tn-yellow
            ,tn-blue ,tn-magenta ,tn-cyan ,tn-line-nr-cur])

;;;;; pdf-view
         `(pdf-view-midnight-colors '(,tn-fg . ,tn-bg))

         )))))

(provide 'tokyo-themes)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; tokyo-themes.el ends here
