# Tokyo Themes for Emacs

[![CI](https://github.com/bbatsov/tokyo-night-emacs/actions/workflows/ci.yml/badge.svg)](https://github.com/bbatsov/tokyo-night-emacs/actions/workflows/ci.yml)
[![MELPA](https://melpa.org/packages/tokyo-night-badge.svg)](https://melpa.org/#/tokyo-night)

A faithful Emacs port of [folke's Tokyo Night](https://github.com/folke/tokyonight.nvim) color theme.

![Tokyo Storm](screenshots/tokyo-storm.png)

All four variants are included:

| Theme | Background | Description |
|-------|-----------|-------------|
| `tokyo-night` | `#1a1b26` | The darkest variant (default) |
| `tokyo-night-storm` | `#24283b` | Medium dark |
| `tokyo-night-moon` | `#222436` | Blue-tinted dark, unique accents |
| `tokyo-night-day` | `#e1e2e7` | Light variant |

## Installation

### MELPA

Tokyo Night is available on [MELPA](https://melpa.org/#/tokyo-night). Assuming
you've [configured MELPA](https://melpa.org/#/getting-started) as a package
source:

```
M-x package-install RET tokyo-night RET
```

Then load any variant:

```emacs-lisp
(load-theme 'tokyo-night t)
```

Or with `use-package`:

```emacs-lisp
(use-package tokyo-night
  :ensure t
  :config
  (load-theme 'tokyo-night t))
```

### package-vc (Emacs 29+)

```emacs-lisp
(package-vc-install "https://github.com/bbatsov/tokyo-night-emacs")
(load-theme 'tokyo-night t)
```

### use-package (Emacs 30+)

```emacs-lisp
(use-package tokyo-night
  :vc (:url "https://github.com/bbatsov/tokyo-night-emacs" :rev :newest)
  :config
  (load-theme 'tokyo-night t))
```

### Manual

Drop all `.el` files somewhere on your `load-path` and `custom-theme-load-path`, then:

```emacs-lisp
(load-theme 'tokyo-night t)
```

Replace `tokyo-night` with `tokyo-night-storm`, `tokyo-night-moon`, or
`tokyo-night-day` to use a different variant.

## Usage

Switch between variants interactively with `M-x tokyo-night-select`.

After changing customization variables, apply them without restarting
with `M-x tokyo-night-reload`.

Browse the full color palette with `M-x tokyo-night-list-colors`
(use `C-u` prefix to pick a specific variant).

## Customization

You can override individual colors without forking:

```emacs-lisp
(setq tokyo-night-override-colors-alist
      '(("tokyo-comment" . "#636da6")))  ; brighter comments
(load-theme 'tokyo-night t)
```

Overrides apply to all variants. See the `tokyo-*-colors-alist`
variables in `tokyo-night.el` for all available color names.

To disable scaled headings in org, outline, markdown, shr, and info:

```emacs-lisp
(setq tokyo-night-scale-headings nil)
(load-theme 'tokyo-night t)
```

To tweak individual scaling factors, use `custom-set-faces` after loading:

```emacs-lisp
(load-theme 'tokyo-night t)
(custom-set-faces
 '(org-level-1 ((t (:height 1.5))))
 '(org-level-2 ((t (:height 1.3)))))
```

For a more visible current line highlight, use `tokyo-bg-highlight`
instead of the default subtle `tokyo-bg-line`:

```emacs-lisp
(setq tokyo-night-override-colors-alist
      '(("tokyo-bg-line" . "#292e42")))  ; same as tokyo-bg-highlight
(load-theme 'tokyo-night t)
```

### Per-mode face overrides

If you want to tweak faces only in specific major modes (without affecting
all buffers), use `face-remap-add-relative` in a mode hook:

```emacs-lisp
;; Softer comments in org-mode
(add-hook 'org-mode-hook
          (lambda ()
            (face-remap-add-relative 'font-lock-comment-face
                                     :foreground "#636da6")))

;; Bolder strings in python-mode
(add-hook 'python-mode-hook
          (lambda ()
            (face-remap-add-relative 'font-lock-string-face
                                     :weight 'bold)))

;; Larger default font in eww
(add-hook 'eww-mode-hook
          (lambda ()
            (face-remap-add-relative 'default :height 1.15)))
```

These overrides are buffer-local and won't affect other modes. See
[Buffer-Local Face Remapping with face-remap-add-relative](https://emacsredux.com/blog/2026/03/16/buffer-local-face-remapping-with-face-remap-add-relative/)
for more details.

### Using theme colors in your config

The `tokyo-night-with-colors` macro binds all palette colors as local
variables, so you can reference them without hardcoding hex values:

```emacs-lisp
(tokyo-night-with-colors
  (set-face-attribute 'some-face nil :foreground tokyo-blue)
  (setq pdf-view-midnight-colors (cons tokyo-fg tokyo-bg)))
```

You can also look up a single color programmatically:

```emacs-lisp
(tokyo-night-get-color "tokyo-blue")  ; => "#7aa2f7"
```

### After-load hook

Run custom code every time a Tokyo theme is loaded:

```emacs-lisp
(add-hook 'tokyo-night-after-load-hook
          (lambda (_theme)
            (set-face-attribute 'line-number nil :slant 'italic)))
```

The hook function receives the theme name as its argument.

### Automatic light/dark switching

Tokyo Night pairs well with packages that switch themes based on the time of
day or your OS appearance setting:

- [auto-dark](https://github.com/LionyxML/auto-dark-emacs) tracks your OS
  dark/light mode and switches themes to match:

```emacs-lisp
(use-package auto-dark
  :config
  (setq auto-dark-dark-theme 'tokyo-night
        auto-dark-light-theme 'tokyo-night-day)
  (auto-dark-mode 1))
```

- [circadian](https://github.com/guidoschmidt/circadian.el) switches on a
  time-based schedule (e.g. sunrise/sunset):

```emacs-lisp
(use-package circadian
  :config
  (setq circadian-themes '((:sunrise . tokyo-night-day)
                            (:sunset  . tokyo-night)))
  (circadian-setup))
```

See [Automatic Light/Dark Theme Switching](https://emacsredux.com/blog/2026/03/29/automatic-light-dark-theme-switching/)
for a deeper look at both approaches.

## Supported Packages

Beyond all built-in Emacs faces, the theme covers these packages:

- **Completion:** vertico, corfu, marginalia, orderless, consult, embark, company, ivy, swiper
- **Development:** magit, forge, transient, git-commit, git-rebase, eglot, lsp-mode, lsp-ui, flycheck, cider
- **Editing:** evil, smartparens, rainbow-delimiters, avy, ace-window, hydra, which-key
- **Email:** mu4e, notmuch
- **UI:** doom-modeline, treemacs, helpful, markdown-mode, web-mode, elfeed

Missing a package? [Open an issue](https://github.com/bbatsov/tokyo-night-emacs/issues/new?template=feature_request.yml)
or submit a PR.

## Design

See [DESIGN.md](DESIGN.md) for the guiding principles behind color
choices and face definitions.

## Credits

The color palette is based on work by [enkia](https://github.com/enkia/tokyo-night-vscode-theme)
and [folke](https://github.com/folke/tokyonight.nvim).

## License

GPL v3+
