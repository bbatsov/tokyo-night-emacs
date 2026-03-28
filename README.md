# Tokyo Night Theme for Emacs

[![CI](https://github.com/bbatsov/emacs-tokyo-themes/actions/workflows/ci.yml/badge.svg)](https://github.com/bbatsov/emacs-tokyo-themes/actions/workflows/ci.yml)

A faithful Emacs port of [folke's Tokyo Night](https://github.com/folke/tokyonight.nvim) color theme.

![Tokyo Storm](screenshots/tokyo-storm.png)

All four variants are included:

| Theme | Background | Description |
|-------|-----------|-------------|
| `tokyo-night` | `#1a1b26` | The darkest variant (default) |
| `tokyo-storm` | `#24283b` | Medium dark |
| `tokyo-moon` | `#222436` | Blue-tinted dark, unique accents |
| `tokyo-day` | `#e1e2e7` | Light variant |

## Installation

### package-vc (Emacs 29+)

```emacs-lisp
(package-vc-install "https://github.com/bbatsov/emacs-tokyo-themes")
(load-theme 'tokyo-night t)
```

### use-package (Emacs 30+)

```emacs-lisp
(use-package tokyo-themes
  :vc (:url "https://github.com/bbatsov/emacs-tokyo-themes" :rev :newest)
  :config
  (load-theme 'tokyo-night t))
```

### Manual

Drop all `.el` files somewhere on your `load-path` and `custom-theme-load-path`, then:

```emacs-lisp
(load-theme 'tokyo-night t)
```

Replace `tokyo-night` with `tokyo-storm`, `tokyo-moon`, or
`tokyo-day` to use a different variant.

## Customization

You can override individual colors without forking:

```emacs-lisp
(setq tokyo-themes-override-colors-alist
      '(("tokyo-comment" . "#636da6")))  ; brighter comments
(load-theme 'tokyo-night t)
```

Overrides apply to all variants. See the `tokyo-*-colors-alist`
variables in `tokyo-themes.el` for all available color names.

To disable scaled headings in org, outline, markdown, shr, and info:

```emacs-lisp
(setq tokyo-themes-scale-headings nil)
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
(setq tokyo-themes-override-colors-alist
      '(("tokyo-bg-line" . "#292e42")))  ; same as tokyo-bg-highlight
(load-theme 'tokyo-night t)
```

## Design

See [DESIGN.md](DESIGN.md) for the guiding principles behind color
choices and face definitions.

## Credits

The color palette is based on work by [enkia](https://github.com/enkia/tokyo-night-vscode-theme)
and [folke](https://github.com/folke/tokyonight.nvim).

## License

GPL v3+
