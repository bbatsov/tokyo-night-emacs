# Changelog

## Unreleased

### Bug fixes

* Fix `tokyo-night-scale-headings` not affecting org-mode and other
  outline-based headings -- `outline-1..3` were missing `:height`, so
  the scale factors never flowed through to org (which inherits them).

## 1.0.0 (2026-04-21)

### New features

* Published on [MELPA](https://melpa.org/#/tokyo-night); base package
  renamed to `tokyo-night` for MELPA namespace compliance.
* Face coverage expanded to mu4e, notmuch, evil, plus 12 additional
  packages.

### Bug fixes

* Give `show-paren-mismatch` a visible background (previously `tokyo-bg`
  was effectively no lift).
* Fix `hi-pink` using red background instead of pink.
* Align `font-lock-type-face` with DESIGN.md so types are distinguishable
  from builtins.

### Documentation

* Add section on automatic light/dark theme switching (auto-dark, circadian).

## 0.1.0 (2026-03-29)

### New features

* Initial release with all four Tokyo Night variants: `tokyo-night`, `tokyo-night-storm`, `tokyo-night-moon`, `tokyo-night-day`.
* Comprehensive face coverage for built-in Emacs faces and packages.
* Third-party package support: magit, transient, vertico, corfu, marginalia,
  orderless, consult, embark, company, ivy, swiper, flycheck, which-key,
  rainbow-delimiters, markdown-mode, helpful, avy, ace-window, cider,
  doom-modeline, elfeed, forge, hydra, lsp-mode, lsp-ui, smartparens,
  treemacs, web-mode.
* `tokyo-night-select` command for switching variants interactively.
* `tokyo-night-reload` command for applying config changes.
* `tokyo-night-list-colors` command for browsing the palette.
* `tokyo-night-with-colors` macro for accessing theme colors in user config.
* `tokyo-night-get-color` function for programmatic color lookup.
* `tokyo-night-after-load-hook` for running code after theme loads.
* `tokyo-night-scale-headings` option to toggle heading size scaling.
* `tokyo-night-override-colors-alist` for customizing individual colors.
