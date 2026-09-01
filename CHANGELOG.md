# Changelog

## Unreleased

### New features

* [#6](https://github.com/bbatsov/tokyo-night-emacs/pull/6): Add appearance options: customizable heading scale factors (`tokyo-night-height-1` through `-height-3` and `-height-doc-title`), `tokyo-night-use-variable-pitch`, `tokyo-night-italic-comments`, `tokyo-night-italic-keywords` and `tokyo-night-flat-mode-line`.

### Bug fixes

* [#8](https://github.com/bbatsov/tokyo-night-emacs/pull/8): Give Storm a
  visible `highlight` background; it still used upstream's base pairing, where
  `bg-highlight` sits a shade away from `bg`, so `highlight` and everything
  drawing from it was nearly invisible.
* [#9](https://github.com/bbatsov/tokyo-night-emacs/pull/9): Give
  `sp-show-pair-mismatch-face` a visible background, matching the fix
  `show-paren-mismatch` got in 1.0.0.
* [#10](https://github.com/bbatsov/tokyo-night-emacs/pull/10): Derive the diff
  backgrounds from each variant's own background; all twelve were too faint,
  and added on Day and removed/changed on Storm were invisible.
* [#13](https://github.com/bbatsov/tokyo-night-emacs/pull/13): Fix
  `tokyo-night-get-color`, `tokyo-night-with-colors`, `tokyo-night-reload` and
  `tokyo-night-list-colors` failing with "No Tokyo theme is active" on Emacs 27
  and 28, which never set the active variant.
* [#13](https://github.com/bbatsov/tokyo-night-emacs/pull/13): Make face text
  readable on its own background: `match` and the other search highlights were
  blue on blue in Day, and the diff line faces used a muted foreground on a
  same-hue background.
* [#11](https://github.com/bbatsov/tokyo-night-emacs/pull/11): Give `match`,
  `whitespace-empty`, `ido-indicator` and `gnus-summary-cancelled` real
  backgrounds instead of the buffer background, which lifted nothing and hid
  `hl-line` underneath; drop the background from `bookmark-face`, which is
  foreground-only.

### Documentation

* [#9](https://github.com/bbatsov/tokyo-night-emacs/pull/9): Document in
  DESIGN.md how the Neovim palette maps onto Emacs faces, and where the two
  disagree about what a color is for.

## 1.1.0 (2026-07-26)

### New features

* [#5](https://github.com/bbatsov/tokyo-night-emacs/pull/5): Add face support for breadcrumb.
* [#5](https://github.com/bbatsov/tokyo-night-emacs/pull/5): Add face support for gptel.
* [#4](https://github.com/bbatsov/tokyo-night-emacs/pull/4): Face coverage
  expanded to anzu, jinx, completion-preview, asciidoc-mode, vundo,
  volatile-highlights, easy-kill, clojure-mode, copilot, git-timemachine,
  haskell-mode, keycast, dictionary, mistty, erlang and inf-ruby.
* [#4](https://github.com/bbatsov/tokyo-night-emacs/pull/4): Deeper cider
  coverage: REPL, stacktrace and inline error faces, plus the nREPL message
  log (and corfu gained its doc popup face).

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
