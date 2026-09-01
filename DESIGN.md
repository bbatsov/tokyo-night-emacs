# Design Principles

## Stay faithful to the original

The color palettes come directly from [folke's Tokyo Night](https://github.com/folke/tokyonight.nvim)
and [enkia's VS Code theme](https://github.com/enkia/tokyo-night-vscode-theme). We don't invent new
colors -- we map the existing palette to Emacs faces.

## Coming from the Neovim palette

Upstream doesn't keep four independent palettes, and the shape it does keep is
worth knowing before you touch a color.

Storm is the base. Night is a copy of Storm with a few background keys
overridden, and Day is a programmatic inversion of Night:

```lua
-- colors/night.lua
local ret = vim.deepcopy(require("tokyonight.colors.storm"))
return vim.tbl_deep_extend("force", ret, {
  bg = "#1a1b26", bg_dark = "#16161e", bg_dark1 = "#0C0E14",
})
```

We flatten all of that into four complete alists, which loses the structure.
Three things follow from the flattening, and each has caused a real bug here.

**A value shared between variants stops looking shared.** Every entry in every
alist looks equally deliberate, so there is nothing to tell you that Night and
Storm carry the same `tokyo-bg-highlight` because upstream picked it once.

**A color tuned against one background lands differently on another.** Night
overrides `bg` and leaves `bg-highlight` alone, so its lift widens as a side
effect. Storm kept the base pairing and sat at a contrast ratio of 1.08, near
enough to invisible, until #8. When you change a background, check what was
sitting on it. The suite pins a floor of 1.15 between `tokyo-bg` and
`tokyo-bg-highlight` in every variant.

The diff backgrounds are the same story and are handled by deriving rather
than picking. Each one is its accent color blended into that variant's own
background, following how upstream builds its diff shades: green at 0.22 for
added, `red-dark` at 0.25 for removed, `blue-dark` at 0.30 for changed. They
scale with the variant by construction, and the suite checks both the recipe
and the resulting lift. Upstream's own ratio for changed is 0.15, which is
tuned for Neovim's DiffChange where faint is the intent; here the same color
also backs magit's base face and ediff, which have to read, so it is stronger.

**Upstream color roles are not Emacs face roles.** This is the one that catches
people. `bg_highlight` drives CursorLine in Neovim, where being barely there is
exactly the intent. Our nearest equivalent to CursorLine is `hl-line`, which
takes `tokyo-bg-line` instead. `tokyo-bg-highlight` goes to `highlight`, and
through it to `secondary-selection`, `widget-field`,
`show-paren-match-expression`, `completions-highlight` and the ediff diff
backgrounds, all of which the user is meant to notice. A color inherited from a
group where subtlety is correct can be plainly wrong once it lands on an Emacs
face that has to read.

So when you map a new face, ask what the color does in Emacs rather than which
Neovim group it came from. Faithfulness is about the palette, not about the
role each color happened to play upstream.

**Day is meant to be less contrasty, so don't "fix" it.** Upstream builds Day
by inverting Night's HSLuv lightness and then lightening whatever came out
dark, scaled by `day_brightness`, which upstream documents as running "from
dull to vibrant colors":

```lua
hsl[3] = 100 - hsl[3]
if hsl[3] < 40 then hsl[3] = hsl[3] + (100 - hsl[3]) * M.day_brightness end
```

Vibrancy costs contrast. Day's accents sit at 3 to 4:1 against the background
where the dark variants reach 6 to 10:1, which is why a pairing that is
comfortable in Night can land near 2:1 in Day. Darkening the accents would buy
contrast at the price of no longer looking like Tokyo Night Day, so the suite
holds Day to a lower legibility floor rather than treating the gap as a bug.
Anyone who wants more contrast has `tokyo-night-override-colors-alist`.

What is still worth fixing in Day is a pairing we chose rather than inherited.
`tokyo-terminal-blk` is an ANSI palette entry, not a UI layer, and using it to
back text gave Day its worst readings until magit's hunk and conflict headings
moved onto `tokyo-bg-highlight`, which is what the file heading beside them
already used.

**`:background tokyo-bg` is not a background.** It is the default background,
so it lifts nothing. It reads as deliberate in a face definition and does
nothing at all on screen. If a face should stand out, give it `tokyo-bg-highlight`
or a purpose-built shade.

## Consistent semantic color mapping

Each color has a role, and that role is consistent across all faces:

- **Blue** (`tokyo-blue`) -- functions, actions, interactive elements, current items
- **Magenta** (`tokyo-magenta`) -- keywords, structural markers, headings
- **Green** (`tokyo-green`) -- strings, success, additions, confirmations
- **Orange** (`tokyo-orange`) -- constants, numbers, warnings-lite, notable items
- **Red** (`tokyo-red`) -- tags, deletions, errors, dangerous actions
- **Teal** (`tokyo-teal`) -- links, properties, preprocessor, secondary references
- **Cyan** (`tokyo-cyan`) -- types, supporting elements, dates
- **Yellow** (`tokyo-yellow`) -- warnings, modifications, function arguments
- **Comment** (`tokyo-comment`) -- de-emphasized text, disabled, inactive, ignored

When theming a new package, find the closest semantic match rather than
picking colors for visual variety.

## Restrained use of emphasis

- Use `bold` sparingly -- it should signal importance, not be the default.
- Use `italic` for comments, keywords, and documentation -- things that are
  contextual rather than primary.
- Avoid combining too many attributes (bold + italic + underline + color).
  One or two is usually enough.

## Readable contrast, not maximum contrast

Faces should be comfortably legible but not harsh. De-emphasized elements
(comments, line numbers, inactive UI) should recede without becoming invisible.
The background shades (`tokyo-bg-dark`, `tokyo-bg-highlight`, `tokyo-bg-line`)
exist to create subtle layering rather than sharp borders.

## All four variants from one definition

The face definitions are shared across all variants. Color differences come
entirely from the palette alists. If a face looks right in one variant but
wrong in another, the fix belongs in the palette, not in a variant-specific
face override.

## Prefer inheritance where appropriate

When a package face is semantically identical to a built-in face (e.g.,
`sp-show-pair-match-face` and `show-paren-match`), use the same colors
to reinforce consistency. Users should not have to learn a different
visual language for each package.

## Theme what exists, don't invent decoration

Only set attributes that serve a purpose. Don't add background colors,
boxes, or underlines just because a face allows them. A foreground color
alone is often enough. Extra decoration should earn its place by solving
a readability or identification problem.
