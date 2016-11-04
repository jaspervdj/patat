patat
=====

[![Build Status](https://img.shields.io/travis/jaspervdj/patat.svg)](https://travis-ci.org/jaspervdj/patat) [![Hackage](https://img.shields.io/hackage/v/patat.svg)](https://hackage.haskell.org/package/patat) [![GitHub tag](https://img.shields.io/github/tag/jaspervdj/patat.svg)]()

`patat` (**P**resentations **A**top **T**he **A**NSI **T**erminal) is a small
tool that allows you to show presentations using only an ANSI terminal.  It does
not require `ncurses`.

Features:

- Leverages the great [Pandoc] library to support many input formats including
  [Literate Haskell].
- Supports [smart slide splitting](#input-format).
- There is a [live reload](#running) mode.
- [Theming](#theming) support.
- Optionally [re-wrapping](#configuration) text to terminal width with proper
  indentation.
- Syntax highlighting for nearly one hundred languages generated from [Kate]
  syntax files.
- Written in [Haskell].

![screenshot](extra/screenshot.png?raw=true)

[Kate]: https://kate-editor.org/
[Haskell]: http://haskell.org/
[Pandoc]: http://pandoc.org/

Table of Contents
-----------------

-   [Installation](#installation)
    -   [Using stack](#using-stack)
    -   [Using cabal](#using-cabal)
-   [Running](#running)
-   [Input format](#input-format)
-   [Configuration](#configuration)
    -   [Theming](#theming)
    -   [Syntax Highlighting](#syntax-highlighting)
-   [Trivia](#trivia)

Installation
------------

You can build from source using `stack install` or `cabal install`.  `patat` is
also available from [Hackage].

[Hackage]: https://hackage.haskell.org/package/patat

For people unfamiliar with the Haskell ecosystem, this means you can do either
of the following:

### Using stack

1. Install [stack] for your platform.
2. Clone this repository.
3. Run `stack setup` (if you're running stack for the first time) and
   `stack install`.
4. Make sure `$HOME/.local/bin` is in your `$PATH`.

[stack]: https://docs.haskellstack.org/en/stable/README/

### Using cabal

1. Install [cabal] for your platform.
2. Run `cabal install patat`.
3. Make sure `$HOME/.cabal/bin` is in your `$PATH`.

[cabal]: https://www.haskell.org/cabal/

Running
-------

    patat [--watch] presentation.md

Controls:

- **Next slide**: `space`, `enter`, `l`, `→`
- **Previous slide**: `backspace`, `h`, `←`
- **Go forward 10 slides**: `j`, `↓`
- **Go backward 10 slides**: `k`, `↑`
- **First slide**: `0`
- **Last slide**: `G`
- **Reload file**: `r`
- **Quit**: `q`

The `r` key is very useful since it allows you to preview your slides while you
are writing them.  You can also use this to fix artifacts when the terminal is
resized.

If you provide the `--watch` flag, `patat` will watch the presentation file for
changes and reload automatically.  This is very useful when you are writing the
presentation.

Input format
------------

The input format can be anything that Pandoc supports.  Plain markdown is
usually the most simple solution:

    ---
    title: This is my presentation
    author: Jane Doe
    ...

    # This is a slide

    Slide contents.  Yay.

    ---

    # Important title

    Things I like:

    - Markdown
    - Haskell
    - Pandoc

Horizontal rulers (`---`) are used to split slides.

However, if you prefer not use these since they are a bit intrusive in the
markdown, you can also start every slide with an `h1` header.  In that case, the
file should not contain a single horizontal ruler.

This means the following document is equivalent:

    ---
    title: This is my presentation
    author: Jane Doe
    ...

    # This is a slide

    Slide contents.  Yay.

    # Important title

    Things I like:

    - Markdown
    - Haskell
    - Pandoc

Configuration
-------------

`patat` is fairly configurable.  The configuration is done using [YAML].  There
are two places where you can put your configuration:

1. In the presentation file itself, using the [Pandoc metadata header].
2. In `$HOME/.patat.yaml`

[YAML]: http://yaml.org/
[Pandoc metadata header]: http://pandoc.org/MANUAL.html#extension-yaml_metadata_block

For example, we can turn on line wrapping by using the following file:

    ---
    title: Presentation with wrapping
    author: John Doe
    patat:
        wrap: true
    ...

    This is a split
    line which should
    be re-wrapped.

Or we can use a normal presentation and have the following `$HOME/.patat.yaml`:

    wrap: true

### Theming

Colors and other properties can also be changed using this configuration.  For
example, we can have:

    ---
    author: 'Jasper Van der Jeugt'
    title: 'This is a test'
    patat:
        wrap: true
        theme:
            emph: [vividBlue, onVividBlack, bold]
            imageTarget: [onDullWhite, vividRed]
    ...

    # This is a presentation

    This is _emph_ text.

    ![Hello](foo.png)

The properties that can be given a list of styles are:

- `blockQuote`
- `borders`
- `bulletList`
- `codeBlock`
- `code`
- `definitionList`
- `definitionTerm`
- `emph`
- `header`
- `imageTarget`
- `imageText`
- `linkTarget`
- `linkText`
- `math`
- `orderedList`
- `quoted`
- `strikeout`
- `strong`
- `tableHeader`
- `tableSeparator`

The accepted styles are:

- `bold`
- `dullBlack`
- `dullBlue`
- `dullCyan`
- `dullGreen`
- `dullMagenta`
- `dullRed`
- `dullWhite`
- `dullYellow`
- `onDullBlack`
- `onDullBlue`
- `onDullCyan`
- `onDullGreen`
- `onDullMagenta`
- `onDullRed`
- `onDullWhite`
- `onDullYellow`
- `onVividBlack`
- `onVividBlue`
- `onVividCyan`
- `onVividGreen`
- `onVividMagenta`
- `onVividRed`
- `onVividWhite`
- `onVividYellow`
- `underline`
- `vividBlack`
- `vividBlue`
- `vividCyan`
- `vividGreen`
- `vividMagenta`
- `vividRed`
- `vividWhite`
- `vividYellow`

### Syntax Highlighting

As part of theming, syntax highlighting is also configurable.  This can be
configured like this:

    ---
    patat:
      theme:
        syntaxHighlighting:
          decVal: [bold, onDullRed]
    ...

    ...

`decVal` refers to "decimal values".  This is known as a "token type".  For a
full list of token types, see [this list] -- the names are derived from there in
an obvious way.

[this list]: https://hackage.haskell.org/package/highlighting-kate-0.6.3/docs/Text-Highlighting-Kate-Types.html#t:TokenType

Trivia
------

_"Patat"_ is the Flemish word for a simple potato.  Dutch people also use it to
refer to French Fries but I don't really do that -- in Belgium we just call
fries _"Frieten"_.

The idea of `patat` is largely based upon [MDP] which is in turn based upon
[VTMC].  I wanted to write a clone using Pandoc because I ran into a markdown
parsing bug in MDP which I could not work around.  A second reason to do a
Pandoc-based tool was that I would be able to use [Literate Haskell] as well.
Lastly, I also prefer not to install Node.js on my machine if I can avoid it.

[MDP]: https://github.com/visit1985/mdp
[VTMC]: https://github.com/jclulow/vtmc
[Literate Haskell]: https://wiki.haskell.org/Literate_programming
