patat
=====

[![Build Status](https://img.shields.io/circleci/project/github/jaspervdj/patat.svg)](https://circleci.com/gh/jaspervdj/patat) [![Hackage](https://img.shields.io/hackage/v/patat.svg)](https://hackage.haskell.org/package/patat) [![GitHub tag](https://img.shields.io/github/tag/jaspervdj/patat.svg)]()

`patat` (**P**resentations **A**top **T**he **A**NSI **T**erminal) is a small
tool that allows you to show presentations using only an ANSI terminal.  It does
not require `ncurses`.

Features:

- Leverages the great [Pandoc] library to support many input formats including
  [Literate Haskell].
- Supports [smart slide splitting](#input-format).
- Slides can be split up into [multiple fragments](#fragmented-slides)
- There is a [live reload](#running) mode.
- [Theming](#theming) support.
- [Auto advancing](#auto-advancing) with configurable delay.
- Optionally [re-wrapping](#line-wrapping) text to terminal width with proper
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

-   [Table of Contents](#table-of-contents)
-   [Installation](#installation)
    -   [Pre-built-packages](#pre-built-packages)
    -   [From source](#from-source)
-   [Running](#running)
-   [Options](#options)
-   [Controls](#controls)
-   [Input format](#input-format)
-   [Configuration](#configuration)
    -   [Line wrapping](#line-wrapping)
    -   [Auto advancing](#auto-advancing)
    -   [Advanced slide splitting](#advanced-slide-splitting)
    -   [Fragmented slides](#fragmented-slides)
    -   [Theming](#theming)
    -   [Syntax Highlighting](#syntax-highlighting)
    -   [Pandoc Extensions](#pandoc-extensions)
-   [Trivia](#trivia)

Installation
------------

### Pre-built-packages

- Debian: <https://packages.debian.org/unstable/patat>
- Ubuntu: <https://packages.ubuntu.com/artful/patat>
- openSUSE: <https://build.opensuse.org/package/show/openSUSE:Factory:ARM/patat>

You can also find generic linux binaries here:
<https://github.com/jaspervdj/patat/releases>.

### From source

Installation from source is very easy.  You can build from source using `stack
install` or `cabal install`.  `patat` is also available from [Hackage].

[Hackage]: https://hackage.haskell.org/package/patat

For people unfamiliar with the Haskell ecosystem, this means you can do either
of the following:

#### Using stack

1. Install [stack] for your platform.
2. Clone this repository.
3. Run `stack setup` (if you're running stack for the first time) and
   `stack install`.
4. Make sure `$HOME/.local/bin` is in your `$PATH`.

[stack]: https://docs.haskellstack.org/en/stable/README/

#### Using cabal

1. Install [cabal] for your platform.
2. Run `cabal install patat`.
3. Make sure `$HOME/.cabal/bin` is in your `$PATH`.

[cabal]: https://www.haskell.org/cabal/

Running
-------

`patat [*options*] file`

Options
-------

`-w`, `--watch`

:   If you provide the `--watch` flag, `patat` will watch the presentation file
    for changes and reload automatically.  This is very useful when you are
    writing the presentation.

`-f`, `--force`

:   Run the presentation even if the terminal claims it does not support ANSI
    features.

`-d`, `--dump`

:   Just dump all the slides to stdout.  This is useful for debugging.

`--version`

:   Display version information.

Controls
--------

- **Next slide**: `space`, `enter`, `l`, `→`, `PageDown`
- **Previous slide**: `backspace`, `h`, `←`, `PageUp`
- **Go forward 10 slides**: `j`, `↓`
- **Go backward 10 slides**: `k`, `↑`
- **First slide**: `0`
- **Last slide**: `G`
- **Reload file**: `r`
- **Quit**: `q`

The `r` key is very useful since it allows you to preview your slides while you
are writing them.  You can also use this to fix artifacts when the terminal is
resized.

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
markdown, you can also start every slide with a header.  In that case, the file
should not contain a single horizontal ruler.

`patat` will pick the most deeply nested header (e.g. `h2`) as the marker for a
new slide.  Headers _above_ the most deeply nested header (e.g. `h1`) will turn
into title slides, which are displayed as as a slide containing only the
centered title.

This means the following document is equivalent to the one we saw before:

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

And that following document contains three slides: a title slide, followed by
two content slides.

    ---
    title: This is my presentation
    author: Jane Doe
    ...

    # Chapter 1

    ## This is a slide

    Slide contents.  Yay.

    ## Another slide

    Things I like:

    - Markdown
    - Haskell
    - Pandoc

For more information, see [Advanced slide splitting](#advanced-slide-splitting).

Configuration
-------------

`patat` is fairly configurable.  The configuration is done using [YAML].  There
are two places where you can put your configuration:

1. In the presentation file itself, using the [Pandoc metadata header].
2. In `$HOME/.patat.yaml`

[YAML]: http://yaml.org/
[Pandoc metadata header]: http://pandoc.org/MANUAL.html#extension-yaml_metadata_block

For example, we set an option `key` to `val` by using the following file:

    ---
    title: Presentation with options
    author: John Doe
    patat:
        key: val
    ...

    Hello world.

Or we can use a normal presentation and have the following `$HOME/.patat.yaml`:

    key: val

### Line wrapping

Line wrapping can be enabled by setting `wrap: true` in the configuration.  This
will re-wrap all lines to fit the terminal width better.

### Auto advancing

By setting `autoAdvanceDelay` to a number of seconds, `patat` will automatically
advance to the next slide.

    ---
    title: Auto-advance, yes please
    author: John Doe
    patat:
        autoAdvanceDelay: 2
    ...

    Hello World!

    ---

    This slide will be shown two seconds after the presentation starts.

Note that changes to `autoAdvanceDelay` are not picked up automatically if you
are running `patat --watch`.  This requires restarting `patat`.

### Advanced slide splitting

You can control the way slide splitting works by setting the `slideLevel`
variable.  This variable defaults to the least header that occurs before a
non-header, but it can also be explicitly defined.  For example, in the
following document, the `slideLevel` defaults to **2**:

    # This is a slide

    ## This is a nested header

    This is some content

With `slideLevel` 2, the `h1` will turn into a "title slide", and the `h2` will
be displayed at the top of the second slide.  We can customize this by setting
`slideLevel` manually:

    ---
    patat:
      slideLevel: 1
    ...

    # This is a slide

    ## This is a nested header

    This is some content

Now, we will only see one slide, which contains a nested header.

### Fragmented slides

By default, slides are always displayed "all at once".  If you want to display
them fragment by fragment, there are two ways to do that.  The most common
case is that lists should be displayed incrementally.

This can be configured by settings `incrementalLists` to `true` in the metadata
block:

    ---
    title: Presentation with incremental lists
    author: John Doe
    patat:
        incrementalLists: true
    ...

    - This list
    - is displayed
    - item by item

Setting `incrementalLists` works on _all_ lists in the presentation.  To flip
the setting for a specific list, wrap it in a block quote.  This will make the
list incremental if `incrementalLists` is not set, and it will display the list
all at once if `incrementalLists` is set to `true`.

This example contains a sublist which is also displayed incrementally, and then
a sublist which is displayed all at once (by merit of the block quote).

    ---
    title: Presentation with incremental lists
    author: John Doe
    patat:
        incrementalLists: true
    ...

    - This list
    - is displayed

        * item
        * by item

    - Or sometimes

        > * all at
        > * once

Another way to break up slides is to use a pagraph only containing three dots
separated by spaces.  For example, this slide has two pauses:

    Legen

    . . .

    wait for it

    . . .

    Dary!

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

`blockQuote`, `borders`, `bulletList`, `codeBlock`, `code`, `definitionList`,
`definitionTerm`, `emph`, `header`, `imageTarget`, `imageText`, `linkTarget`,
`linkText`, `math`, `orderedList`, `quoted`, `strikeout`, `strong`,
`tableHeader`, `tableSeparator`

The accepted styles are:

`bold`, `dullBlack`, `dullBlue`, `dullCyan`, `dullGreen`, `dullMagenta`,
`dullRed`, `dullWhite`, `dullYellow`, `onDullBlack`, `onDullBlue`, `onDullCyan`,
`onDullGreen`, `onDullMagenta`, `onDullRed`, `onDullWhite`, `onDullYellow`,
`onVividBlack`, `onVividBlue`, `onVividCyan`, `onVividGreen`, `onVividMagenta`,
`onVividRed`, `onVividWhite`, `onVividYellow`, `underline`, `vividBlack`,
`vividBlue`, `vividCyan`, `vividGreen`, `vividMagenta`, `vividRed`,
`vividWhite`, `vividYellow`

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

### Pandoc Extensions

Pandoc comes with a fair number of extensions on top of markdown:

    <https://hackage.haskell.org/package/pandoc-2.0.5/docs/Text-Pandoc-Extensions.html>

`patat` enables a number of them by default, but this is also customizable.

In order to enable an additional extensions, e.g. `autolink_bare_uris`, add it
to the `pandocExtensions` field in the YAML metadata:

    ---
    patat:
      pandocExtensions:
        - patat_extensions
        - autolink_bare_uris
    ...

    Document content...

The `patat_extensions` in the above snippet refers to the default set of
extensions enabled by `patat`.  If you want to disable those and only use a
select few extensions, simply leave it out and choose your own:

    ---
    patat:
      pandocExtensions:
        - autolink_bare_uris
        - emoji
    ...

    ...

    Document content...

If you don't want to enable any extensions, simply set `pandocExtensions` to the
empty list `[]`.

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
