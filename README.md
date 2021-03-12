patat
=====

![CI](https://github.com/jaspervdj/patat/workflows/CI/badge.svg) [![Hackage](https://img.shields.io/hackage/v/patat.svg)](https://hackage.haskell.org/package/patat) [![GitHub tag](https://img.shields.io/github/tag/jaspervdj/patat.svg)]()

`patat` (**P**resentations **A**top **T**he **A**NSI **T**erminal) is a small
tool that allows you to show presentations using only an ANSI terminal.  It does
not require `ncurses`.

Features:

- Leverages the great [Pandoc] library to support many input formats including
  [Literate Haskell].
- Supports [smart slide splitting](#input-format).
- Slides can be split up into [multiple fragments](#fragmented-slides)
- There is a [live reload](#running) mode.
- [Theming](#theming) support including 24-bit RGB.
- [Auto advancing](#auto-advancing) with configurable delay.
- Optionally [re-wrapping](#line-wrapping) text to terminal width with proper
  indentation.
- Syntax highlighting for nearly one hundred languages generated from [Kate]
  syntax files.
- Experimental [images](#images) support.
- Supports [evaluating code snippets and showing the result](#evaluating-code).
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
    -   [Images](#images)
    -   [Breadcrumbs](#breadcrumbs)
    -   [Evaluating code](#evaluating-code)
-   [Trivia](#trivia)

Installation
------------

### Pre-built-packages

- Archlinux: <https://aur.archlinux.org/packages/patat-bin>
- Debian: <https://packages.debian.org/unstable/patat>
- Ubuntu: <https://packages.ubuntu.com/bionic/patat>
- openSUSE: <https://build.opensuse.org/package/show/openSUSE:Factory:ARM/patat>
- Fedora: <https://src.fedoraproject.org/rpms/patat>

You can also find generic Linux and Mac OS binaries here:
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
- **Jump to slide N**: `N` followed by `enter`
- **Reload file**: `r`
- **Quit**: `q`

The `r` key is very useful since it allows you to preview your slides while you
are writing them.  You can also use this to fix artifacts when the terminal is
resized.

Input format
------------

The input format can be anything that Pandoc supports.  Plain markdown is
usually the most simple solution:

```markdown
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
```

Horizontal rulers (`---`) are used to split slides.

However, if you prefer not use these since they are a bit intrusive in the
markdown, you can also start every slide with a header.  In that case, the file
should not contain a single horizontal ruler.

`patat` will pick the most deeply nested header (e.g. `h2`) as the marker for a
new slide.  Headers _above_ the most deeply nested header (e.g. `h1`) will turn
into title slides, which are displayed as as a slide containing only the
centered title.

This means the following document is equivalent to the one we saw before:

```markdown
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
```

And that following document contains three slides: a title slide, followed by
two content slides.

```markdown
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
```

For more information, see [Advanced slide splitting](#advanced-slide-splitting).

Patat supports comments which can be used as speaker notes.

```markdown
---
title: This is my presentation
author: Jane Doe
...

# Chapter 1

<!--
Note: I should not bore the audience with my thoughts on powerpoint but
just get straight to the point.
-->

Slide contents.  Yay.

<!-- TODO: Finish the rest of the presentation. -->
```

Configuration
-------------

`patat` is fairly configurable.  The configuration is done using [YAML].  There
are two places where you can put your configuration:

1. In the presentation file itself, using the [Pandoc metadata header].
2. In `$HOME/.patat.yaml`

[YAML]: http://yaml.org/
[Pandoc metadata header]: http://pandoc.org/MANUAL.html#extension-yaml_metadata_block

For example, we set an option `key` to `val` by using the following file:

```markdown
---
title: Presentation with options
author: John Doe
patat:
    key: val
...

Hello world.
```

Or we can use a normal presentation and have the following `$HOME/.patat.yaml`:

    key: val

### Line wrapping

Line wrapping can be enabled by setting `wrap: true` in the configuration.  This
will re-wrap all lines to fit the terminal width better.

### Margins

Margins can be enabled by setting a `margins` entry in the configuration:

```markdown
---
title: Presentation with margins
author: John Doe
patat:
    wrap: true
    margins:
        left: 10
        right: 10
...

Lorem ipsum dolor sit amet, ...
```

This example configuration will generate slides with a margin of 10 characters on the left,
and break lines 10 characters before they reach the end of the terminal's width.

It is recommended to enable [line wrapping](#line-wrapping) along with this feature.

### Auto advancing

By setting `autoAdvanceDelay` to a number of seconds, `patat` will automatically
advance to the next slide.

```markdown
---
title: Auto-advance, yes please
author: John Doe
patat:
    autoAdvanceDelay: 2
...

Hello World!

---

This slide will be shown two seconds after the presentation starts.
```

Note that changes to `autoAdvanceDelay` are not picked up automatically if you
are running `patat --watch`.  This requires restarting `patat`.

### Advanced slide splitting

You can control the way slide splitting works by setting the `slideLevel`
variable.  This variable defaults to the least header that occurs before a
non-header, but it can also be explicitly defined.  For example, in the
following document, the `slideLevel` defaults to **2**:

```markdown
# This is a slide

## This is a nested header

This is some content
```

With `slideLevel` 2, the `h1` will turn into a "title slide", and the `h2` will
be displayed at the top of the second slide.  We can customize this by setting
`slideLevel` manually:

```markdown
---
patat:
  slideLevel: 1
...

# This is a slide

## This is a nested header

This is some content
```

Now, we will only see one slide, which contains a nested header.

### Fragmented slides

By default, slides are always displayed "all at once".  If you want to display
them fragment by fragment, there are two ways to do that.  The most common
case is that lists should be displayed incrementally.

This can be configured by settings `incrementalLists` to `true` in the metadata
block:

```markdown
---
title: Presentation with incremental lists
author: John Doe
patat:
    incrementalLists: true
...

- This list
- is displayed
- item by item
```

Setting `incrementalLists` works on _all_ lists in the presentation.  To flip
the setting for a specific list, wrap it in a block quote.  This will make the
list incremental if `incrementalLists` is not set, and it will display the list
all at once if `incrementalLists` is set to `true`.

This example contains a sublist which is also displayed incrementally, and then
a sublist which is displayed all at once (by merit of the block quote).

```markdown
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
```

Another way to break up slides is to use a pagraph only containing three dots
separated by spaces.  For example, this slide has two pauses:

```markdown
Legen

. . .

wait for it

. . .

Dary!
```

### Theming

Colors and other properties can also be changed using this configuration.  For
example, we can have:

```markdown
---
author: 'Jasper Van der Jeugt'
title: 'This is a test'
patat:
    wrap: true
    theme:
        emph: [vividBlue, onVividBlack, italic]
        strong: [bold]
        imageTarget: [onDullWhite, vividRed]
...

# This is a presentation

This is _emph_ text.

![Hello](foo.png)
```

The properties that can be given a list of styles are:

`blockQuote`, `borders`, `bulletList`, `codeBlock`, `code`, `definitionList`,
`definitionTerm`, `emph`, `header`, `imageTarget`, `imageText`, `linkTarget`,
`linkText`, `math`, `orderedList`, `quoted`, `strikeout`, `strong`,
`tableHeader`, `tableSeparator`, `underline`

The accepted styles are:

`bold`, `italic`, `dullBlack`, `dullBlue`, `dullCyan`, `dullGreen`,
`dullMagenta`, `dullRed`, `dullWhite`, `dullYellow`, `onDullBlack`,
`onDullBlue`, `onDullCyan`, `onDullGreen`, `onDullMagenta`, `onDullRed`,
`onDullWhite`, `onDullYellow`, `onVividBlack`, `onVividBlue`, `onVividCyan`,
`onVividGreen`, `onVividMagenta`, `onVividRed`, `onVividWhite`, `onVividYellow`,
`underline`, `vividBlack`, `vividBlue`, `vividCyan`, `vividGreen`,
`vividMagenta`, `vividRed`, `vividWhite`, `vividYellow`

Also accepted are styles of the form `rgb#RrGgBb` and `onRgb#RrGgBb`, where `Rr`
`Gg` and `Bb` are hexadecimal bytes (e.g. `rgb#f08000` for an orange foreground,
and `onRgb#101060` for a deep purple background).  Naturally, your terminal
needs to support 24-bit RGB for this to work.  When creating portable
presentations, it might be better to stick with the named colours listed above.

### Syntax Highlighting

As part of theming, syntax highlighting is also configurable.  This can be
configured like this:

```markdown
---
patat:
  theme:
    syntaxHighlighting:
      decVal: [bold, onDullRed]
...

...
```

`decVal` refers to "decimal values".  This is known as a "token type".  For a
full list of token types, see [this list] -- the names are derived from there in
an obvious way.

[this list]: https://hackage.haskell.org/package/highlighting-kate-0.6.3/docs/Text-Highlighting-Kate-Types.html#t:TokenType

Note that in order to get syntax highlighting to work, you should annotate code
blocks with the language, e.g. using a fenced code block:

    ```ruby
    puts "Hello, world!"
    ```

### Pandoc Extensions

Pandoc comes with a fair number of extensions on top of markdown, listed [here](https://hackage.haskell.org/package/pandoc-2.0.5/docs/Text-Pandoc-Extensions.html).

`patat` enables a number of them by default, but this is also customizable.

In order to enable an additional extensions, e.g. `autolink_bare_uris`, add it
to the `pandocExtensions` field in the YAML metadata:

```markdown
---
patat:
  pandocExtensions:
    - patat_extensions
    - autolink_bare_uris
...

Document content...
```

The `patat_extensions` in the above snippet refers to the default set of
extensions enabled by `patat`.  If you want to disable those and only use a
select few extensions, simply leave it out and choose your own:

```markdown
---
patat:
  pandocExtensions:
    - autolink_bare_uris
    - emoji
...

...

Document content...
```

If you don't want to enable any extensions, simply set `pandocExtensions` to the
empty list `[]`.


### Images

`patat-0.8.0.0` and newer include images support for some terminal emulators.

```markdown
---
patat:
  images:
    backend: auto
...

# A slide with only an image.

![](matterhorn.jpg)
```

If `images` is enabled (not by default), `patat` will draw slides that consist
only of a single image just by drawing the image, centered and resized to fit
the terminal window.

`patat` supports the following image drawing backends:

-   `backend: iterm2`: uses [iTerm2](https://iterm2.com/)'s special escape
    sequence to render the image.  This even works with animated GIFs!

-   `backend: kitty`: uses
    [Kitty's icat command](https://sw.kovidgoyal.net/kitty/kittens/icat.html).

-   `backend: w3m`: uses the `w3mimgdisplay` executable to draw directly onto
    the window.  This has been tested in `urxvt` and `xterm`, but is known to
    produce weird results in `tmux`.

    If `w3mimgdisplay` is in a non-standard location, you can specify that using
    `path`:

    ```yaml
    backend: 'w3m'
    path: '/home/jasper/.local/bin/w3mimgdisplay'
    ```

### Breadcrumbs

By default, `patat` will print a breadcrumbs-style header, e.g.:

    example.md > This is a title > This is a subtitle

This feature can be turned off by using:

```yaml
patat:
  breadcrumbs: false
```

### Evaluating code

`patat` can evaluate code blocks and show the result.  You can register an
_evaluator_ by specifying this in the YAML metadata:

    ---
    patat:
      eval:
        ruby:
          command: irb --noecho --noverbose
          fragment: true  # Optional
          replace: false  # Optional
    ...

    Here is an example of a code block that is evaluated:

    ```ruby
    puts "Hi"
    ```

An arbitrary amount of evaluators can be specified, and whenever a a class
attribute on a code block matches the evaluator, it will be used.

**Note that executing arbitrary code is always dangerous**, so double check the
code of presentations downloaded from the internet before running them if they
contain `eval` settings.

Aside from the command, there are two more options:

 -  `fragment`: Introduce a pause (see [fragments](#fragmented-slides)) in
    between showing the original code block and the output.  Defaults to `true`.
 -  `replace`: Remove the original code block and replace it with the output
    rather than appending the output in a new code block.  Defaults to `false`.

Setting `fragment: false` and `replace: true` offers a way to "filter" code
blocks, which can be used to render ASCII graphics.

    ---
    patat:
      eval:
        figlet:
          command: figlet
          fragment: false
          replace: true
    ...

    ```figlet
    Fancy Font
    ```

This feature works by simply by:

1.  Spawn a process with the provided command
2.  Write the contents of the code block to the `stdin` of the process
3.  Wait for the process to exit
4.  Render the `stdout` of the process

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
