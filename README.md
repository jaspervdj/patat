patat
=====

[![Build Status](https://travis-ci.org/jaspervdj/patat.svg?branch=master)](https://travis-ci.org/jaspervdj/patat)

`patat` (**P**resentations **A**nd **T**he **A**NSI **T**erminal) is a small
tool that allows you to show presentations using only an ANSI terminal.  It does
not require `ncurses`.

![screenshot](extra/screenshot.png?raw=true)

`patat` is written in [Haskell] and built upon the great [Pandoc] library.  This
means it is theoretically not limited to Markdown, but can support every
input format that Pandoc supports.

[Haskell]: http://haskell.org/
[Pandoc]: http://pandoc.org/

Table of Contents
-----------------

-   [Installation](#installation)
-   [Running](#running)
-   [Input format](#input-format)
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
