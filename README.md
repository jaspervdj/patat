patat
=====

`patat` (**P**resentations **A**nd **T**he **A**NSI **T**erminal) is a small
tool that allows you to show presentations using only an ANSI terminal.  It does
not require `ncurses`.

![screenshot](extra/screenshot.png?raw=true)

`patat` is written in [Haskell] and built upon the great [Pandoc] library.  This
means it is theoretically not limited to Markdown, but can it supports every
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

Running
-------

    patat presentation.md

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
