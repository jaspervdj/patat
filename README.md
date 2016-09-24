patat
=====

`patat` (**P**resentations **A**nd **T**he **A**NSI **T**erminal) is a small
tool that allows you to show presentations using only an ANSI terminal.  It does
not require `ncurses`.

![screenshot](extra/screenshot.png?raw=true)

`patat` is written in [Haskell] and built upon the great [Pandoc] library.  This
means it is not limited to Markdown, but it supports basically every input
format that Pandoc supports.

[Haskell]: http://haskell.org/
[Pandoc]: http://pandoc.org/

Trivia
------

_"Patat"_ is the Flemish word for a simple potato.  Dutch people also use it to
refer to French Fries but I don't really do that -- in Belgium we just call
fries _"Frieten"_.

The idea of `patat` is largely based upon [MDP] which is in turn based upon
[VTMC].  I wanted to write a clone using Pandoc because I ran into a markdown
parsing bug in MDP which I could not work around.  A second reason to do a
Pandoc-based tool was that I would be able to use [Literate Haskell] as well.
Lastly I also prefer not to install Node.js on my machine if I can avoid it.

[MDP]: https://github.com/visit1985/mdp
[VTMC]: https://github.com/jclulow/vtmc
[Literate Haskell]: https://wiki.haskell.org/Literate_programming
