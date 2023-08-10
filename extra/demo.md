---
title: 'Logic Programming in Haskell'
author: 'Jasper Van der Jeugt'
patat:
  incrementalLists: true
  wrap: true
  margins:
    left: 1
    right: 1
  eval:
    ghci:
      command: 'tr -d "\n" | ghci | tail -n2 | head -n1'
...

# Filler

## Filler

## Filler

## Filler

## Filler

# List Comprehensions

## Introduction

If we want to keep things simple, **list comprehensions** can be a powerful
tool.

~~~~~{.haskell}
evens = [x | x <- [1..10], x `mod` 2 == 0]
~~~~~

Statements in list comprehensions are usually either:

 -  _Pattern matches_ using `<-`
 -  Expressions of type `Bool`

. . .

You can also `let` to introduce new bindings, but this is less common.

## Laziness

Due to Haskell's _laziness_, we don't traverse the whole search space.
Searching for _triangular numbers_:

~~~~~{.haskell .ghci}
take 3 $
    [ (a, b, c)
    | c <- [1 .. 100000]
    , b <- [1 .. c], a <- [1 .. b]
    , a * a + b * b == c * c
    ]
~~~~~

## Laziness

Yo!

# Filler

## Filler

Filler

## Filler

Filler

## Filler

Filler

## Filler

## Filler

## Filler

## Filler

## Filler

## Filler

## Filler

## Filler

## Filler

## Filler

## Filler

## Filler

## Filler

## Filler

## Filler

## Filler

## Filler

## Filler

## Filler

## Filler

## Filler

## Filler

## Filler

## Filler

## Filler

## Filler

## Filler

## Filler

## Filler

## Filler

## Filler

## Filler

## Filler
