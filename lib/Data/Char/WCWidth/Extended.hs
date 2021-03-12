module Data.Char.WCWidth.Extended
    ( module Data.Char.WCWidth
    , wcstrwidth
    ) where

import Data.Char.WCWidth

wcstrwidth :: String -> Int
wcstrwidth = sum . map wcwidth
