--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Patat.Presentation.Internal
    ( Presentation (..)
    , Slide (..)
    ) where


--------------------------------------------------------------------------------
import           Data.Monoid (Monoid)
import qualified Text.Pandoc as Pandoc
import           Prelude


--------------------------------------------------------------------------------
data Presentation = Presentation
    { pFilePath    :: !FilePath
    , pTitle       :: ![Pandoc.Inline]
    , pAuthor      :: ![Pandoc.Inline]
    , pSlides      :: [Slide]
    , pActiveSlide :: !Int
    } deriving (Show)


--------------------------------------------------------------------------------
newtype Slide = Slide {unSlide :: [Pandoc.Block]}
    deriving (Monoid, Show)
