--------------------------------------------------------------------------------
-- | Defines a cleanup action that needs to be run after we're done with a slide
-- or image.
module Patat.Cleanup
    ( Cleanup
    ) where


--------------------------------------------------------------------------------
type Cleanup = IO ()
