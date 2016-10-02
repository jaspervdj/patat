module Data.Data.Extended
    ( module Data.Data

    , grecQ
    , grecT
    ) where

import           Data.Data

-- | Recursively find all values of a certain type.
grecQ :: (Data a, Data b) => a -> [b]
grecQ = concat . gmapQ (\x -> maybe id (:) (cast x) $ grecQ x)

-- | Recursively apply an update to a certain type.
grecT :: (Data a, Data b) => (a -> a) -> b -> b
grecT f x = gmapT (grecT f) (castMap f x)

castMap :: (Typeable a, Typeable b) => (a -> a) -> b -> b
castMap f x = case cast x of
    Nothing -> x
    Just y  -> case cast (f y) of
        Nothing -> x
        Just z  -> z
