{-# LANGUAGE RankNTypes #-}

module Util where

import Control.Lens
import Data.UUID as UUID
import Data.UUID.V4 as V4

withLens :: Lens' a b -> (b -> b) -> a -> a
withLens l f s = set l (f (view l s)) s

generateId :: IO String
generateId = fmap UUID.toString V4.nextRandom


