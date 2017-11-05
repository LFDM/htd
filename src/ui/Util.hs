{-# LANGUAGE RankNTypes #-}

module Util where

import Control.Lens

withLens :: Lens' a b -> (b -> b) -> a -> a
withLens l f s = set l (f (view l s)) s

