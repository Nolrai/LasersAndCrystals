{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fprof-auto-calls #-}

module Utils where

import Control.Lens
import Relude
import G

showT :: Show a => a -> Text
showT = show

class Drawable a where
  draw :: a -> Picture
