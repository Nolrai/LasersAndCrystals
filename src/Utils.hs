{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fprof-auto-calls #-}

module Utils where

import Control.Lens
import Relude

showT :: Show a => a -> Text
showT = show
