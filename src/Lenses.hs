{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lenses where

import RIO
import Bottle (Bottle)

class HasBottle a where
  bottleL :: Lens' a [Bottle]

