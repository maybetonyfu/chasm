{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Environment where

import RIO
import RIO.Process
import Bottle
import Slice
import Environment

data ChasmApp = ChasmApp
  { chLogFunc :: !LogFunc
  , chProcessContext :: ProcessContext
  , chBaseDir :: FilePath
  , chBottles :: [Bottle]
  , chSlices :: [Slice]
  }


instance HasLogFunc ChasmApp where
  logFuncL = lens chLogFunc (\x y -> x { chLogFunc = y })

instance HasProcessContext ChasmApp where
  processContextL = lens chProcessContext (\x y -> x { chProcessContext = y })

instance HasBottle ChasmApp where
  bottleL = lens chBottles (\x y -> x {chBottles = y})
