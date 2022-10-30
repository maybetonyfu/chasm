{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Report where

import RIO
import qualified RIO.ByteString.Lazy as BS
import Lenses
import Types
import Data.Aeson

data Report = Report
  {
    reportCons :: [Constraint],
    reportMUS :: [[Int]],
    reportMSS :: [[Int]],
    reportMCS :: [[Int]],
    reportIslands :: [[Int]] }

instance ToJSON Report where
  toJSON (Report rCons rMus rMss rMcs rIslds) =
    object [
      "constraints" .= rCons,
      "mus" .= rMus,
      "mss" .= rMss,
      "mcs" .= rMcs,
      "islands" .= rIslds
           ]

report :: (HasLogFunc env, HasMUSs env, HasMSSs env, HasMCSs env, HasIslands env, HasConstraints env) => RIO env ()
report = do
  constraints <- readIORefFromLens constraintsL
  muses <- readIORefFromLens musesL
  msses <- readIORefFromLens mssesL
  mcses <- readIORefFromLens mcsesL
  islands <- readIORefFromLens islandsL
  let rpt = Report  constraints (map (map cstId) muses) (map (map cstId) msses) (map (map cstId) mcses) islands
  let reportJson = encode rpt
  BS.putStr reportJson
  
