{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module Report where

import           Data.Aeson
import           Lenses
import           RIO
import qualified RIO.ByteString.Lazy as BS
import           Types

data Report = Report
  {
    reportCons    :: [Constraint],
    reportMUS     :: [[Int]],
    -- reportMSS :: [[Int]],
    reportMCS     :: [[Int]],
    reportIslands :: [[Int]] }

instance ToJSON Report where
  toJSON (Report rCons rMus rMcs rIslds) =
    object [
      "constraints" .= rCons,
      "mus" .= rMus,
      -- "mss" .= rMss,
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
  let rpt = Report  constraints (map (map cstId) muses) (map (map cstId) mcses) islands
  let reportJson = encode rpt
  BS.putStr reportJson

