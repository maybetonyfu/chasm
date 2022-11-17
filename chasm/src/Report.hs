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
    reportMCS     :: [[Int]],
    reportIslands :: [[Int]],
    reportIslandsMCS :: [([[Int]], [[Int]])]

  }

instance ToJSON Report where
  toJSON (Report rCons rMus rMcs rIslds rIsldsMcs) =
    object [
      "constraints" .= rCons,
      "mus" .= rMus,
      -- "mss" .= rMss,
      "mcs" .= rMcs,
      "islands" .= rIslds,
      "islandsFixes" .= rIsldsMcs
           ]

report :: (HasLogFunc env, HasMUSs env, HasMSSs env, HasMCSs env, HasIslands env, HasConstraints env, HasIslandsMCSes env) => RIO env ()
report = do
  constraints <- readIORefFromLens constraintsL
  muses <- readIORefFromLens musesL
  mcses <- readIORefFromLens mcsesL
  islands <- readIORefFromLens islandsL
  islandsMCSes <- readIORefFromLens islandsMcsesL
  let rpt = Report  constraints (map (map cstId) muses) (map (map cstId) mcses) islands islandsMCSes
  let reportJson = encode rpt
  BS.putStr reportJson

