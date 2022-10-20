{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Marco where

import RIO
import RIO.List
import Types

data MarcoState   =  Marco
  { mcMap :: [[Constraint]]
  , mcSeed :: [Constraint]
  , mcMuses :: [[Constraint]]
  , mcMsses :: [[Constraint]]
  }

shrink ::  ([Constraint] -> Bool) -> [Constraint] -> [Constraint]
shrink sat [] = error "A satisifiable seed is provided to the 'shrink' function"
shrink sat seed =
  let removeOne = find (\x -> (not . sat) (delete x seed)) seed
   in case removeOne of
        Nothing -> seed
        Just x -> shrink sat (delete x seed)

grow ::  ([Constraint] -> Bool) -> [Constraint] -> [Constraint] -> [Constraint]
grow sat constraints [] = error "An unsatisfiable seed is provided to the 'grow' function"
grow sat constraints seed =
  let addOne = find (\x -> sat (x: seed)) (constraints \\ seed)
   in case addOne of
        Nothing -> seed
        Just x -> grow sat constraints (x: seed)

marco ::  ([Constraint] -> Bool) -> [Constraint] -> MarcoState  -> MarcoState
marco sat constraints m@(Marco mMap mSeed mMus mMss) =
  if all sat mMap
     then m
     else
       let seed = getUnexplored mMap
           m' = Marco mMap seed mMus mMss
       in if sat seed
            then marcoMSS sat constraints m'
            else marcoMUS sat constraints m'

marcoMUS ::  ([Constraint] -> Bool) -> [Constraint] -> MarcoState -> MarcoState
marcoMUS sat constraints m@(Marco mMap mSeed mMus mMss) =
  let mus = shrink sat mSeed
      m' = Marco (filter (\node -> null (node `intersect` mus)) mMap) mSeed (mus:mMus) mMss
  in marco sat constraints m'

marcoMSS ::  ([Constraint] -> Bool) -> [Constraint] -> MarcoState  -> MarcoState
marcoMSS  sat constraints m@(Marco mMap mSeed mMus mMss) =
  let mss = grow sat constraints mSeed
      m' = Marco (filter (\node -> all (\c -> c `elem` mss) node) mMap) mSeed mMus (mss:mMss)
  in marco sat constraints m'


getUnexplored :: [[Constraint]] -> [Constraint]
getUnexplored = undefined
