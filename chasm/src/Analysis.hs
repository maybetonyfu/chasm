{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}

module Analysis where

import RIO
import qualified RIO.List as L
import qualified RIO.Map as M
import RIO.List ((\\))
import Lenses
import Types

generateMCSs ::  (HasLogFunc env, HasConstraints env, HasMSSs env, HasMCSs env) => RIO env ()
generateMCSs = do
  msss <- readIORefFromLens mssesL
  constraints <- readIORefFromLens constraintsL
  let mcss = map (constraints \\)  msss
  mcsHandle <- view mcsesL
  writeIORef mcsHandle mcss

searchIslands :: (HasLogFunc env, HasMUSs env, HasIslands env) => RIO env ()
searchIslands = do
  constraintMuses <- readIORefFromLens musesL
  let muses = zip [0..] (map (map cstId) constraintMuses)
  let adjMap = foldr (buildAdjMap muses) M.empty muses
  let islands = runDfs (M.toList adjMap) [] (map fst muses) []
  islandsHandle <- view islandsL
  writeIORef islandsHandle islands

buildAdjMap :: [(Int, [Int])]-> (Int, [Int]) -> M.Map Int [Int]  -> M.Map Int [Int]
buildAdjMap muses (musId, mus) adjMap =
  let adjs =
       concatMap (\constrId ->
                    let contains = filter (\(musId', mus') -> constrId `elem` mus')  muses
                        excludeMe = filter (\(musId', _) -> musId' /= musId) contains
                    in map fst excludeMe ) mus
  in M.insert musId (L.nub adjs) adjMap

runDfs :: [(Int, [Int])] -> [Int] -> [Int] -> [[Int]] -> [[Int]]
runDfs adjMap visited [] result = result
runDfs adjMap visited unvisited@(n:ns) result =
  let (visited', unvisited') = dfs adjMap visited unvisited n
  in runDfs adjMap [] unvisited' (visited':result)

dfs :: [(Int, [Int])] ->[Int] ->[Int] -> Int -> ([Int], [Int])
dfs adjMap visited [] _ = (visited, [])
dfs adjMap visited unvisited node =
  if node `elem` visited
      then (visited, unvisited)
      else
         let adjs = maybe [] snd (L.find ((== node) . fst) adjMap)
         in foldr (\n (visited, unvisited) -> dfs adjMap visited unvisited n) (node:visited, L.delete node unvisited) adjs
