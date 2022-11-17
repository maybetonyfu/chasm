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
  let allConstIds = L.nub $ map cstId constraints
  let mcsIds =  map (\mss -> let ids = L.nub (map cstId mss) in allConstIds \\ ids) msss
  let mcss = map (\mcs -> filter ((`elem` mcs) . cstId) constraints) mcsIds
  -- let flattenedMcsIds = L.sort (L.nub (concat mcsIds))
  -- let sortedMcsIds = L.sortBy (\a b -> compare (length (filter ((`elem` a) . cstId) constraints)) (length (filter ((`elem` b). cstId) constraints))) mcsIds
  -- logInfo (displayShow flattenedMcsIds)
  -- logInfo (displayShow sortedMcsIds)
  -- let minimalUsefulIds = minimalUsefulMCS flattenedMcsIds sortedMcsIds sortedMcsIds
  -- let mcss = map (\ids -> filter ((`elem` ids) . cstId) constraints) minimalUsefulIds
  mcsHandle <- view mcsesL
  writeIORef mcsHandle mcss

minimalUsefulMCS ::  [Int] -> [[Int]] -> [[Int]]-> [[Int]]
minimalUsefulMCS _ [] acc = acc
minimalUsefulMCS allIds (mcs:mcss) acc =
  if  L.sort (L.nub (concat (L.delete mcs acc))) == allIds -- if removing a constraints does not weaken
    then minimalUsefulMCS allIds mcss (L.delete mcs acc) -- it is safe to remove
    else minimalUsefulMCS allIds mcss acc -- it must be kept


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


islandsMCSes ::  (HasLogFunc env, HasMUSs env, HasIslands env, HasMCSs env, HasIslandsMCSes env, HasConstraints env) => RIO env ()
islandsMCSes = do
  islands <- readIORefFromLens islandsL
  islandMcssHandle <- view islandsMcsesL
  islandMcss <- forM islands islandMCS
  writeIORef islandMcssHandle islandMcss

islandMCS ::  (HasLogFunc env, HasMUSs env, HasMCSs env, HasIslandsMCSes env, HasConstraints env) => [Int] -> RIO env ([[Int]], [[Int]])
islandMCS island = do
  muses <- readIORefFromLens musesL
  mcses <- readIORefFromLens mcsesL
  constraints <- readIORefFromLens constraintsL
  let numberedMuses = zip [0..] muses
  let islandMuses = map snd (filter ((`elem` island) . fst)  numberedMuses)
  let islandConstraints = concat islandMuses
  let islandConstraintsNumbers = map cstId islandConstraints
  let mcsNumbers = (fmap . fmap) cstId mcses
  let islandMcsIds = L.nub . filter (\m -> not (null m)) . map (\mcs -> filter (`elem` islandConstraintsNumbers) . L.nub $ mcs) $ mcsNumbers
  let flattenedMcsIds = L.sort (L.nub (concat islandMcsIds))
  let sortedMcsIds = L.sortBy (\a b -> compare (length (filter ((`elem` a) . cstId) constraints)) (length (filter ((`elem` b). cstId) constraints))) islandMcsIds
  let minimalUsefulIds = minimalUsefulMCS flattenedMcsIds sortedMcsIds sortedMcsIds
  return (minimalUsefulIds, sortedMcsIds \\ minimalUsefulIds)
