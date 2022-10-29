{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
module Analysis where

import RIO
import qualified RIO.List as L
import Lenses
import Types

report :: (HasLogFunc env, HasMUSs env, HasMSSs env) => RIO env ()
report = do
  constraintMuses <- readIORefFromLens musesL
  let muses = zip [0..] (map (map cstId) constraintMuses)
  let edges = L.nub $ concatMap (findEdges muses) muses
  logInfo (displayShow edges)
  let islands = findIslands edges (map fst muses)
  logInfo (displayShow islands)
  return ()

findEdges :: [(Int, [Int])] -> (Int, [Int]) -> [Edge]
findEdges muses (musId, mus) =
  concatMap (\constrId ->
          let contains = filter (\(musId', mus') -> constrId `elem` mus')  muses
              excludeMe = filter (\(musId', _) -> musId' /= musId) contains
              ids = map fst excludeMe
          in map (\i -> if i < musId then (i, musId) else (musId, i)) ids
        ) mus

type Edge = (Int, Int)

findIslands :: [Edge] -> [Int] -> [[Int]]
findIslands edges nodes =
  let adjMap = map (\n ->
                      let nx = map snd . filter ((== n) . fst) $  edges
                          xn = map fst . filter ((==n) . snd) $ edges
                      in (n, nx ++ xn)
                   ) nodes
  in runDfs adjMap [] nodes []

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

-- (a -> b -> b) -> b -> t a -> b

-- [(0, [1]), (1, [0, 2]), (2, [1]), (3, [4]), (4, [3])]
    -- g.addEdge(1, 0)
    -- g.addEdge(2, 1)
    -- g.addEdge(3, 4)
