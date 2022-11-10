{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Marco where

import RIO
import RIO.List
import Types
import Goal
import Lenses
import SAT.MiniSat
import qualified RIO.Map as M
import qualified RIO.Text as T
data MarcoState = Marco
  { mcMap :: [[Constraint]],
    mcSeed :: [Constraint],
    mcMuses :: [[Constraint]],
    mcMsses :: [[Constraint]]
  }


shrink ::(HasLogFunc env, HasConstraints env) => [Constraint] -> [Int] -> RIO env [Constraint]
shrink constraints [] = error "A satisifiable seed is provided to the 'shrink' function"
shrink constraints seed' = do
  let seed = filter ((`elem` seed').  cstId) constraints
  let test c = do
        isUnsat <- fmap not (sat (delete c seed))
        if isUnsat then return (Just (delete c seed)) else return Nothing
  mbNextSeed <- mapM test seed
  let possibleNextSeeds = catMaybes mbNextSeed
  case possibleNextSeeds of
    [] -> return seed
    (nextSeed:_) -> shrink constraints (map cstId nextSeed)

grow :: (HasLogFunc env, HasConstraints env) => [Constraint] -> [Int] -> RIO env [Constraint]
grow constraints seed' = do
  let seed = filter ((`elem` seed').  cstId) constraints
  let rest = filter ((`notElem` seed').  cstId) constraints
  let go c = do
        isSat <- sat (c:seed)
        if isSat then return (Just (c:seed)) else return Nothing
  mbNextSeed <- mapM go rest
  let possibleNextSeeds = catMaybes mbNextSeed
  case possibleNextSeeds of
    [] -> return seed
    (nextSeed:_) -> grow constraints (map cstId nextSeed)

runMarco :: (HasLogFunc env, HasConstraints env, HasMarcoMap env, HasMUSs env, HasMSSs env) => RIO env ()
runMarco = do
  constraints <- readIORefFromLens constraintsL
--  mapM_ (logInfo . displayShow) constraints --
  clauses <- generateClauses constraints
  let textClause = map simplifyShow clauses
  -- logInfo "\nClauses: "
  -- forM_ textClause (logInfo . display)
  -- logInfo "\nConstraints: "
  -- forM_ constraints (\c -> do
  --                       let num = T.pack . show . cstId $ c
  --                       let head = T.pack . cstHead $ c
  --                       let body = T.pack . show . cstBody $ c
  --                       logInfo . display $ (num <> ": Head: " <> head <> ", Body: " <> body)
  --                       )

  let constraintIds = map cstId constraints
      formulas = map Var constraintIds
      tautologies = map (\f -> f :||: Not f) formulas
  marcoMapHandle <- view marcoMapL
  writeIORef marcoMapHandle tautologies
  marco

marco :: (HasLogFunc env, HasConstraints env, HasMarcoMap env, HasMUSs env, HasMSSs env) => RIO env ()
marco = do
  marcoMap <- readIORefFromLens marcoMapL
  constraints <- readIORefFromLens constraintsL
  let marcoMapSat = satisfiable (All marcoMap)
  if not marcoMapSat
    then do
      -- logInfo "\nMarco map is no longer satisfiable. The search is over."
      return ()
    else do
      seed <- getUnexplored
      isSat <- sat (filter ((`elem` seed) . cstId) constraints)
      if isSat
        then marcoMSS seed >> marco
        else marcoMUS seed >> marco

marcoMSS :: (HasLogFunc env, HasConstraints env, HasMarcoMap env, HasMSSs env) => [Int] -> RIO env ()
marcoMSS seed = do
  constraints <- readIORefFromLens constraintsL
  mss <- grow constraints seed
  -- logInfo "\nFound MSS:"
  -- logInfo . displayShow . map cstId $ mss

  let inverseConstraints = filter (`notElem` mss) constraints
  let inverseIds = map cstId inverseConstraints
  let formula = Some (map Var inverseIds)

  marcoMapHandle <- view marcoMapL
  modifyIORef marcoMapHandle (formula:)
  mssHandle <- view mssesL
  modifyIORef mssHandle (mss:)


marcoMUS :: (HasLogFunc env, HasConstraints env, HasMarcoMap env, HasMUSs env) => [Int] -> RIO env ()
marcoMUS seed = do
  constraints <- readIORefFromLens constraintsL
  mus <- shrink constraints seed
  -- logInfo "\nFound MUS:"
  -- logInfo . displayShow . map cstId $ mus

  let ids = map cstId mus
  let formula = Some (map (Not . Var) ids)
  marcoMapHandle <- view marcoMapL
  modifyIORef marcoMapHandle (formula:)

  musHandle <- view musesL
  modifyIORef musHandle (mus:)

getUnexplored ::  (HasLogFunc env, HasConstraints env, HasMarcoMap env) => RIO env [Int]
getUnexplored = do
  marcoMap <- readIORefFromLens marcoMapL
  constraints <- readIORefFromLens constraintsL
  if null marcoMap
    then return (nub (map cstId constraints))
    else do
      let assignments = take 10 (solve_all (All marcoMap))
      let countTruthy x = length (filter (==True) (M.elems x))
      let compareTruthy a b = compare (countTruthy a) (countTruthy b)
      let preferTruthyAssignm = maximumByMaybe compareTruthy assignments
      case preferTruthyAssignm of
        Nothing -> error "solve after satisfiable check should never fail"
        Just m -> do
          let assignmentList = M.toList m
          let ids = map fst . filter snd $ assignmentList
          return ids

