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

shrink ::(HasLogFunc env, HasConstraints env) => [Constraint] -> RIO env [Constraint]
shrink [] = error "A satisifiable seed is provided to the 'shrink' function"
shrink seed = do
  let test c = do
        isUnsat <- fmap not (sat (delete c seed))
        if isUnsat then return (Just (delete c seed)) else return Nothing
  mbNextSeed <- mapM test seed
  let possibleNextSeeds = catMaybes mbNextSeed
  case possibleNextSeeds of
    [] -> return seed
    (nextSeed:_) -> shrink nextSeed

grow :: (HasLogFunc env, HasConstraints env) => [Constraint] -> [Constraint] -> RIO env [Constraint]
grow constraints seed = do
  let rest = constraints \\ seed
  let go c = do
        isSat <- sat (c:seed)
        if isSat then return (Just (c:seed)) else return Nothing
  mbNextSeed <- mapM go rest
  let possibleNextSeeds = catMaybes mbNextSeed
  case possibleNextSeeds of
    [] -> return seed
    (nextSeed:_) -> grow constraints nextSeed

runMarco :: (HasLogFunc env, HasConstraints env, HasMarcoMap env, HasMUSs env, HasMSSs env) => RIO env ()
runMarco = do
  constraints <- readIORefFromLens constraintsL
--  mapM_ (logInfo . displayShow) constraints --
  clauses <- generateClauses constraints
  let textClause = map simplifyShow clauses
  logInfo "\nClauses: "
  forM_ textClause (logInfo . display)
  logInfo "\nConstraints: "
  forM_ constraints (\c -> do
                        let num = T.pack . show . cstId $ c
                        let head = T.pack . cstHead $ c
                        let body = T.pack . show . cstBody $ c
                        logInfo . display $ (num <> ": Head: " <> head <> ", Body: " <> body)
                        )

  let constraintIds = map cstId constraints
      formulas = map Var constraintIds
      tautologies = map (\f -> f :||: Not f) formulas
  marcoMapHandle <- view marcoMapL
  writeIORef marcoMapHandle tautologies
  marco

marco :: (HasLogFunc env, HasConstraints env, HasMarcoMap env, HasMUSs env, HasMSSs env) => RIO env ()
marco = do
  marcoMap <- readIORefFromLens marcoMapL
  let marcoMapSat = satisfiable (All marcoMap)
  if not marcoMapSat
    then do
      logInfo "\nMarco map is no longer satisfiable. The search is over."
      return ()
    else do
      seed <- getUnexplored
      isSat <- sat seed
      if isSat
        then marcoMSS seed >> marco
        else marcoMUS seed >> marco

marcoMSS :: (HasLogFunc env, HasConstraints env, HasMarcoMap env, HasMSSs env) => [Constraint] -> RIO env ()
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


marcoMUS :: (HasLogFunc env, HasConstraints env, HasMarcoMap env, HasMUSs env) => [Constraint] -> RIO env ()
marcoMUS seed = do
  mus <- shrink seed
  logInfo ("\nFound MUS: " <> displayShow (map cstId mus))

  let ids = map cstId mus
  let formula = Some (map (Not . Var) ids)
  marcoMapHandle <- view marcoMapL
  modifyIORef marcoMapHandle (formula:)
  mssHandle <- view musesL
  modifyIORef mssHandle (mus:)

getUnexplored ::  (HasLogFunc env, HasConstraints env, HasMarcoMap env) => RIO env [Constraint]
getUnexplored = do
  marcoMap <- readIORefFromLens marcoMapL
  constraints <- readIORefFromLens constraintsL
  let assignments = solve (All marcoMap)
  case assignments of
    Nothing -> error "solve after satisfiable check should never fail"
    Just m -> do
      let assignmentList = M.toList m
      let ids = map fst . filter snd $ assignmentList
      return $ filter (\c -> cstId c `elem` ids) constraints
