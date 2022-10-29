{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Environment where

import Bottle
import Data.Aeson (Result (Success))
import Language.Haskell.Exts
import Lenses
import Load
import RIO
import RIO.FilePath
import RIO.Process
import qualified RIO.Text as T
import Range
import Types
import Slice
import Typing
import Goal
import Marco
import Analysis
import SAT.MiniSat (Formula)

data ChasmApp = ChasmApp
  { chLogFunc :: !LogFunc,
    chProcessContext :: ProcessContext,
    chBasicInfo :: BasicInfo,
    chTargetName :: IORef Text,
    chAST :: IORef (Maybe (Module SrcSpanInfo)),
    chLoad :: IORef [Load],
    chBottles :: IORef [Bottle],
    chSliceCounter :: IORef Int,
    chSlices :: IORef [Slice],
    chTypeVarCounter :: IORef Int,
    chConstraintConter :: IORef Int,
    chConstraints :: IORef [Constraint],
    chMarcoSeed :: IORef [Constraint],
    chMarcoMap :: IORef [Formula Int],
    chMUSes :: IORef [[Constraint]],
    chMSSes :: IORef [[Constraint]]
  }

instance HasLogFunc ChasmApp where
  logFuncL = lens chLogFunc (\x y -> x {chLogFunc = y})

instance HasProcessContext ChasmApp where
  processContextL = lens chProcessContext (\x y -> x {chProcessContext = y})

instance HasBottle ChasmApp where
  bottleL = lens chBottles (\x y -> x {chBottles = y})

instance HasBasicInfo ChasmApp where
  basicInfoL = lens chBasicInfo (\x y -> x {chBasicInfo = y})

instance HasAST ChasmApp where
  astL = lens chAST (\x y -> x {chAST = y})

instance HasLoad ChasmApp where
  loadL = lens chLoad (\x y -> x {chLoad = y})

instance HasSlices ChasmApp where
  slicesL = lens chSlices (\x y -> x {chSlices = y})

instance HasSliceCounter ChasmApp where
  sliceCounterL = lens chSliceCounter (\x y -> x {chSliceCounter = y})

instance HasTargetName ChasmApp where
  targetNameL = lens chTargetName (\x y -> x {chTargetName = y})

instance HasTypeVarCounter ChasmApp where
  typeVarCounterL = lens chTypeVarCounter (\x y -> x {chTypeVarCounter = y})

instance HasConstraintCounter ChasmApp where
  constraintCounterL = lens chConstraintConter (\x y -> x {chConstraintConter = y})

instance HasConstraints ChasmApp where
  constraintsL = lens chConstraints (\x y -> x {chConstraints = y})

instance HasMarcoSeed ChasmApp where
  marcoSeedL = lens chMarcoSeed (\x y -> x {chMarcoSeed = y})

instance HasMarcoMap ChasmApp where
  marcoMapL = lens chMarcoMap (\x y -> x {chMarcoMap = y})

instance HasMUSs ChasmApp where
  musesL = lens chMUSes (\x y -> x {chMUSes = y})

instance HasMSSs ChasmApp where
  mssesL = lens chMSSes (\x y -> x {chMSSes = y})

parseProgram :: (HasBasicInfo env, HasLogFunc env, HasAST env) => RIO env ()
parseProgram = do
  logDebug "Stage: Parsing program"
  (rootDir, filePath) <- view basicInfoL
  fileContent <- readFileUtf8 (normalise rootDir </> normalise filePath)
  logDebug (display fileContent)
  let pResult = parseModuleWithMode parseMode (T.unpack fileContent)
      parseMode = defaultParseMode {parseFilename = filePath}
  case pResult of
    ParseOk hModule -> do
      logInfo "Parsing Success"
      astHandle <- view astL
      writeIORef astHandle (Just hModule)
    ParseFailed srcLoc message ->
      error "Parsing Failed"

plan :: RIO ChasmApp ()
plan = do
  -----Preperation Phases----------------------
  parseProgram -- ast
  analyzeImports -- load, targetName

  loadBottles -- bottles
  loadSlicesCurrentModule -- slices
  loadSlicesFromBottles -- slices

  constraintsFromBottles --constraints
  constraintsFromCurrentModule --constraints
  ------Analysis Phases------------------------
  isWellTyped <- wellTyped
  if isWellTyped
    then logInfo "Program is well typed"
    else do
      runMarco
      report
      

  ---------------------------------------------

main :: IO ()
main = do
  lo <- logOptionsHandle stderr False
  withLogFunc lo $ \lf -> do
    processContext <- mkDefaultProcessContext
    emptyAST <- newIORef Nothing
    emptyLoad <- newIORef []
    emptyBottles <- newIORef []
    zeroSliceCounter <- newIORef 0
    zeroTypeVarCounter <- newIORef 0
    emptySlices <- newIORef []
    fallbackTargetName <- newIORef "Main"
    zeroConstraintConter <- newIORef 0
    emptyConstraints <- newIORef []
    emptyMarcoSeed <- newIORef []
    emptyMarcoMap <- newIORef []
    emptyMUSes <- newIORef []
    emptyMSSes <- newIORef []
    let chApp =
          ChasmApp
            { chLogFunc = lf,
              chProcessContext = processContext,
              chBasicInfo = ("c:/Users/sfuu0016/Projects/chasm-example", "Test.hs"),
              chTargetName = fallbackTargetName,
              chAST = emptyAST,
              chLoad = emptyLoad,
              chBottles = emptyBottles,
              chSliceCounter = zeroSliceCounter,
              chSlices = emptySlices,
              chTypeVarCounter = zeroTypeVarCounter,
              chConstraintConter = zeroConstraintConter,
              chConstraints = emptyConstraints,
              chMarcoSeed = emptyMarcoSeed,
              chMarcoMap = emptyMarcoMap,
              chMUSes = emptyMUSes,
              chMSSes = emptyMSSes
            }
    runRIO chApp plan
