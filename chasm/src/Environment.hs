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
import RIO.Text as T
import Range
import Types
import Slice
import Typing
import Goal

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
    chConstraints :: IORef [Constraint]
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
  parseProgram -- ast
  analyzeImports -- load, targetName

  loadBottles -- bottles
  loadSlicesCurrentModule -- slices
  loadSlicesFromBottles -- slices

  constraintsFromBottles --constraints
  constraintsFromCurrentModule --constraints
  sat <- solve
  logInfo . displayShow $ sat

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
              chConstraintConter = zeroSliceCounter,
              chConstraints = emptyConstraints
            }
    runRIO chApp plan
