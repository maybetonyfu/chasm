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

data ChasmApp = ChasmApp
  { chLogFunc :: !LogFunc,
    chProcessContext :: ProcessContext,
    chBasicInfo :: BasicInfo,
    chAST :: IORef (Maybe (Module SrcSpanInfo)),
    chLoad :: IORef [Load],
    chBottles :: IORef [Bottle],
    chSliceCounter :: IORef Int,
    chSlices :: IORef [Slice]
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
  parseProgram
  analyzeImports
  loadBottles
  loadSlicesCurrentModule
  loadSlicesFromBottles
  -- constraintsFromCurrentModule
  -- constraintsFromBottles
  return ()

main :: IO ()
main = do
  lo <- logOptionsHandle stderr True
  withLogFunc lo $ \lf -> do
    processContext <- mkDefaultProcessContext
    emptyAST <- newIORef Nothing
    emptyLoad <- newIORef []
    emptyBottles <- newIORef []
    zeroSliceCounter <- newIORef 0
    emptySlices <- newIORef []
    let chApp =
          ChasmApp
            { chLogFunc = lf,
              chProcessContext = processContext,
              chBasicInfo = ("c:/Users/sfuu0016/Projects/chasm-example", "Test.hs"),
              chAST = emptyAST,
              chLoad = emptyLoad,
              chBottles = emptyBottles,
              chSliceCounter = zeroSliceCounter,
              chSlices = emptySlices
            }
    runRIO chApp plan
