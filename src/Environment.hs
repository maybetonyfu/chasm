{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Environment where
import RIO.Text as T
import RIO
import RIO.Process
import Bottle
import Range
import Types
import Lenses
import Load
import Language.Haskell.Exts

import Data.Aeson (Result(Success))

data ChasmApp = ChasmApp
  { chLogFunc :: !LogFunc
  , chProcessContext :: ProcessContext
  , chBasicInfo :: BasicInfo
  , chAST :: IORef (Maybe (Module SrcSpanInfo))
  , chLoad :: IORef [Load]
  , chBottles :: [Bottle]
  , chSlices :: [Slice]
  }

instance HasLogFunc ChasmApp where
  logFuncL = lens chLogFunc (\x y -> x { chLogFunc = y })

instance HasProcessContext ChasmApp where
  processContextL = lens chProcessContext (\x y -> x { chProcessContext = y })

instance HasBottle ChasmApp where
  bottleL = lens chBottles (\x y -> x {chBottles = y})

instance HasBasicInfo ChasmApp where
  basicInfoL = lens chBasicInfo (\x y -> x {chBasicInfo = y})

instance HasAST ChasmApp where
  astL = lens chAST (\x y -> x {chAST = y})

instance HasLoad ChasmApp where
  loadL = lens chLoad (\x y -> x {chLoad = y})

parseProgram :: (HasBasicInfo env, HasLogFunc env, HasAST env) => RIO env ()
parseProgram = do
  (rootDir, filePath) <- view basicInfoL
  let pResult = parseModuleWithMode parseMode ""
      parseMode = defaultParseMode {parseFilename = filePath}
  case pResult of
    ParseOk hModule -> do
      logInfo "Parsing Success"
      astHandle <- view astL
      writeIORef astHandle (Just hModule)
    ParseFailed srcLoc message ->
      logInfo "Parsing Failed"
  return ()

plan :: RIO ChasmApp ()
plan = do
  parseProgram
  analyzeImports
  -- loadBottles
  -- loadSlicesCurrentModule
  -- loadSlicesFromBottles
  -- constraintsFromCurrentModule
  -- constraintsFromSlices
  return ()

main :: IO ()
main = do
  lo <- logOptionsHandle stderr False
  withLogFunc lo $ \lf -> do
    processContext <- mkDefaultProcessContext
    emptyAST <- newIORef Nothing
    emptyLoad <- newIORef []
    let chApp = ChasmApp
            { chLogFunc = lf
            , chProcessContext = processContext
            , chBasicInfo = ("", "")
            , chAST = emptyAST
            , chLoad = emptyLoad
            , chBottles = []
            , chSlices = []}
    runRIO chApp plan
