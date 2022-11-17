{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Environment        hiding (main)
import           RIO
import           RIO.Process
import           System.Environment


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> error "two arguments are needed: basedir and filepath"
    [x] -> error "two arguments are needed: basedir and filepath"
    (basedir:filepath:_) -> do
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
        emptyMCSes <- newIORef []
        emptyIslands <- newIORef []
        emptyIslandsMCSes <- newIORef []
        let chApp =
              ChasmApp
                { chLogFunc = lf,
                  chProcessContext = processContext,
                  chBasicInfo = (basedir, filepath),
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
                  chMSSes = emptyMSSes,
                  chMCSes = emptyMCSes,
                  chIslands = emptyIslands,
                  chIslandsMCSes = emptyIslandsMCSes
                }
        runRIO chApp plan

