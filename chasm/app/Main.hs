{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import RIO
import Environment hiding (main)
import RIO.Process

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
    emptyMCSes <- newIORef []
    emptyIslands <- newIORef []
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
              chMSSes = emptyMSSes,
              chMCSes = emptyMCSes,
              chIslands = emptyIslands
            }
    runRIO chApp plan
