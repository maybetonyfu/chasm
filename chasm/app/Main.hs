{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import RIO

main :: IO ()
main = runSimpleApp $ do
    logInfo "Hello, Tony"
