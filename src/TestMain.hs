module Main where

import Test.Framework (defaultMain)

import qualified BowlingTests

main :: IO ()
main = defaultMain BowlingTests.tests
