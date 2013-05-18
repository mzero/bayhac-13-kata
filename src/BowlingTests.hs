module BowlingTests where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@?=))
import Test.QuickCheck

import Bowling

tests :: [Test]
tests =
    [ testGroup "Simple Scores"
        [ testCase "zero" (score [] @?= 0)
        , testCase "some" (score [1,2,3,4] @?= 10)
        ]
    ]
