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
        , testProperty "gutters" prop_noTens
        ]
    ]

data DullFrame = DullFrame Int Int
    deriving (Show)
instance Arbitrary DullFrame where
    arbitrary = do
        n <- choose (0,9)
        a <- choose (0,n)
        return $ DullFrame a (n - a)
    shrink (DullFrame a b) =
        [ DullFrame a' b' | a' <- [0..a], b' <- [0..b], a' + b' /= a + b ]

prop_noTens :: [DullFrame] -> Bool
prop_noTens dfs = score frameBowls == sum frameBowls
  where
    frameBowls = concatMap (\(DullFrame a b) -> [a,b]) dfs

