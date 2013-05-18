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
    , testGroup "Spares"
        [ testCase "stike" (score [7,3,8,1] @?= 27)
        , testProperty "after spares" prop_spares
        ]
    , testGroup "Strikes"
        [ testCase "stike" (score [10,8,1] @?= 28)
        , testProperty "after strikes" prop_strikes
        ]
    ]

data DullFrame = DullFrame Int Int
    deriving (Eq, Show)
instance Arbitrary DullFrame where
    arbitrary = do
        n <- choose (0,9)
        a <- choose (0,n)
        return $ DullFrame a (n - a)
    shrink (DullFrame a b) =
        [ DullFrame a' b' | a' <- [0..a], b' <- [0..b], a' + b' /= a + b ]

dullBowls :: [DullFrame] -> [Int]
dullBowls = concatMap $ \(DullFrame a b) -> [a,b]

newtype DullPin = DullPin Int
    deriving (Eq, Show)
instance Arbitrary DullPin where
    arbitrary = DullPin `fmap` choose (0,9)
    shrink (DullPin a) = DullPin `fmap` shrinkIntegral a


prop_noTens :: [DullFrame] -> Bool
prop_noTens dfs = score bowls == sum bowls
  where
    bowls = dullBowls dfs

prop_spares :: [DullFrame] -> DullPin -> DullFrame -> Bool
prop_spares dfs dp dfn = score bowls == sum (dullBowls dfs) + 10 + a + a + b
  where
    bowls = dullBowls dfs ++ [c, 10 - c, a, b]
    DullPin c = dp
    DullFrame a b = dfn

prop_strikes :: [DullFrame] -> DullFrame -> Bool
prop_strikes dfs dfn = score bowls == sum (dullBowls dfs) + 10 + a + b + a + b
  where
    bowls = dullBowls dfs ++ [10, a, b]
    DullFrame a b = dfn

