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
    , testGroup "Incomplete"
        [ testCase "incomplete frame" (score [1, 2, 3] @?= 6)
        , testCase "incomplete spare" (score [7, 3] @?= 10)
        , testCase "incomplete strike" (score [10] @?= 10)
        , testCase "semicomplete strike" (score [10, 3] @?= 16)
        ]
    , testGroup "Limit to 10 frames"
        [ testCase "chop at frame 10" (score [1,1,1,2,1,3,1,4,1,5,2,1,2,2,2,3,2,4,2,5,3,1,3,2] @?= 45)
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
prop_noTens dfs = score bowls == sum bowls'
  where
    bowls = dullBowls dfs
    bowls' = dullBowls $ take 10 dfs

prop_spares :: [DullFrame] -> DullPin -> DullFrame -> Bool
prop_spares dfs dp dfn = score bowls == sum (dullBowls dfs') + 10 + a + a + b
  where
    dfs' = take 8 dfs
    bowls = dullBowls dfs' ++ [c, 10 - c, a, b]
    DullPin c = dp
    DullFrame a b = dfn

prop_strikes :: [DullFrame] -> DullFrame -> Bool
prop_strikes dfs dfn = score bowls == sum (dullBowls dfs') + 10 + a + b + a + b
  where
    dfs' = take 8 dfs
    bowls = dullBowls dfs' ++ [10, a, b]
    DullFrame a b = dfn

