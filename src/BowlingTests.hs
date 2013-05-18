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
    , testGroup "Frame scores"
        [ testCase "ex1" (score2 [5,3] @?= [8])
        , testCase "ex2" (score2 [5,3,6,4,4,2] @?= [8,22,28])
        , testCase "ex3" (score2 [5,3,10,4,2] @?= [8,24,30])

        , testCase "d1" (score2 [9,0,9,1,8,0,7,0,9,0,10,6,3] @?= [9,27,35,42,51,70,79])
        , testCase "d2" (score2 [0,1,7,1,9,0,9,1,8,2,8,2,6,4] @?= [1,9,18,36,54,70])
        , testCase "d3" (score2 [10,9,0,3,6,6,4,1,6,7,3] @?= [19,28,37,48,55])
        , testCase "d4" (score2 [1,5,8,2,3,6,7,0,1,4,8,1] @?= [6,19,28,35,40,49])
        , testCase "d5" (score2 [8,1,0,0,7,0,9,0,0,9,0,6] @?= [9,9,16,25,34,40])

        , testCase "ex4" (score2 [6,1,9,0,8,2,5,5,8,0,6,2,9,1,7,2,8,2,9,1]
                                    @?= [7,16,31,49,57,65,82,91,110])
        , testCase "ex4" (score2 [6,1,9,0,8,2,5,5,8,0,6,2,9,1,7,2,8,2,9,1,7]
                                    @?= [7,16,31,49,57,65,82,91,110,127])
        ]

    , testGroup "Frame display"
        [ testCase "d0" (frameDisplay2 [] @?= "")
        , testCase "d1" (frameDisplay2 [5] @?= "5")
        , testCase "d2" (frameDisplay2 [5,3] @?= "53")
        , testCase "d3" (frameDisplay2 [5,0] @?= "5-")
        , testCase "d4" (frameDisplay2 [5,5] @?= "5/")
        , testCase "d5" (frameDisplay2 [10] @?= "X ")
        , testCase "d6" (frameDisplay2 [0,7] @?= "-7")
        , testCase "d7" (frameDisplay2 [0,0] @?= "--")
        , testCase "d8" (frameDisplay2 [1,2,10,3,4] @?= "12X 34")
        , testCase "d9" (frameDisplay2 [1,0,2,0,3,0,4,0,5,0,6,0,7,0,8,0,9,0,10,1,2] @?= "1-2-3-4-5-6-7-8-9-X12")
        , testCase "d10" (frameDisplay2 [1,0,2,0,3,0,4,0,5,0,6,0,7,0,8,0,9,0,10,10,10] @?= "1-2-3-4-5-6-7-8-9-XXX")
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

