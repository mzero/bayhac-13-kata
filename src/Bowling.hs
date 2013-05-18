module Bowling (
    score,
    scoreSheet
    ) where

import Control.Applicative ((<$>))
import Control.Monad (liftM2)
import Data.List (tails)

-- | Given a sequence pins downed per bowl, compute the current score.
-- If the list of pins downed is too long, the excess are just ignored.
score :: [Int] -> Int
score bs = go (1::Int) $ zip3 bs (drop 1 bs') (drop 2 bs')
  where
    bs' = bs ++ repeat 0
    go n _              | n > 10      = 0
    go _ []                           = 0
    go n ((a,b,c):ts)   | a == 10     = a + b + c + go (n + 1) ts
    go n ((a,b,c):_:ts) | a + b == 10 = a + b + c + go (n + 1) ts
                        | otherwise   = a + b     + go (n + 1) ts
    go _ ((a,b,_b):[])                 = a + b


type Pins = Int

data Frame = Blank
           | Incomplete Pins
           | Frame Pins Pins
           | Spare Pins
           | Strike
           | Extra Pins

newtype Frames = Frames { framesList :: [Frame] }

frames :: [Int] -> Frames
frames = Frames . go (1 :: Int)
  where
    go n bs | n > 10 = take 2 $ map Extra bs
    go n []          = take (11 - n) $ repeat Blank

    go n (10:bs)                = Strike       : go (n - 1) bs
    go n (a:b:bs) | a + b == 10 = Spare a      : go (n + 1) bs
                  | otherwise   = Frame a b    : go (n + 1) bs
    go n (a:[])                 = Incomplete a : go (n + 1) []

frameScores :: Frames -> [Maybe Int]
frameScores = take 10 . map scoreOne . tails . framesList
  where
    scoreOne (Frame a b : _) = Just $ a + b
    scoreOne (Spare _ : fs)  = (10 +) <$> nextOne fs
    scoreOne (Strike : fs)   = (10 +) <$> nextTwo fs
    scoreOne _               = Nothing

    nextOne [] = Nothing
    nextOne (Blank : _) = Nothing
    nextOne (Incomplete a : _) = Just a
    nextOne (Frame a _ : _) = Just a
    nextOne (Spare a : _) = Just a
    nextOne (Strike : _) = Just 10
    nextOne (Extra a : _) = Just a

    nextTwo [] = Nothing
    nextTwo (Blank : _) = Nothing
    nextTwo (Incomplete _ : _) = Nothing
    nextTwo (Frame a b : _) = Just $ a + b
    nextTwo (Spare _ : _) = Just 10
    nextTwo (Strike : fs) = (10 +) <$> nextOne fs
    nextTwo (Extra a : fs) = (a +) <$> nextOne fs




scoreSheet :: [Int] -> String
scoreSheet bs = unlines [divider, frameLine, midDivider, scoreLine, divider]
  where
    fs = frames bs
    divider    = '+' : concat (replicate 10 "------+")
    midDivider = '|' : concat (replicate 10 "  +---|")
    frameLine = '|' : concatMap frameBox (take 10 $ framesList fs)

    frameBox Blank = "  | , |"
    frameBox (Incomplete a) = "  |" ++ show a ++ ", |"
    frameBox (Frame a b) = "  |" ++ show a ++ ',' : show b ++ "|"
    frameBox (Spare a) = "  |" ++ show a ++ ",/|"
    frameBox Strike = "  |X, |"
    frameBox _ = "  | , |"

    scoreLine = '|' : concatMap scoreBox (scanl1 (liftM2 (+)) $ frameScores fs)

    scoreBox Nothing = "      |"
    scoreBox (Just x) | x < 10    = "  " ++ show x ++ "   |"
    scoreBox (Just x) | x < 100   = " " ++ show x ++ "   |"
    scoreBox (Just x) | otherwise = show x ++ "   |"

