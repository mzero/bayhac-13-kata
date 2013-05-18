module Bowling (
    score,
    scoreSheet,

    score2,
    frameDisplay2,
    ) where

import Control.Applicative ((<$>))
import Data.List (inits, tails)
import Data.Maybe (mapMaybe)

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
    go _ ((a,b,_b):[])                = a + b


type Pin = Int

-- | A frame of bowling.
data Frame = Half Pin
           | Frame Pin Pin
           | Spare Pin
           | Strike
           | Extra Pin

-- | Frames beyond 10 are just handled normally
frames :: [Pin] -> [Frame]
frames = go (1 :: Int)
  where
    go _ []          = []
    go n bs | n > 10 = map Extra bs

    go n (10:bs)                = Strike       : go (n - 1) bs
    go n (a:b:bs) | a + b == 10 = Spare a      : go (n - 1) bs
                  | otherwise   = Frame a b    : go (n - 1) bs
    go n (a:[])                 = Half a       : go (n - 1) []

pins :: [Frame] -> [Pin]
pins = concatMap p
  where
    p (Half a) = [a]
    p (Frame a b) = [a, b]
    p (Spare a) = [a, 10 - a]
    p (Strike) = [10]
    p (Extra a) = [a]

frameScores :: [Frame] -> [Int]
frameScores = mapMaybe scoreOne . tails
  where
    scoreOne (Frame a b : _) = Just $ a + b
    scoreOne (Spare _ : fs)  = (10 +) <$> next 1 fs
    scoreOne (Strike : fs)   = (10 +) <$> next 2 fs
    scoreOne _               = Nothing

    next :: Int -> [Frame] -> Maybe Int
    next n = next' n . pins
    next' 0 _ = Just 0
    next' _ [] = Nothing
    next' n (p:ps) = (p +) <$> next' (n - 1) ps

frameDisplay :: [Frame] -> String
frameDisplay = concat . zipWith fd [(1::Int)..]
  where
    fd _ (Half a) = pd a
    fd _ (Frame a b) = pd a ++ pd b
    fd _ (Spare a) = pd a ++ "/"
    fd n (Strike) | n < 10 = "X "
                  | otherwise = "X"
    fd _ (Extra a) = pd a

    pd 0 = "-"
    pd n = show n

score2 :: [Int] -> [Int]
score2 = cummulativeSum . frameScores . frames

frameDisplay2 :: [Int] -> String
frameDisplay2 = frameDisplay . frames

cummulativeSum :: (Num a) => [a] -> [a]
cummulativeSum = drop 1 . map sum . inits


scoreSheet :: [Int] -> String
scoreSheet bs = unlines [divider, frameLine, midDivider, scoreLine, divider]
  where
    fs = frames bs
    divider    = '+' : concat (replicate 10 "------+")
    midDivider = '|' : concat (replicate 10 "  +---|")
    frameLine = '|' : concatMap frameBox (take 10 fs)

    frameBox (Half a) = "  |" ++ show a ++ ", |"
    frameBox (Frame a b) = "  |" ++ show a ++ ',' : show b ++ "|"
    frameBox (Spare a) = "  |" ++ show a ++ ",/|"
    frameBox Strike = "  |X, |"
    frameBox _ = "  | , |"

    scoreLine = '|' : concatMap scoreBox (take 10 $ map Just (cummulativeSum $ frameScores fs) ++ repeat Nothing)

    scoreBox Nothing = "      |"
    scoreBox (Just x) | x < 10    = "  " ++ show x ++ "   |"
    scoreBox (Just x) | x < 100   = " " ++ show x ++ "   |"
    scoreBox (Just x) | otherwise = show x ++ "   |"

