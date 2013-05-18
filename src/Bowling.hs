module Bowling (
    score
    ) where

-- | Given a sequence pins downed per bowl, compute the current score.
-- If the list of pins downed is too long, the excess are just ignored.
score :: [Int] -> Int
score bs = go $ zip3 bs (drop 1 bs') (drop 2 bs')
  where
    bs' = bs ++ repeat 0
    go []                           = 0
    go ((a,b,c):ts)   | a == 10     = a + b + c + go ts
    go ((a,b,c):_:ts) | a + b == 10 = a + b + c + go ts
                      | otherwise   = a + b     + go ts
    go ((a,b,c):[])                 = a + b

