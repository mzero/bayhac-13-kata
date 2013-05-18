module Bowling (
    score
    ) where

-- | Given a sequence pins downed per bowl, compute the current score.
-- If the list of pins downed is too long, the excess are just ignored.
score :: [Int] -> Int
score bs = go 1 $ zip3 bs (drop 1 bs') (drop 2 bs')
  where
    bs' = bs ++ repeat 0
    go n _              | n > 10      = 0
    go _ []                           = 0
    go n ((a,b,c):ts)   | a == 10     = a + b + c + go (n + 1) ts
    go n ((a,b,c):_:ts) | a + b == 10 = a + b + c + go (n + 1) ts
                        | otherwise   = a + b     + go (n + 1) ts
    go _ ((a,b,c):[])                 = a + b

