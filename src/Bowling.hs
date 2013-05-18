module Bowling (
    score
    ) where


-- | Given a sequence pins downed per bowl, compute the current score.
-- If the list of pins downed is too long, the excess are just ignored.
score :: [Int] -> Int
score = sum

