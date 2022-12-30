
module Cac.Eval where

import Cac.Comp

chooseN :: Int -> [a] -> [[a]] 
chooseN 0 _  = [[]]
chooseN n [] = []
chooseN n (x:xs) = chooseN n xs ++ map (x:) (chooseN (n-1) xs)

{-
chooseN2 :: Int -> [a] -> [[a]]
chooseN2 0 xs = []
chooseN2 n (x:xs) = [x : take (n-1) xs] ++ [chooseN2 xs]
-}

count13 :: Comp -> Int
count13 (Comp notes) = error "foo"


getAllF3 :: CompF -> [Set3] 
getAllF3 (CompF fg _ _) = map Set3 $ chooseN 3 fg


getAllFM3 :: CompF -> [Set3]
getAllFM3 (CompF fg mg _) = 




-- we can observe overlapping pitch class sets: for example, we can take
-- subsets of 3 or 4 pitches, and observe how many subsets exist at a similar
-- location in the "good example." Divide good and bad into subsets of three
-- pitches and observe how many overlap. they could be divided into background
-- and foreground. similarly situated sets would be matched.

-- terminology
--
--   test instance
--
--     a test composition. grouped into "foreground", "middle ground", and
--     "background" pitches. within each set they are ordered. simultaneous
--     pitches are ordered from low to high.
--
--     for instance [60,61,40,48], [87,20,60], [34]
--
--     foreground, middle ground, and background sets are called F, M, B
--
--   3-subset
--
--     three notes taken from the test instance
--
--   F 3-subset
--
--     a 3-subset in which all notes come from foreground
--
--   FM 3-subset
--
--     a 3-subset in which some notes come from F and some from M
--
--   FB, MB 3-subsets
--
--     obvious
--
--   matched 3-subsets
--
--     when a 3-subset is exactly the same as one from a training instance
--
--   3S_u: a 3-subset from the unknown instance. 3S_t: from training instance
--
--   match strength of 3S_u and 3S_t 
--
--     depends on whether the sets are similarly situated.
--
--       the matching pitches are P1_u, P2_u, and P3_u. And P1_t, P2_t, P3_t
--
--       contributors to strength
--
--          all of (P1,P2,P3) in F, or two in same theater
--
--          Px repetitions number are similar
--
--          P1, P2, and P3 are in same or similar order. where P1 occurs, in
--          1st, 2nd or 3rd position.
--
--            (P1,P2,P3) strongest. (P2,P1,P3) less strong.
--
--          P1, P2, P3 in same octave
--
--  unmatched 3-sets. would rank them by innocuous sounding. non-innocuous
--  would degrade the ranking a lot. No unmatched sets is very strong rank.
--
--  final score depends on strength of matched 3-sets. a lot of strong matches
--  helps a lot. weak matches don't say a lot either good or bad, but perhaps
--  a lot of weak matches degrade result. unmatched sets in the unknown
--  example hurt a LOT.
--
--  this is expensive computation. multiply that by number of training
--  examples.
--          
--
--
 
    
