-- Author: Xiaocong Zhang
-- ID: 1292460
-- Email: xiaocongz@student.unimelb.edu.au
-- This Project simulates a card guessing game, in which the guesser repeatedly guesses and receives feedback from questioner until he successes.


{-
    This card guessing game assumes a standard deck of 52 cards, thus no repeating cards anywhere.
Two players, one questioner who initially tells how many cards to guess, and one guesser who repeatedly guesses based on feedback from last guess, are involved.
Approaches taken:
    GameState contains all possible guesses left in this round.
Initially it contains all possible combinations, totaling 13!/(n!*(13-n)!).
Each round, it is filtered with last guess and feedback.
    To select next guess, we find such a guess with least average group size left.
We group left possiblities if they provide same feedback for our guess, we hope to get smallest average group size.
For 3,4 card games, we sample the possiblities so that computation is feasible.

-}
module Proj2 (feedback, initialGuess, nextGuess, GameState,) where

import Data.List
import Card

-- | record the state of game.
data GameState = GameState {
                            scale :: Int,                   -- ^ how many cards to guess
                            possiblities :: [[Card.Card]]   -- ^ all possible guesses left to guess
                        }
    deriving (Show)

-- | feedback returns a tuple of 5 Int, indicating
--      how many cards are correct,
--      how many ranks in the answer are above higher bound of guess,
--      how many ranks are correct,
--      how many ranks in the answer are above higher bound of guess,
--      how many suits are correct,
-- target is the answer of the game.
-- guess is compared with answer, but because of the structure of feedback tuple, target and guess aren't symmetric.
feedback ::  [Card.Card] -> [Card.Card] -> (Int,Int,Int,Int,Int)
feedback target guess = (match target guess,length $ filter (<minRankGuess) rankTarget,match rankTarget rankGuess,length $ filter (>maxRankGuess) rankTarget,match suitTarget suitGuess )
    where
        rankTarget = map rank target
        suitTarget = map suit target
        rankGuess = map rank guess
        suitGuess = map suit guess
        minRankGuess = minimum $ map rank guess
        maxRankGuess = maximum $ map rank guess

-- | Find same items in two lists, returning number of matching pairs.
-- Used to calculate correct ranks, suits and cards in feedback.
-- Two lists to match are Commutative.
match   :: Ord t
        => [t]  -- ^ one list to compare
        -> [t]  -- ^ another list to compare
        -> Int  -- ^ how many pairs of same items are in two lists
match _ [] = 0
match [] _ = 0
match t g
    | t0 == g0 = 1 + (match ts gs)
    | t0 < g0 = match ts (g0:gs)
    | t0 > g0 = match (t0:ts) gs
    where
        (t0:ts) = sort t
        (g0:gs) = sort g

-- | Take as much as scaleLimit items in a list, with uniform interval.
-- Sample the list so that we can process data in 10 seconds.
-- Choose uniform interval since list of possible guesses are sorted, making adjacent guesses similar.
sample  :: Int -- ^ upper bound of size of returned list
        -> [t] -- ^ list to sample
        -> [t] -- ^ sampled result
sample scaleLimit list
    | scaleLimit < length list = [list!!(i*interval) | i <-[1..scaleLimit] ]
    | otherwise = list
    where interval = length list `div` (scaleLimit + 1)

-- | Return suit for nth card in initial guess.
-- The strategy is to only choose two suits, Heart and Diamond without loss of generality.
initialSuit :: Int          -- ^ nth card in initial guess
            -> Card.Suit    -- ^ suit of nth card
initialSuit n
    | even n = Card.Heart
    | otherwise = Card.Diamond

-- | initialGuess generates initial guesses and initial GameState.
-- initial guess is a list of cards, with two interleaving suits and uniformly distributed ranks.
-- initial GameState contains scale of game and all possible guesses.
-- initialGuess requires the scale of the game below 13, as the implementation probably cannot handle that many possiblities
initialGuess :: Int -> ([Card.Card],GameState)
initialGuess n
    | n < 13 = (guess, GameState n initialPossibilities)
    where
        initialPossibilities = allGuess n
        interval = 13 `div` (n + 1)
        ranks = [[minBound..maxBound::Card.Rank]!!(x*interval)| x <-[1..n]] -- uniformly distributed ranks
        suits = [initialSuit x | x <- [1..n]] -- [Diamond, Heart, Diamond, Heart...]
        guess = [Card.Card (fst x) (snd x) | x <- zip suits ranks ]

-- | Generate all possible guesses for a n-card game.
allGuess :: Int -> [[Card.Card]]
allGuess 0 = []
allGuess n
    | n == 1 = map (\x -> [x]) [minBound..maxBound::Card.Card]
    | n > 1 = filter isSorted $ (++) <$> allGuess 1 <*> allGuess (n - 1)
    | otherwise = [] -- maybe abort?

-- | Check if a list is strictly ascending.
-- Used to filter repeated card compositions when generating all guesses for initial state.
isSorted :: Ord t => [t] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:xs:xss) = x < xs && isSorted (xs:xss)

-- | Generate next (guess,state) given last (guess,state) and feedback.
-- nextGuess use sampling to reduce computation for games of >2 cards to guess.
nextGuess   :: ([Card.Card],GameState)  -- ^ last guess and GameState
            -> (Int,Int,Int,Int,Int)    -- ^ last feedback
            -> ([Card.Card],GameState)  -- ^ next guess and GameState
nextGuess (preGuess,preState) feedbackTuple =  (guess, GameState n leftPossibilities)
    where
        leftPossibilities = filter (isPossible preGuess feedbackTuple) (possiblities preState)
        n = scale preState
        limits
            | n == 2 = (fst limitLeastAverageLeft, fst limitAverageLeft)
            | n > 2 = (snd limitLeastAverageLeft, snd limitAverageLeft)
        guess = snd $ leastAverageLeft leftPossibilities limits


-- | Is target possible given guess and feedback.
-- Used to filter possible answers after one round of guessing.
isPossible  :: [Card.Card]              -- ^ guess
            -> (Int,Int,Int,Int,Int)    -- ^ feedback for guess
            -> [Card.Card]              -- ^ target to test
            -> Bool                     -- ^ if the target is possible to be answer
isPossible guess feedbackTuple target = feedback target guess == feedbackTuple

-- | How many possible guesses to sample at most, as candidates for next guess.
-- 4000 for 2 card games, 400 for more cards.
limitLeastAverageLeft = (4000, 400)

-- | How many possible guesses to sample at most, for calculation of average left guesses.
-- 5000 for 2 card games, 500 for more cards.
limitAverageLeft = (5000, 500)

-- | evaluate how many possiblities will be left if choose a guess.
-- group possiblities by feedback, return average size of groups.
-- sample the possiblities for efficiency.
averageLeft :: [[Card.Card]]    -- ^ left possiblities in GameState
            -> [Card.Card]      -- ^ guess to evaluate
            -> Int              -- ^ limit of sampling
            -> Int              -- ^ average number of left possiblities next round
averageLeft possibilities guess limit = sum [length x | x <- feedbackTuples] `div` length feedbackTuples
    where
        sampledPossibilities = sample limit possibilities
        feedbackTuples = group $ sort [feedback x guess | x <- sampledPossibilities]

-- | Given possiblities left so far, evaluate only part of them(sampling), and return one that would leave least possiblities.
leastAverageLeft :: [[Card.Card]]       -- ^ left possiblities in GameState
                 -> (Int, Int)          -- ^ limit of sampling
                 -> (Int, [Card.Card])  -- ^ (average size of left possiblities groups, corrisponding guess)
leastAverageLeft possibilities limits = head $ sort guessAverageLeft
    where guessAverageLeft = [(averageLeft possibilities x (snd limits), x) |x <- sample (fst limits) possibilities]
