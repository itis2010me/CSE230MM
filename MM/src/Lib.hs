module Lib where

import Data.List
import System.Random

data Slot
  = Empty
  | Guess PinColor
  deriving (Show, Eq)

data PinColor
  = Red
  | Blue
  | Green
  | White
  | Purple
  | Yellow
  deriving (Eq)

instance Show PinColor where
  show Red    = "R"
  show Blue   = "B"
  show Green  = "G"
  show White  = "W"
  show Purple = "P"
  show Yellow = "Y"

red :: Slot
red = Guess Red
green :: Slot
green = Guess Green
blue :: Slot
blue = Guess Blue
yellow :: Slot
yellow = Guess Yellow
white :: Slot
white = Guess White
purple :: Slot
purple = Guess Purple

testSol :: [Slot]
testSol = [green, white, red, blue]

testGuess :: [Slot]
testGuess = [green, white, blue, red]

success :: [Slot]
success = [red, red, red, red]

controlT :: [String]
controlT = [ "r - red", "b - blue", "g - green", "w - white"
            , "p - purple", "y - yellow"]
navControl :: [String]
navControl = [ "↑/↓ - Navigation", "↩ - Select", "q - exit"]

-- integer to Slot
zTs :: Int -> Slot
zTs x = case x of
          0 -> red
          1 -> green
          2 -> blue
          3 -> yellow
          4 -> white
          _ -> purple

intsToSlots :: [Int] -> [Slot]
intsToSlots = map zTs

-- random list of numbers between 0-5
randomGuess :: Int -> IO [Int]
randomGuess n = sequence $ replicate n $ randomRIO (0,5::Int)

randomSingleGuess :: Int -> IO [Int]
randomSingleGuess n = do
                        res <- randomGuess n
                        if hasDuplicate res
                          then randomSingleGuess n
                          else return res

hasDuplicate :: [Int] -> Bool
hasDuplicate xs = length (nub xs) /= length xs

--        sol       guess                         output    output
judge :: [Slot] -> [Slot] -> [Slot] -> [Slot] -> [Slot] -> [Slot]
judge _ [] ns ng out = judgeW ns ng out
judge (s:sols) (g:gs) ns ng out =
  if s == g
    then judge sols gs ns ng (out ++ [red])
    else judge sols gs (ns ++ [s]) (ng ++ [g]) out
judge _ _ _ _ _ = [Empty]

--         sol       guess     output   output
judgeW :: [Slot] -> [Slot] -> [Slot] -> [Slot]
judgeW _ [] out = out
judgeW solution (g:gs) out =
  let (newS, b) = check solution g ([], False) in
    if b
      then
        judgeW newS gs (out ++ [white])
      else
        judgeW solution gs out

--        sol     one guess
check :: [Slot] -> Slot -> ([Slot], Bool) -> ([Slot], Bool)
check [] _ res = res
check (s:sol) g (slots, _) =
  if s == g
    then check [] g (slots ++ sol, True)
    else check sol g (slots ++ [s], False)

masterJudge :: [Slot] -> [Slot] -> [Slot]
masterJudge s g =
  if (emptyLength > 0)
  then res ++ (replicate emptyLength Empty)
  else res
    where res         = judge s g [] [] []
          emptyLength = 4 - (length res)