module Lib (module Lib) where

import Data.List (nub)
import System.Random (randomRIO)

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

color :: [Int]
color = [0,1,2,3,4,5]

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
navControl = [ "↑/↓ - Navigation", "↩ - Select", "⟵ - remove", "q - exit"]
notice :: [String]
notice = ["Attention: \n No duplicated colors!!!"]

-- 10 rounds of guessing state for gameScreen
-- the 2nd parameter is used for choosing current round: [0: "Not start yet", 1: "Current Round", 2:"Prior Rounds"]
initialGS :: [([Slot], Int)]
initialGS = [ ([Empty, Empty, Empty, Empty], 0)
            , ([Empty, Empty, Empty, Empty], 0)
            , ([Empty, Empty, Empty, Empty], 0)
            , ([Empty, Empty, Empty, Empty], 0)
            , ([Empty, Empty, Empty, Empty], 0)
            , ([Empty, Empty, Empty, Empty], 0)
            , ([Empty, Empty, Empty, Empty], 0)
            , ([Empty, Empty, Empty, Empty], 0)
            , ([Empty, Empty, Empty, Empty], 0)
            , ([Empty, Empty, Empty, Empty], 1)
            ]

initialRS :: [[Slot]]
initialRS = [ [Empty, Empty, Empty, Empty]
            , [Empty, Empty, Empty, Empty]
            , [Empty, Empty, Empty, Empty]
            , [Empty, Empty, Empty, Empty]
            , [Empty, Empty, Empty, Empty]
            , [Empty, Empty, Empty, Empty]
            , [Empty, Empty, Empty, Empty]
            , [Empty, Empty, Empty, Empty]
            , [Empty, Empty, Empty, Empty]
            , [Empty, Empty, Empty, Empty]
            ]

initialBoss :: ([Slot], Bool)
initialBoss = ([Empty, Empty, Empty, Empty], False)

-- S is the search space for DKAI algorithm
searchS :: [[Slot]]
searchS = map intsToSlots (filter hasNoDuplicate [[a,b,c,d]| a <- color,
                b <- color,
                c <- color,
                d <- color])

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
                        if not (hasNoDuplicate res)
                          then randomSingleGuess n
                          else return res

-- no duplicate
hasNoDuplicate :: [Int] -> Bool
hasNoDuplicate xs = length (nub xs) == length xs

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

-- DKAI algorithm
dkSearch :: [[Slot]] -> [Slot] -> [Slot]-> [[Slot]]
dkSearch [] _ _ = []
dkSearch (x:xs) base res = 
    if temp == res 
        then x : dkSearch xs base res
        else dkSearch xs base res
        where
            temp    = masterJudge base x


-- title 
s1, s2, s3, s4, s5 :: String

s1 = "    _/      _/                        _/                          _/      _/   _/                  _/   "
s2 = "   _/_/  _/_/    _/_/_/    _/_/_/  _/_/_/_/    _/_/    _/  _/_/  _/_/  _/_/       _/_/_/      _/_/_/    "
s3 = "  _/  _/  _/  _/    _/  _/_/        _/      _/_/_/_/  _/_/      _/  _/  _/   _/  _/    _/  _/    _/     "
s4 = " _/      _/  _/    _/      _/_/    _/      _/        _/        _/      _/   _/  _/    _/  _/    _/      "
s5 = "_/      _/    _/_/_/  _/_/_/        _/_/    _/_/_/  _/        _/      _/   _/  _/    _/    _/_/_/       "
                                                                               

emp :: [Slot]
emp = [Empty, Empty, Empty, Empty]                                                                      



   
                             
                             