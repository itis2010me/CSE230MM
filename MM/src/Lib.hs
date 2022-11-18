module Lib where

data Slot
  = Empty
  | Guess PinColor
  deriving (Show, Eq)

data PinColor
  = Red
  | Blue
  | Green
  | White
  | Black
  | Yellow
  deriving (Eq)

instance Show PinColor where
  show Red    = "R"
  show Blue   = "B"
  show Green  = "G"
  show White  = "W"
  show Black  = "Bl"
  show Yellow = "y"

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

testSol :: [Slot]
testSol = [green, white, red, blue]

testGuess :: [Slot]
testGuess = [green, green, white, yellow]

--        sol       guess                          output      output
judge :: [Slot] -> [Slot] -> [Slot] -> [Slot] -> [String] -> [String]
judge _ [] ns ng out = judgeW ns ng out
judge (s:sols) (g:gs) ns ng out = 
  if s == g 
    then judge sols gs ns ng (out ++ ["R"]) 
    else judge sols gs (ns ++ [s]) (ng ++ [g]) out
judge _ _ _ _ _ = ["Error"]

judgeW :: [Slot] -> [Slot] -> [String] -> [String]
judgeW _ [] out = out
judgeW solution (g:gs) out =
  let (newS, b) = check solution g ([], False) in
    if b
      then
        judgeW newS gs (out ++ ["W"])
      else
        judgeW solution gs out

check :: [Slot] -> Slot -> ([Slot], Bool) -> ([Slot], Bool)
check [] _ res = res
check (s:sol) g (slots, _) =
  if s == g
    then check [] g (slots ++ sol, True)
    else check sol g (slots ++ [s], False)

masterJudge :: [Slot] -> [Slot] -> [String]
masterJudge s g = judge s g [] [] []