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
testGuess = [green, green, white, yellow]

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