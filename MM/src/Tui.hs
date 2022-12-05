{-# LANGUAGE OverloadedStrings #-}
module Tui (module Tui) where

import Lib

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Graphics.Vty.Input.Events
import qualified Graphics.Vty as V
import Brick.Util
-- import Brick
-- import Brick (Widget, simpleMain, (<+>), str, withBorderStyle, joinBorders, emptyWidget, vBox, setAvailableSize, padTopBottom, withAttr)
-- import Brick.Widgets.Center (center)
-- import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Data.List (nub)
import qualified Brick.Widgets.Border as B

tui :: IO ()
tui = do
  initialState <- buildInitialState
  _ <- defaultMain tuiApp initialState
  print ("Done." :: String)

-- MM State
data TuiState =
  TuiState
  {   homeScreen     :: [String]         -- there are 3 modes for homeScreen, ["1 Player", "2 Players", "DKAI vs player"]
    , navSelect      :: Int              -- index for homeScreen to choose different modes, [0: "1 Player", 1: "2 Players", 2: "DKAI vs player"]
    , screen         :: Int              -- 0: userInput, 1: homeScreen, 2: gameScreen, 3: AI game, 4: userInput for AI game
    , gameState      :: [([Slot], Int)]  -- 10 rounds of guessing state for gameScreen
    , gameStateIndex :: (Int, Int)       -- current input pointer, rowIndex and colIndex
    , pinSlots       :: [[Slot]]         -- used for showing each rounds result, each round has 4 elements which can be: ["Empty": both color & position false, "Red": both color & position true, "White": color true & position false]
    , boss           :: ([Slot], Bool)   -- guess solution for player to guess
    , random         :: [Slot]           -- randomly generated colors, used in 1 player mode
    , aiSearchSpace  :: [[Slot]]         -- DKAI algorithm's solution search space
  }
  deriving (Show, Eq)

tuiApp :: App TuiState e ()
tuiApp =
  App
    { appDraw         = drawTui
    , appChooseCursor = showFirstCursor
    , appHandleEvent  = handleTuiEvent
    , appStartEvent   = pure
    , appAttrMap      = const theMap
    }

buildInitialState :: IO TuiState
buildInitialState = do
    randomNum        <- randomSingleGuess 4

    pure TuiState
      { homeScreen     =    [ "  1 Player    "
                            , "  2 Players   "
                            , "DKAI vs player"
                            ]
      , screen         = 1
      , navSelect      = 0
      , gameState      = initialGS
      , gameStateIndex = (9, 0)
      , pinSlots       = initialRS
      , boss           = initialBoss
      , random         = intsToSlots randomNum
      , aiSearchSpace  = searchS
      }

drawTui :: TuiState -> [Widget ()]
drawTui ts =
    case screen ts of
        0  -> [inputUI]
            where
                box     = B.borderWithLabel label inside
                inside  = drawInputScreen (boss ts)
                label   = str "Please Input Color"
                inputUI = (C.vCenter $ C.hCenter $ box) <=> (C.vCenter $ C.hCenter $ controlBox)
        1  -> [homeUI]
            where
              box    = B.borderWithLabel label inside
              inside = (drawHomeScreen (homeScreen ts) (navSelect ts) 0)
              label  = str "Game Modes"
              homeUI = drawTitle <=> (C.hCenter box) <=> (C.vCenter $ C.hCenter $ controlBox)
        2  -> [outUI]
            where
              boxGuess     = B.borderWithLabel labelGuess insideGuess
              insideGuess  = vBox $ map drawRow (gameState ts)
              labelGuess   = str "Guess"
              boxResult    = B.borderWithLabel labelResult insideResult
              insideResult = vBox $ map drawSlots (pinSlots ts)
              labelResult  = str "Result"
              bossUI       = drawBossUI (boss ts) gamePrompt
              gameUI       = padBottom (Pad 5) (C.vCenter $ C.hCenter (boxGuess <+> boxResult))
              mUI          = (C.vCenter $ C.hCenter (bossUI <=> gameUI))
              outUI        = C.vCenter $ C.hCenter (B.borderWithLabel (str "Game") mUI <+> (controlBox))
              gamePrompt   = if (((gameStateIndex ts) == ((-1), 0)) && (fst (head (gameState ts)) /= fst (boss ts)))
                             then "Failed!"
                             else "Success!"
        3  -> [aiUI]
            where
              boxGuess     = B.borderWithLabel labelGuess insideGuess
              insideGuess  = vBox $ map drawRow (gameState ts)
              labelGuess   = str "Guess"
              boxResult    = B.borderWithLabel labelResult insideResult
              insideResult = vBox $ map drawSlots (pinSlots ts)
              labelResult  = str "Result"
              bossUI       = drawBossUI (boss ts) gamePrompt
              gameUI       = padBottom (Pad 5) (C.vCenter $ C.hCenter (boxGuess <+> boxResult))
              mUI          = (C.vCenter $ C.hCenter (bossUI <=> gameUI))
              aiUI         = C.vCenter $ C.hCenter (B.borderWithLabel (str "Game") mUI <+> (controlBox))
              gamePrompt   = if (((gameStateIndex ts) == ((-1), 0)) && (fst (head (gameState ts)) /= fst (boss ts)))
                             then "Failed!"
                             else "Success!"
        -- screen 4
        _  -> [aiInputUI]
            where
                box       = B.borderWithLabel label inside
                inside    = drawInputScreen (boss ts)
                label     = str "Please Input Color for DKAI"
                aiInputUI = (C.vCenter $ C.hCenter $ box) <=> (C.vCenter $ C.hCenter $ controlBox) 

drawBossUI :: ([Slot], Bool) -> String ->  Widget ()
drawBossUI (solution, showSol) bossLabel = bUI
            where
              box    = B.borderWithLabel label inside
              inside = if showSol then drawSlots solution else hidden
              label  = if showSol then str bossLabel else str "Boss"
              bUI    = C.vCenter $ C.hCenter box
              hidden = str " [X][X][X][X] "

drawTitle :: Widget ()
drawTitle = padTopBottom 5 (C.hCenter (l1 <=> l2 <=> l3 <=> l4 <=> l5))
  where
    l1 = str s1
    l2 = str s2
    l3 = str s3
    l4 = str s4
    l5 = str s5

drawInputScreen :: ([Slot], Bool) -> Widget ()
drawInputScreen row = emptySpace <+> rowUI <+> emptySpace
  where
    rowUI      = drawSlots (fst row)
    emptySpace = str "            "

controlBox :: Widget ()
controlBox = setAvailableSize (50, 52) (withBorderStyle ascii $ B.borderWithLabel controlLabel (controls <+> navigationC))
            where
              controlLabel = str "Controls & Info"
              controls     = vBox $ map drawListElement controlT
              navigationC  = vBox $ map drawListElement navControl

drawHomeScreen :: [String] -> Int -> Int -> Widget ()
drawHomeScreen [] _ _ = emptyWidget
drawHomeScreen (x:xs) m n = current <=> rest
                        where
                          current = if m == n
                                    then str "> " <+> str x <+> str " <"
                                    else str "  " <+> str x <+> str "  "
                          rest    = drawHomeScreen xs m (n + 1)

drawListElement :: String -> Widget ()
drawListElement s = padLeftRight 2 $ str s

drawRow :: ([Slot], Int) -> Widget ()
drawRow row = case snd row of
              1 -> leftArrow <+> rowUI <+> rightArrow
              _ -> emptySpace <+> rowUI <+> emptySpace
              where
                rowUI      = drawSlots (fst row)
                leftArrow  = str ">"
                rightArrow = str "<"
                emptySpace = str " "

drawSlots :: [Slot] -> Widget ()
drawSlots []             = emptyWidget
drawSlots (Empty:xs)     = str "[ ]" <+> rest
                      where
                        rest      = drawSlots xs
drawSlots ((Guess c):xs) = setColorF (str slotColor) <+>rest
                      where
                        setColorF = case c of
                                    Red -> withAttr "red"
                                    Blue -> withAttr "blue"
                                    Green -> withAttr "green"
                                    White -> withAttr "white"
                                    Purple -> withAttr "purple"
                                    Yellow -> withAttr "yellow"
                        slotColor = "[" ++ show c ++ "]"
                        rest      = drawSlots xs

-- initial idea about drawing the pins differently
-- drawPins :: [Slot] -> Widget ()
-- drawPins []             = emptyWidget
-- drawPins (Empty:xs)     = str "[ ]" <+> rest
--                       where
--                         rest      = drawSlots xs
-- drawPins ((Guess c):xs) = pin <+> rest
--                       where
--                         setColorF c = case c of
--                                     Red -> withAttr "redPin" (str "⦿")
--                                     White -> withAttr "whitePin" (str "⦾")
--                         pin         = str "[" <+> setColorF c <+> str "]"
--                         rest        = drawSlots xs 

homeScreenSelect :: TuiState -> Int -> TuiState
homeScreenSelect s dir =
  case screen s of
    1 -> case dir of
          -- 1 means down, 0 means up
          1 -> TuiState {
                        homeScreen    = homeScreen s,
                        screen         = screen s,
                        navSelect      = newNavSelect,
                        gameState      = gameState s,
                        gameStateIndex = gameStateIndex s,
                        pinSlots       = pinSlots s,
                        boss           = boss s,
                        random         = random s,
                        aiSearchSpace  = aiSearchSpace s
                        }
            where
             newNavSelect = if navSelect s == 2 then 0 else navSelect s + 1

          _ -> TuiState {
                        homeScreen     = homeScreen s,
                        screen         = screen s,
                        navSelect      = newNavSelect,
                        gameState      = gameState s,
                        gameStateIndex = gameStateIndex s,
                        pinSlots       = pinSlots s,
                        boss           = boss s,
                        random         = random s,
                        aiSearchSpace  = aiSearchSpace s
                        }
            where
              newNavSelect = if navSelect s == 0 then 2 else navSelect s - 1
    _ -> s

select :: TuiState -> TuiState
select s =
  case screen s of
    -- Home screen
    1 -> TuiState
                {
                  homeScreen     = homeScreen s,
                  screen         = newScreen,
                  navSelect      = navSelect s,
                  gameState      = gameState s,
                  gameStateIndex = gameStateIndex s,
                  pinSlots       = pinSlots s,
                  boss           = newBoss,
                  random         = random s,
                  aiSearchSpace  = aiSearchSpace s
                }
      where
          newScreen   = case navSelect s of
                          0 -> 2 -- 1 player -> gameScreen(2)
                          1 -> 0 -- 2 players -> player 1 input screen(0)
                          _ -> 4 -- DKAI mode -> player 1 input AI screen (4)
          newBoss     = case navSelect s of
                          0 -> (random s, False)
                          _ -> boss s

    -- Game screen
    2 -> if rowIndex < 0 || colIndex /= 4 then s
         else
            TuiState
                {
                  homeScreen     = homeScreen s,
                  screen         = screen s,
                  navSelect      = navSelect s,
                  gameState      = newGameState,
                  gameStateIndex = newGameStateIndex,
                  pinSlots       = newPinSlots,
                  boss           = newBoss,
                  random         = random s,
                  aiSearchSpace  = aiSearchSpace s
                }

      where
        rowIndex          = fst (gameStateIndex s)
        colIndex          = snd (gameStateIndex s)
        newGameStateIndex = (rowIndex - 1, 0)
        newRowIndex       = fst newGameStateIndex
        -- replace current row with ([Slot], 1) -> ([Slot], 2)
        newGameState1     = replaceList (gameState s) rowIndex (guessRow, 2)
        newGameState      = if newRowIndex == (-1) then newGameState1
                            else replaceList newGameState1 newRowIndex emptyRow
        newPinSlots       = replaceList (pinSlots s) rowIndex judgeResult
        guessRow          = fst (gameState s !! (max 0 rowIndex))
        judgeResult       = masterJudge (fst(boss s)) guessRow
        newBoss
          | judgeResult == success            = (fst (boss s), True)
          | newRowIndex == (-1)               = (fst (boss s), True) -- if used up all slots
          | otherwise                         = boss s
        emptyRow          = ([Empty, Empty, Empty, Empty], 1)

    -- just switch to screen 2 if ready
    0 -> if emptyLength /= 0 then s
          else
            TuiState
                      {
                        homeScreen     = homeScreen s,
                        screen         = 2,
                        navSelect      = navSelect s,
                        gameState      = gameState s,
                        gameStateIndex = gameStateIndex s,
                        pinSlots       = pinSlots s,
                        boss           = boss s,
                        random         = random s,
                        aiSearchSpace  = aiSearchSpace s
                      }
      where
        oldSlots     = fst (boss s)
        existedSlots = filter (/= Empty) oldSlots
        emptyLength = 4 - length existedSlots

    -- AI mode selection
    3 -> if snd (boss s) then s
          else
            TuiState
                      {
                        homeScreen     = homeScreen s,
                        screen         = screen s,
                        navSelect      = navSelect s,
                        gameState      = newGameState,
                        gameStateIndex = newGameStateIndex,
                        pinSlots       = newPinSlots,
                        boss           = newBoss,
                        random         = random s,
                        aiSearchSpace  = newSearchSpace
                      }
      where
        rowIndex           = fst (gameStateIndex s)
        newRowIndex        = fst newGameStateIndex
        newGameState       = replaceList (replaceList (gameState s) rowIndex (aiGuess, 2)) newRowIndex emptyRow
        newGameStateIndex  = (rowIndex - 1, 0)
        judgeResult        = masterJudge (fst(boss s)) aiGuess
        newPinSlots        = replaceList (pinSlots s) rowIndex judgeResult
        newSearchSpace     = dkSearch (aiSearchSpace s) aiGuess judgeResult
        aiGuess            = head (aiSearchSpace s)
        newBoss
          | judgeResult == success            = (fst (boss s), True)
          | newRowIndex == (-1)               = (fst (boss s), True) -- if used up all slots
          | otherwise                         = boss s
        emptyRow           = ([Empty, Empty, Empty, Empty], 1)

    -- screen 4
    _ -> if emptyLength /= 0 then s
          else
            TuiState -- AI's color input is duplicate protected
                      {
                        homeScreen     = homeScreen s,
                        screen         = newScreen,
                        navSelect      = navSelect s,
                        gameState      = gameState s,
                        gameStateIndex = gameStateIndex s,
                        pinSlots       = pinSlots s,
                        boss           = newBoss,
                        random         = random s,
                        aiSearchSpace  = aiSearchSpace s
                      }
      where
        oldSlots     = fst (boss s)
        existedSlots = filter (/= Lib.Empty) oldSlots
        emptyLength  = 4 - length existedSlots
        valid        = validInput existedSlots
        validInput x = length (nub x) == length x
        newBoss      = if valid then boss s else initialBoss
        newScreen    = if valid then 3 else 4

userInput :: TuiState -> Slot -> TuiState
userInput s guess =
  case screen s of
    0 -> if emptyLength < 0 then s
          else
            TuiState
                      {
                        homeScreen     = homeScreen s,
                        screen         = screen s,
                        navSelect      = navSelect s,
                        gameState      = gameState s,
                        gameStateIndex = gameStateIndex s,
                        pinSlots       = pinSlots s,
                        boss           = newBoss,
                        random         = random s,
                        aiSearchSpace  = aiSearchSpace s
                      }
      where
        newBoss      = (newSlots, snd (boss s))
        oldSlots     = fst (boss s)
        existedSlots = filter (/= Empty) oldSlots ++ [guess]
        newSlots     = if emptyLength > 0
                       then existedSlots ++ replicate emptyLength Empty
                       else existedSlots
        emptyLength = 4 - length existedSlots

        -- if already won or already inputed 4 guesses, no state change
    2 -> if snd (boss s) || snd (gameStateIndex s) > 3 then s
         else
            TuiState
                {
                  homeScreen     = homeScreen s,
                  screen         = screen s,
                  navSelect      = navSelect s,
                  gameState      = newGameState,
                  gameStateIndex = newGameStateIndex,
                  pinSlots       = pinSlots s,
                  boss           = boss s,
                  random         = random s,
                  aiSearchSpace  = aiSearchSpace s
                }
      where
        newGameState = if colIndex > 3
                        then gameState s
                        else map f (gameState s)
        f row
          | snd row == 1 && colIndex <= 3 = (replaceList (fst row) colIndex guess, 1)
          | otherwise = row
        newGameStateIndex = if colIndex > 3
                            then gameStateIndex s
                            else (rowIndex, colIndex + 1)
        rowIndex     = fst (gameStateIndex s)
        colIndex     = snd (gameStateIndex s)

    4 -> if emptyLength < 0 then s
          else
            TuiState
                      {
                        homeScreen     = homeScreen s,
                        screen         = screen s,
                        navSelect      = navSelect s,
                        gameState      = gameState s,
                        gameStateIndex = gameStateIndex s,
                        pinSlots       = pinSlots s,
                        boss           = newBoss,
                        random         = random s,
                        aiSearchSpace  = aiSearchSpace s
                      }
      where
        newBoss      = (newSlots, snd (boss s))
        oldSlots     = fst (boss s)
        existedSlots = filter (\x -> x /= Empty) oldSlots ++ [guess]
        newSlots     = if (emptyLength > 0)
                       then existedSlots ++ (replicate emptyLength Empty)
                       else existedSlots
        emptyLength = 4 - (length existedSlots)
    _ -> s

-- KBS button
back :: TuiState -> TuiState
back s =
  case screen s of
    2 -> case colIndex of
      0 -> s
      _ ->
            TuiState
                {
                  homeScreen     = homeScreen s,
                  screen         = screen s,
                  navSelect      = navSelect s,
                  gameState      = newGameState,
                  gameStateIndex = newGameStateIndex,
                  pinSlots       = pinSlots s,
                  boss           = boss s,
                  random         = random s,
                  aiSearchSpace  = aiSearchSpace s
                }
      where
        rowIndex          = fst (gameStateIndex s)
        colIndex          = snd (gameStateIndex s)
        newGameStateIndex = (rowIndex, colIndex - 1)
        newGameState      = map f (gameState s)
        f row
          | snd row == 1 && colIndex > 0 = (replaceList (fst row) (colIndex - 1) Lib.Empty, 1)
          | otherwise = row
    
    0 -> if emptyLength == 4 then s
          else
              TuiState
                    {
                      homeScreen     = homeScreen s,
                      screen         = screen s,
                      navSelect      = navSelect s,
                      gameState      = gameState s,
                      gameStateIndex = gameStateIndex s,
                      pinSlots       = pinSlots s,
                      boss           = newBoss,
                      random         = random s,
                      aiSearchSpace  = aiSearchSpace s
                    }
      where
        newBoss      = (newSlots, snd (boss s))
        oldSlots     = fst (boss s)
        existedSlots = filter (/= Empty) oldSlots
        newSlots     = replaceList oldSlots index Lib.Empty
        index        = 4 - emptyLength - 1
        emptyLength = 4 - length existedSlots


    4 -> if emptyLength == 4 then s
          else
              TuiState
                    {
                      homeScreen     = homeScreen s,
                      screen         = screen s,
                      navSelect      = navSelect s,
                      gameState      = gameState s,
                      gameStateIndex = gameStateIndex s,
                      pinSlots       = pinSlots s,
                      boss           = newBoss,
                      random         = random s,
                      aiSearchSpace  = aiSearchSpace s
                    }
      where
        newBoss      = (newSlots, snd (boss s))
        oldSlots     = fst (boss s)
        existedSlots = filter (/= Empty) oldSlots
        newSlots     = replaceList oldSlots index Lib.Empty
        index        = 4 - emptyLength - 1
        emptyLength = 4 - length existedSlots
    _ -> s


replaceList :: [a] -> Int -> a -> [a]
replaceList s index target = fst splittedList ++ [target] ++ tail (snd splittedList)
  where splittedList = splitAt index s

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q')  [] -> halt s
        EvKey KUp          [] -> continue $ homeScreenSelect s 0
        EvKey KDown        [] -> continue $ homeScreenSelect s 1
        EvKey KEnter       [] -> continue $ select s
        EvKey KBS          [] -> continue $ back s
        EvKey (KChar 'r')  [] -> continue $ userInput s Lib.red
        EvKey (KChar 'b')  [] -> continue $ userInput s Lib.blue
        EvKey (KChar 'g')  [] -> continue $ userInput s Lib.green
        EvKey (KChar 'w')  [] -> continue $ userInput s Lib.white
        EvKey (KChar 'p')  [] -> continue $ userInput s Lib.purple
        EvKey (KChar 'y')  [] -> continue $ userInput s Lib.yellow
        _                     -> continue s
    _ -> continue s


theMap :: AttrMap
theMap = attrMap V.defAttr
        [
          ("red", fg V.red)
        , ("blue", fg V.blue)
        , ("green", fg V.green)
        , ("yellow", fg V.yellow)
        , ("purple", fg V.magenta)
        , ("white", fg V.white)
        , ("redPin", bg V.red)
        , ("whitePin", bg V.white)
        ]