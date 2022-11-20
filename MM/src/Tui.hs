{-# LANGUAGE OverloadedStrings #-}
module Tui where

import Lib

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Graphics.Vty.Input.Events
import Brick (Widget, simpleMain, (<+>), str, withBorderStyle, joinBorders, emptyWidget)
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (padLeftRight)
import Control.Lens hiding (Empty)

tui :: IO ()
tui = do
  initialState <- buildInitialState
  endState <- defaultMain tuiApp initialState
  print "Done"

-- MM State
data TuiState =
  TuiState
  {   homeScreen     :: [String]         -- there are 3 modes for homeScreen, ["1 Player", "2 Players", "DKAI vs player"]
    , navSelect      :: Int              -- index for homeScreen to choose different modes, [0: "1 Player", 1: "2 Players", 2: "DKAI vs player"]
    , screen         :: Int              -- 0: ?, 1: homeScreen, 2: gameScreen
    , gameState      :: [([Slot], Int)]  -- 10 rounds of guessing state for gameScreen
    , gameStateIndex :: (Int, Int)       -- current input pointer, rowIndex and colIndex
    , pinSlots       :: [[Slot]]         -- used for showing each rounds result, each round has 4 elements which can be: ["Empty": both color & position false, "Red": both color & position true, "White": color true & position false]
  }
  deriving (Show, Eq)

controlT = [ "q - exit", "r - red", "b - blue", "g - green", "w - white"
            , "p - purple", "y - yellow"]
navControl = [ "↑/↓ - Navigation", "↩ - Select"]

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

tuiApp :: App TuiState e ()
tuiApp =
  App
    { appDraw         = drawTui
    , appChooseCursor = showFirstCursor
    , appHandleEvent  = handleTuiEvent
    , appStartEvent   = pure
    , appAttrMap      = const $ attrMap mempty []
    }

buildInitialState :: IO TuiState
buildInitialState =
    pure TuiState
    { homeScreen     =    [ "  1 Player    " 
                          , "  2 Players   "
                          , "DKAI vs player"
                          ]
    , screen         = 1
    , navSelect      = 1
    , gameState      = initialGS
    , gameStateIndex = (9, 0)
    , pinSlots       = initialRS
    }

drawTui :: TuiState -> [Widget ()]
drawTui ts =
    case screen ts of
        0  -> [vBox $ map f [last (homeScreen ts)]]
            where
                f    = str
        1  -> [homeUI]
            where
              box    = B.borderWithLabel label inside
              inside = (drawHomeScreen (homeScreen ts) (navSelect ts) 0)
              label  = str "MasterMind"
              homeUI = (C.vCenter $ C.hCenter $ box) <=> (C.vCenter $ C.hCenter $ controlBox)
        2  -> [gameUI]
            where
              boxGuess     = B.borderWithLabel labelGuess insideGuess
              insideGuess  = vBox $ map drawRow (gameState ts)
              labelGuess   = str "MasterMind"
              boxResult    = B.borderWithLabel labelResult insideResult
              insideResult = vBox $ map drawSlots (pinSlots ts)
              labelResult  = str "Result"
              gameUI = (C.vCenter $ C.hCenter $ (boxGuess <+> boxResult)) <=> (C.vCenter $ C.hCenter $ controlBox)

controlBox :: Widget ()
controlBox = withBorderStyle ascii $ B.borderWithLabel controlLabel (controls <+> navigationC)
            where 
              controlLabel = str "Controls"
              controls     = vBox $ map drawListElement controlT
              navigationC  = vBox $ map drawListElement navControl

drawHomeScreen :: [String] -> Int -> Int -> Widget ()
drawHomeScreen [] _ _ = emptyWidget
drawHomeScreen (x:xs) m n = current <=> rest
                        where
                          current = if (m == n)
                                    then str "> " <+> str x <+> str " <"
                                    else str "  " <+> str x <+> str "  "
                          rest    = drawHomeScreen xs m (n + 1)

drawListElement :: String -> Widget ()
drawListElement s = padLeftRight 5 $ str s

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
drawSlots ((Guess c):xs) = str slotColor <+> rest 
                      where 
                        slotColor = "[" ++ show c ++ "]"
                        rest      = drawSlots xs

update :: TuiState -> String -> TuiState
update s ns = TuiState {homeScreen     = oldContents ++ [ns],
                        screen         = screen s,
                        navSelect      = navSelect s,
                        gameState      = gameState s,
                        gameStateIndex = gameStateIndex s,
                        pinSlots       = pinSlots s
                        }
  where
    oldContents = homeScreen s

homeScreenSelect :: TuiState -> Int -> TuiState
homeScreenSelect s dir = 
  case screen s of
    1 -> case dir of 
          1 -> TuiState {homeScreen    = homeScreen s,
                        screen         = screen s,
                        navSelect      = newNavSelect,
                        gameState      = gameState s,
                        gameStateIndex = gameStateIndex s,
                        pinSlots       = pinSlots s
                        }
            where
             newNavSelect = if (navSelect s) == 2 then 0 else (navSelect s) + 1

          _ -> TuiState {homeScreen     = homeScreen s,
                         screen         = screen s,
                         navSelect      = newNavSelect,
                         gameState      = gameState s,
                         gameStateIndex = gameStateIndex s,
                         pinSlots       = pinSlots s
                        }
            where
              newNavSelect = if (navSelect s) == 0 then 2 else (navSelect s) - 1
    _ -> s

toggle :: TuiState -> TuiState
toggle s = TuiState
                    {
                      homeScreen     = homeScreen s,
                      screen         = newScreen,
                      navSelect      = navSelect s,
                      gameState      = gameState s,
                      gameStateIndex = gameStateIndex s,
                      pinSlots       = pinSlots s
                    }
            where
                newScreen   = mod ((screen s) + 1) 3

gameScreenSelect :: TuiState -> Slot -> TuiState
gameScreenSelect s guess =
  case screen s of
    2 -> TuiState
                {
                  homeScreen     = homeScreen s,
                  screen         = screen s,
                  navSelect      = navSelect s,
                  gameState      = newGameState,
                  gameStateIndex = newGameStateIndex,
                  pinSlots       = pinSlots s
                }
      where 
        newGameState = if snd (gameStateIndex s) == 3
                       then replaceList (map f (gameState s)) newRowIndex newRow
                       else map f (gameState s)
        f row        = if snd row == 1 && colIndex == 3
                       then (replaceList (fst row) colIndex guess, 2)
                       else if snd row == 1 && colIndex /= 3
                       then (replaceList (fst row) colIndex guess, 1)
                       else row
        newGameStateIndex = if colIndex == 3 
                            then (rowIndex - 1, 0)
                            else (rowIndex, colIndex + 1)
        rowIndex     = fst (gameStateIndex s)
        colIndex     = snd (gameStateIndex s)
        newRowIndex  = fst newGameStateIndex
        newRow       = ([Empty, Empty, Empty, Empty], 1)
    _ -> s

replaceList :: [a] -> Int -> a -> [a]
replaceList s index target = (fst splittedList) ++ [target] ++ (tail (snd splittedList))
  where splittedList = splitAt index s

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q')  [] -> halt s
        EvKey (KLeft)      [] -> continue $ update s "←"
        EvKey (KRight)     [] -> continue $ update s "→"
        EvKey (KUp)        [] -> continue $ homeScreenSelect s 0
        EvKey (KDown)      [] -> continue $ homeScreenSelect s 1
        EvKey (KEnter)     [] -> continue $ toggle s
        EvKey (KChar 'r')  [] -> continue $ gameScreenSelect s red
        EvKey (KChar 'b')  [] -> continue $ gameScreenSelect s blue
        EvKey (KChar 'g')  [] -> continue $ gameScreenSelect s green
        EvKey (KChar 'w')  [] -> continue $ gameScreenSelect s white
        EvKey (KChar 'p')  [] -> continue $ gameScreenSelect s purple
        EvKey (KChar 'y')  [] -> continue $ gameScreenSelect s yellow
        _                     -> continue s
    _ -> continue s
                                              