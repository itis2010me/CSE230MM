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
    , pinSlots       :: [[String]]       -- used for showing each rounds result, each round has 4 elements which can be: ["Empty": both color & position false, "Red": both color & position true, "White": color true & position false]
  }
  deriving (Show, Eq)

controlT = [ "q - exit", "a - red", "s - blue", "d - green", "f - white"
            , "g - purple", "h - yellow"]
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
            , ([Guess Red, Empty, Empty, Empty], 1)
            , ([Guess Red, Guess Blue, Guess Green, Guess Yellow], 2)
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
    , gameStateIndex = (0, 0)
    , pinSlots       = []
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
              box    = B.borderWithLabel label inside
              inside = vBox $ map drawRow (gameState ts)
              label  = str "MasterMind"
              gameUI = (C.vCenter $ C.hCenter $ box) <=> (C.vCenter $ C.hCenter $ controlBox)

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

-- toggle :: TuiState -> TuiState
-- toggle s = TuiState
--                     {
--                       homeScreen    = homeScreen s,
--                       screen        = new_screen,
--                       navSelect     = new_nav,
--                       gameState     = gameState s
--                     }
--             where
--                 oldContents = homeScreen s
--                 new_screen   = mod ((screen s) + 1) 3

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
        -- EvKey (KChar 'a')  [] -> continue $ toggle s
        -- EvKey (KChar 'd')  [] -> continue $ toggle s
        _                     -> continue s
    _ -> continue s
                                              