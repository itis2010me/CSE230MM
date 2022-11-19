{-# LANGUAGE OverloadedStrings #-}
module Tui where

-- import Lib (Slot, PinColor)

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
  {   homeScreen     :: [String]
    , navSelect      :: Int
    , screen         :: Int
    , gameState      :: [([Slot], Int)]
    , gameStateIndex :: (Int, Int) -- current input pointer
    , pinSlots       :: [[String]]
  }
  deriving (Show, Eq)

data ResourceName =
  ResourceName
  deriving (Show, Eq, Ord)


controlT = [ "q - exit", "a - red", "s - blue", "d - green", "f - white"
            , "g - black", "h - yellow"]
navControl = [ "↑/↓ - Navigation"
             , "↩ - Selec"]

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

tuiApp :: App TuiState e ()
tuiApp =
  App
    { appDraw = drawTui
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleTuiEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap mempty []
    }

buildInitialState :: IO TuiState
buildInitialState =
    pure TuiState
    { homeScreen =    [ "  1 Player    " 
                      , "  2 Players   "
                      , "DKAI vs player"
                      ]
    , screen        = 1
    , navSelect     = 1
    , gameState     = initialGS
    , gameStateIndex = (0,0) -- current input pointer
    , pinSlots = []
    }

drawTui :: TuiState -> [Widget ()]
drawTui ts =
    case screen ts of
        0  -> [vBox $ map f [last (homeScreen ts)]]
            where
                f = str
        1 -> [ui]
            where
              box = B.borderWithLabel label inside
              inside = (drawHomeScreen (homeScreen ts) (navSelect ts) 0) <=> placeHolder
              label = str "MasterMind"
              ui = (C.vCenter $ C.hCenter $ box) <=> (C.vCenter $ C.hCenter $ controlBox)
              placeHolder = str "                  "
        2 -> [gameUI]
          where
            gameUI = (C.vCenter $ C.hCenter $ box) <=> (C.vCenter $ C.hCenter $ controlBox)
            box = B.borderWithLabel label inside
            inside = vBox $ map drawRow (gameState ts)
            label = str "MasterMind"

controlBox :: Widget ()
controlBox = withBorderStyle ascii $ B.borderWithLabel controlLabel (controls <+> navigationC)
            where 
              controlLabel = str "Controls"
              controls = vBox $ map drawListElement controlT
              navigationC = vBox $ map drawListElement navControl
--                 gameModes  selec  count
drawHomeScreen :: [String] -> Int -> Int -> Widget ()
drawHomeScreen [] _ _ = emptyWidget
drawHomeScreen (x:xs) m n = current <=> rest
                        where
                          current = if (m == n)
                                    then str "> " <+> str x <+> str " <"
                                    else str "  " <+> str x <+> str "  "
                          rest = drawHomeScreen xs m (n+1)

drawListElement :: String -> Widget ()
drawListElement s = padLeftRight 5 $ str s

drawRow :: ([Slot], Int) -> Widget ()
drawRow row = case snd row of
              1 -> leftArrow <+> rowUI <+> rightArrow
              _ -> emptySpace <+> rowUI <+> emptySpace
              where
                rowUI = drawSlots (fst row)
                leftArrow  = str ">"
                rightArrow = str "<"
                emptySpace = str " "

drawSlots :: [Slot] -> Widget ()
drawSlots [] = emptyWidget  
drawSlots (Empty:xs)     = str "[ ]" <+> rest
                      where 
                        rest = drawSlots xs
drawSlots ((Guess c):xs) = str slotColor <+> rest 
                      where 
                        slotColor = "[" ++ show c ++ "]"
                        rest = drawSlots xs


update :: TuiState -> String -> TuiState
update s ns = TuiState {homeScreen = old_contents ++ [ns],
                        screen = screen s,
                        navSelect     = navSelect s,
                        gameState     = gameState s,
                        gameStateIndex = gameStateIndex s,
                        pinSlots = pinSlots s
                        }
  where
    old_contents = homeScreen s

homeScreenSelect :: TuiState -> Int -> TuiState
homeScreenSelect s dir = 
  case screen s of
    1 -> case dir of 
          1 -> TuiState {homeScreen = homeScreen s,
                        screen      = screen s,
                        navSelect     = newNavSelect,
                        gameState     = gameState s,
                        gameStateIndex = gameStateIndex s,
                        pinSlots = pinSlots s
                        }
            where
             newNavSelect = if (navSelect s) == 2 then 0 else (navSelect s) + 1

          _ -> TuiState {homeScreen = homeScreen s,
                         screen = screen s,
                         navSelect     = newNavSelect,
                         gameState     = gameState s,
                         gameStateIndex = gameStateIndex s,
                         pinSlots = pinSlots s
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
--                 old_contents = homeScreen s
--                 new_screen   = mod ((screen s) + 1) 3

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q')  [] -> halt s
        EvKey (KLeft)      [] -> continue $ update s "←"
        EvKey (KRight)     [] -> continue $ update s "→"
        EvKey (KUp)        [] -> continue $ homeScreenSelect s (-1)
        EvKey (KDown)      [] -> continue $ homeScreenSelect s 1
        -- EvKey (KChar 'a')  [] -> continue $ toggle s
        -- EvKey (KChar 'd')  [] -> continue $ toggle s
        _ -> continue s
    _ -> continue s
                                              