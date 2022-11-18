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
  {   state_content :: [String]
    , navSelect :: Int
    , screen :: Int
    , gameState :: [([Slot], Int)]
  }
  deriving (Show, Eq)

data ResourceName =
  ResourceName
  deriving (Show, Eq, Ord)


controlT = [ "q - exit", "a - red", "s - blue", "d - green", "f - white"
            , "g - black", "h - yellow"]
navControl = [ "↑/↓ - Navigation"
             , "↩ - Selec"]

initialGS = [ ([Empty, Empty, Empty, Empty], `0)
            , ([Empty, Empty, Empty, Empty], 0)
            , ([Empty, Empty, Empty, Empty], 0)
            , ([Empty, Empty, Empty, Empty], 0)
            , ([Empty, Empty, Empty, Empty], 0)
            , ([Empty, Empty, Empty, Empty], 0)
            , ([Empty, Empty, Empty, Empty], 0)
            , ([Empty, Empty, Empty, Empty], 0)
            , ([Empty, Empty, Empty, Empty], 1)
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
    { state_content = [ "1 Player      "
                      , "2 Players     "
                      , "DKAI vs player"
                      ]
    , screen        = 1
    , navSelect     = 1
    , gameState     = initialGS
    }

drawTui :: TuiState -> [Widget ()]
drawTui ts =
    case screen ts of
        0  -> [vBox $ map f [last (state_content ts)]]
            where
                f = str
        1 -> [ui]
            where
              box = B.borderWithLabel label inside
              inside = vBox $ map drawListElement (state_content ts)
              label = str "MasterMind"
              ui = (C.vCenter $ C.hCenter $ box) <=> (C.vCenter $ C.hCenter $ controlBox)
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
update s ns = TuiState {state_content = old_contents ++ [ns],
                        screen = screen s,
                        navSelect     = navSelect s,
                        gameState     = gameState s
                        }
  where
    old_contents = state_content s

toggle :: TuiState -> TuiState
toggle s = TuiState
                    {
                      state_content = old_contents,
                      screen        = new_screen,
                      navSelect     = navSelect s,
                      gameState     = gameState s
                    }
            where
                old_contents = state_content s
                new_screen   = mod ((screen s) + 1) 3

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q')  [] -> halt s
        EvKey (KLeft)      [] -> continue $ update s "←"
        EvKey (KRight)     [] -> continue $ update s "→"
        EvKey (KUp)        [] -> continue $ update s "↑"
        EvKey (KDown)      [] -> continue $ update s "↓"
        EvKey (KChar 'a')  [] -> continue $ toggle s
        EvKey (KChar 'd')  [] -> continue $ toggle s
        _ -> continue s
    _ -> continue s
