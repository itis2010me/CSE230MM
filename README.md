# CSE230 Final Project F22
```
    _/      _/                        _/                         
   _/_/  _/_/    _/_/_/    _/_/_/  _/_/_/_/    _/_/    _/  _/_/
  _/  _/  _/  _/    _/  _/_/        _/      _/_/_/_/  _/_/     
 _/      _/  _/    _/      _/_/    _/      _/        _/        
_/      _/    _/_/_/  _/_/_/        _/_/    _/_/_/  _/        

    _/      _/   _/                  _/   
   _/_/  _/_/       _/_/_/      _/_/_/    
  _/  _/  _/   _/  _/    _/  _/    _/     
 _/      _/   _/  _/    _/  _/    _/      
_/      _/   _/  _/    _/    _/_/_/   
```
# Project Organization

- `README.md` - This file. 
- `./MM` - Directory containing all source code.
  - `./MM/src/Tui.hs` - Source code for all TUI drawing and game state transitions.
  - `./MM/src/Lib.hs` - Source code for all custom data types, game logics, constants and internal variables.
  - `./MM/*` - All files automatically generated by `stack`.
- `./figures` - Directory containing screenshots of the game. 

## Application
Our application is based on the 1970 boardgame "MasterMind". 

## [Classic MasterMind Game](https://en.wikipedia.org/wiki/Mastermind_(board_game))
This game aims to guess the correct sequence of colors in 4 positions. We will have two players, one player guesses and one player provides the colors and the positions of the 4 pegs. For example, the answer could be blue, red, yellow, green and the player needs to guess both the color and the order correctly. There will be 6 colors available. The guesser will guess up to 10 rounds and each round the player would know extra information about the answer based on the current guess.

## Application Architecture
```haskell
data Slot
  = Empty
  | Guess PinColor

data PinColor
  = Red
  | Blue
  | Green
  | White
  | Purple
  | Yellow
```
### Game State
```haskell
TuiState
  {   homeScreen     :: [String]         -- Descriptions for the game modes.
    , navSelect      :: Int              -- Index for homeScreen selection.
    , screen         :: Int              -- Index for which UI screen to draw.
    , gameState      :: [([Slot], Int)]  -- Game board state, a list of rows, each row is a list of Slots.
    , gameStateIndex :: (Int, Int)       -- current input pointer, (row, col).
    , pinSlots       :: [[String]]       -- list of Slots for the information.
    , boss           :: ([Slot], Bool)   -- the 4 colored pegs that the player is trying to guess.
    , random         :: [Slot]           -- randomly generated colors, used in 1 player mode only.
    , aiSearchSpace  :: [[Slot]]         -- DKAI algorithm's solution search space
  }
```
- UI Screens
  - Home screen
  - Game screen
  - User input screen
  - AI input screen
  - AI game screen
- Game Modes
  - 1 Player - Player trying to guess the randomly generated 4 colored pegs, currenly restricting to non-repeating colors.
  - 2 Players
    Player 1 will be directed to User input screen to input 4(repeat allowed) colors.
    Player 2 will be trying to guess those 4 colors.
  - DKAI vs Player 
    Player 1 will be directed to User input screen to input 4(repeat disallowed) colors.
    AI will try to make a guess after player press `KENTER`
    AI's initial *blind* guess will always be [Red, Green, Blue, Yellow].


## Challenges
- Initial project enviroment setup.
  We encountered a major issue in the beginning of our project. Due to some subtle differences in our haskell enviroment `.yaml.lock` files, we were not able to get our code to compile. 
  This was later solved by modifying the `.yaml.lock` file and shared with all team members. 
- Random number generation.
  Player 1 mode requires the system to randomly generate a set of 4 colors for the player to guess, however, we spend quite some time figuring out how to integrate the randomness into our program. 
- Color attributes

- AI's implementation
  Implementation of Donald Knuth's algorithm, which can solve any non-repeating patterns in (most cases) five moves or fewer. This is the DKAI mode in our game. 
- Implementation choice for updating the game's internal settings. 
  Could be seperate game screens and state variables that the system keeps track of, or having the game settings in a seperate readable file outside. 

## Goals
### G1
- Basic TUI setup as well as basic game state logic. [Done]
- Implementation for 1 Player game mode. [Done]

### G2
- Implementation for 2 Players game mode. [Done]

### G3 (Milestone 3)
- TUI optimization with colored pegs. [Done]
- TUI navigation and control optimizations. [Done]

### G4
- Implementation for the AI player game mode. [Done]
- Implementation for variable game settings. (If time permits)
    - Players are allowed to change the followings:
        - Number of rounds (Not likely)
        - Possible peg colors (Not likely)
        - Allow/disallow duplicate pins in hidden pegs. (Likely)
    - Settings might be inputted into a different setting file, which will be read by the system at start. This may reqire restart the game if the user wishes to change game settings.


## Collaborator
- Chufan Wu
- Yuan Chang
- Yuxin Liu
- Siran Ma

### History
Last edit Nov 30, 2022