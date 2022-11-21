# CSE230 Final Project F22
```
    _/      _/                        _/                          _/      _/   _/                  _/   
   _/_/  _/_/    _/_/_/    _/_/_/  _/_/_/_/    _/_/    _/  _/_/  _/_/  _/_/       _/_/_/      _/_/_/    
  _/  _/  _/  _/    _/  _/_/        _/      _/_/_/_/  _/_/      _/  _/  _/   _/  _/    _/  _/    _/     
 _/      _/  _/    _/      _/_/    _/      _/        _/        _/      _/   _/  _/    _/  _/    _/      
_/      _/    _/_/_/  _/_/_/        _/_/    _/_/_/  _/        _/      _/   _/  _/    _/    _/_/_/       
```

## Application
Our application is based on the game "MasterMind". 

## [Classic MasterMind Game](https://en.wikipedia.org/wiki/Mastermind_(board_game))
This game aims to guess the correct sequence of colors in 4 positions. We will have two players, one player guess and one player provide the colors and the positions of the 4 pegs. For example, the answer could be blue, red, yellow, green and the player needs to guess both the color and the order correctly. There will be 6 colors available. The guesser will guess 10 rounds and each round the player would know extra information about the answer based on the current guess.

## Application Architecture
```
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
```
### Game State
```
TuiState
  {   homeScreen     :: [String]
    , navSelect      :: Int
    , screen         :: Int
    , gameState      :: [([Slot], Int)]
    , gameStateIndex :: (Int, Int) -- current input pointer
    , pinSlots       :: [[String]]
    , boss           :: ([Slot], Bool)
    , random         :: [Slot]
  }
```
*TODO :: further explanation*

## Challenges
- Initial project enviroment setup.
  We encountered a major issue in the beginning of our project. Due to some subtle differences in our haskell enviroment `.yaml.lock` files, we were not able to get our code to compile. 
  This was later solved by modifying the `.yaml.lock` file and shared with all team members. 
- Random number generation.
  Player 1 mode requires the system to randomly generate a set of 4 colors for the player to guess, however, we spend quite some time figuring out how to integrate the randomness into our program. 
- AI's implementation


## Goals
### G1
- Basic TUI setup as well as basic game state logic. [Done]
- Implementation for 1 Player game mode. [Done]

### G2
- Implementation for 2 Players game mode. [Done]

### G3 (Milestone 3)
- TUI optimization with colored pegs.
- TUI navigation and control optimizations.

### G4
- Implementation for the AI player game mode.
- Implementation for variable game settings. (If time permits)
    - Players are allowed to change the followings:
        - Number of rounds
        - Possible peg colors (minimum 2)
        - Allow/disallow duplicate pins in hidden pegs. 





## Collaborator
- Chufan Wu
- Yuan Chang
- Yuxin Liu
- Siran Ma

### History
Last edit Nov 18, 2022