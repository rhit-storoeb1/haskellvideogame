module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.IO.Unsafe

width, height, offset :: Int
width = 620
height = 950
offset = 100

playerWidth, playerHeight, playerSpeed :: Float
playerWidth = 50
playerHeight = 75
playerSpeed = 5

meleeWidth, meleeHeight :: Float
meleeWidth = 60
meleeHeight = 10

window :: Display
window = InWindow "Game" (width, height) (offset, offset)

bg :: Picture
bg = unsafePerformIO $ loadBMP "assets/volcanoBGbigger.bmp"

data VideoGame = Game
  { playerLoc :: (Float, Float) ,
    playerMovingUp :: Bool,
    playerMovingRight :: Bool,
    playerMovingDown :: Bool,
    playerMovingLeft :: Bool,
    meleeActive :: Bool
  } deriving Show 
  
initialState :: VideoGame
initialState = Game
  { playerLoc = (0, 0),
    playerMovingUp = False,
    playerMovingRight = False,
    playerMovingDown = False,
    playerMovingLeft = False,
    meleeActive = False
  }

render :: VideoGame -> Picture
render game = 
              pictures [translate bgx bgy bg, 
                        translate x y player, 
                        mkMelee leftX y,
                        mkMelee rightX y
                        ]
               where
               (bgx, bgy) = (0, 0)
               (x,y) = playerLoc game
               player = color red $ rectangleSolid playerWidth playerHeight

               leftX = x - 50
               rightX = x + 50
               meleeColor = if (meleeActive game) then blue else makeColor 0 0 0 0

               mkMelee :: Float -> Float -> Picture
               mkMelee x y = pictures [translate x y $ color meleeColor $ rectangleSolid meleeWidth meleeHeight]
  

background :: Color
background = white



fps :: Int
fps = 60

handleKeys :: Event -> VideoGame -> VideoGame

handleKeys (EventKey (Char 'w') Down _ _) game = game {playerMovingUp = True}
handleKeys (EventKey (Char 'w') Up _ _) game = game {playerMovingUp = False}

handleKeys (EventKey (Char 'a') Down _ _) game = game {playerMovingLeft = True}
handleKeys (EventKey (Char 'a') Up _ _) game = game {playerMovingLeft = False}

handleKeys (EventKey (Char 's') Down _ _) game = game {playerMovingDown = True}
handleKeys (EventKey (Char 's') Up _ _) game = game {playerMovingDown = False}

handleKeys (EventKey (Char 'd') Down _ _) game = game {playerMovingRight = True}
handleKeys (EventKey (Char 'd') Up _ _) game = game {playerMovingRight = False}

handleKeys (EventKey (Char 'k') Down _ _) game = game {meleeActive = True}
handleKeys (EventKey (Char 'k') Up _ _) game = game {meleeActive = False}

handleKeys _ game = game

movePlayer :: VideoGame -> VideoGame

movePlayer game = game {playerLoc = (x', y')}
                   where
                    (x,y) = playerLoc game
                    left = playerMovingLeft game
                    right = playerMovingRight game
                    up = playerMovingUp game
                    down = playerMovingDown game
                    moveX = if left
                            then -playerSpeed
                            else 0
                    moveX' = if right
                             then 
                              if x >= ((fromIntegral width/2) - (playerWidth/2)) then 0
                              else moveX + playerSpeed
                             else if x <= ((-fromIntegral width/2) + (playerWidth/2)) then 0
                             else moveX
                    moveY = if up
                            then playerSpeed
                            else 0
                    moveY' = if down
                             then 
                              if y <= ((-fromIntegral height/2) + (playerHeight/2)) then 0
                              else moveY - playerSpeed
                             else if y >= ((fromIntegral height/2) - (playerHeight/2)) then 0 
                             else moveY
                    x' = x + moveX'
                    y' = y + moveY'               
                    
-- showMelee :: VideoGame -> VideoGame
-- bullets: take the picture passed in from render and do pictures [(pictures picture) ADD HERE]



main :: IO ()
main = play window background fps initialState render handleKeys update
 where
  update :: Float -> VideoGame -> VideoGame
  update seconds = movePlayer
