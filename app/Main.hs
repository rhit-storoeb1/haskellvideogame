module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.IO.Unsafe
type Size = (Float, Float)
        
        
width, height, offset :: Int
width = round bgWidth - 5
height = 950
offset = 100

playerWidth, playerHeight, playerSpeed :: Float
playerWidth = 51
playerHeight = 90
playerSpeed = 5

meleeWidth, meleeHeight :: Float
meleeWidth = 60
meleeHeight = 10

bgHeight, bgWidth :: Float
bgHeight = 2572
bgWidth = 619


window :: Display
window = InWindow "Game" (width, height) (offset, offset)

bg :: Picture
bg = unsafePerformIO $ loadBMP "assets/volcanoBGbigger.bmp"

bg2 :: Picture
bg2 = unsafePerformIO $ loadBMP "assets/volcanoBGbigger.bmp"

player :: Picture
player = unsafePerformIO $ loadBMP "assets/orangeship32.bmp"


bgScrollSpeed :: Float
bgScrollSpeed = 450

bombTimerMax :: Float
bombTimerMax = 125

moveBG seconds game = game {backgroundY = y',
                            background2Y = y''}
                       where
                        y = backgroundY game
                        y2 = background2Y game
                        y' = if y <= -bgHeight
                             then y2+bgHeight- bgScrollSpeed * seconds
                             else y - bgScrollSpeed * seconds
                        y'' = if y2 <= -bgHeight
                              then y+bgHeight- bgScrollSpeed * seconds
                              else y2 - bgScrollSpeed * seconds
                        

data VideoGame = Game
  { playerLoc :: (Float, Float) ,
    playerMovingUp :: Bool,
    playerMovingRight :: Bool,
    playerMovingDown :: Bool,
    playerMovingLeft :: Bool,
    meleeActive :: Bool,
    droppingBomb :: Bool,
    bombLoc :: (Float, Float),
    bombTimer :: Float,
    explosionActive :: Bool,
    explosionDuration :: Float,
    backgroundY :: Float,
    background2Y :: Float
  } deriving Show 
  
initialState :: VideoGame
initialState = Game
  { playerLoc = (0, 0),
    playerMovingUp = False,
    playerMovingRight = False,
    playerMovingDown = False,
    playerMovingLeft = False,
    meleeActive = False,
    droppingBomb = False,
    bombLoc = (0, 0),
    bombTimer = bombTimerMax,
    explosionActive = False,
    explosionDuration = 10,
    backgroundY = 0,
    background2Y = bgHeight
  }
  


render :: VideoGame -> Picture
render game = 
              pictures [translate 0 bgy bg, 
                        translate 0 bg2y bg2,
                        translate x y player, 
                        mkMelee leftX y,
                        mkMelee rightX y,
                        if (droppingBomb game)
                         then translate bx by $ color bombColor $ circleSolid 10
                         else blank,
                        if (explosionActive game && explosionDuration game > 0) 
                         then translate bx by $ color explosionColor $ circleSolid explosionRadius
                         else blank
                        ]
               where
               bgy = backgroundY game
               bg2y = background2Y game
               (x,y) = playerLoc game
               
               (bx, by) = bombLoc game
               frameNum = floor $ bombTimer game
               bombColor = if (frameNum `div` 10) `mod` 2 == 0 then black else red
               explosionColor = yellow
               explosionRadius = 50

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

handleKeys (EventKey (Char 'h') Down _ _) game = if droppingBomb game
                                                  then game
                                                  else game {droppingBomb = True, bombLoc = playerLoc game, explosionDuration = 10, explosionActive = False}

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

updateBomb :: VideoGame -> VideoGame
updateBomb game = if (droppingBomb game)
                  then 
                   if (bombTimer game) <= 0
                   then game {droppingBomb = False, bombTimer = bombTimerMax}
                   else game {bombTimer = (bombTimer game) - 1}
                  else game
                  
checkExplosion :: VideoGame -> VideoGame                 
checkExplosion game =
      if (bombTimer game <= 0)
       then game { explosionActive = True , explosionDuration = 10}
       else if (explosionActive game && explosionDuration game <= 0)
        then game { explosionActive = False, explosionDuration = 0 }
         else game { explosionDuration = max 0 $ explosionDuration game - 1 }



main :: IO ()
main = play window background fps initialState render handleKeys update
 where
  update :: Float -> VideoGame -> VideoGame
  update seconds = movePlayer . moveBG seconds . updateBomb . checkExplosion




















