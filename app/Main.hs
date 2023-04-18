module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.IO.Unsafe

width, height, offset :: Int
width = round bgWidth - 5
height = 950
offset = 100

playerWidth, playerHeight, playerSpeed :: Float
playerWidth = 50
playerHeight = 75
playerSpeed = 5

meleeWidth, meleeHeight :: Float
meleeWidth = 60
meleeHeight = 10

smallBulletDims :: Float
smallBulletDims = 10

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
player = unsafePerformIO $ loadBMP "assets/orangeship3.bmp"


bgScrollSpeed :: Float
bgScrollSpeed = 450

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
    backgroundY :: Float,
    background2Y :: Float,
    shootTimer :: Int,
    bullets :: [Bullet],
    shootMachineGun :: Bool
  } deriving Show 
  
initialState :: VideoGame
initialState = Game
  { playerLoc = (0, 0),
    playerMovingUp = False,
    playerMovingRight = False,
    playerMovingDown = False,
    playerMovingLeft = False,
    meleeActive = False,
    backgroundY = 0,
    background2Y = bgHeight,
    shootTimer = 0,
    bullets = [],
    shootMachineGun = False
  }
  


render :: VideoGame -> Picture
render game = 
              pictures [translate 0 bgy bg, 
                        translate 0 bg2y bg2,
                        translate x y player, 
                        mkMelee leftX y,
                        mkMelee rightX y,
                        mkBullet (bullets game)
                        ]
               where
               bgy = backgroundY game
               bg2y = background2Y game
               (x,y) = playerLoc game

               leftX = x - 50
               rightX = x + 50
               meleeColor = if (meleeActive game) then blue else makeColor 0 0 0 0

               mkMelee :: Float -> Float -> Picture
               mkMelee x y = pictures [translate x y $ color meleeColor $ rectangleSolid meleeWidth meleeHeight]  

mkBullet :: [Bullet] -> Picture
mkBullet [] = pictures []
mkBullet (bullet:rest) = 
  pictures [drawnBullet, restOfBullets]
  where
    drawnBullet = translate (x bullet) (y bullet) $ color black $ rectangleSolid smallBulletDims smallBulletDims
    restOfBullets = mkBullet rest

background :: Color
background = white

data Bullet = Bullet 
  { x :: Float,
    y :: Float,
    xv :: Float,
    yv :: Float,
    damage :: Int
  } deriving Show

append :: Bullet -> [Bullet] -> [Bullet]
append b [] = [b]
append b (x:xs) = x : append b xs

addBullet :: VideoGame -> VideoGame
addBullet game = game { bullets = newBulletsList }
  where 
    time = shootTimer game
    (bx,by) = playerLoc game
    oldBullets = bullets game
    newBullet = Bullet {
        x = bx,
        y = by,
        xv = 0,
        yv = 30,
        damage = 50
      }
    newBulletsList = 
      if (time `mod` 10 == 1) 
        then append newBullet oldBullets
        else oldBullets

updateBullets :: VideoGame -> VideoGame
updateBullets game = game { bullets = newBullets }
  where
    newBullets = moveBullets (bullets game)

moveBullets :: [Bullet] -> [Bullet]
moveBullets [] = []
moveBullets (bullet:rest) = newBulletsList
  where
  -- add top wall collision detection here (I believe)
  newBullet = Bullet {
    x = (x bullet),
    y = (y bullet) + (yv bullet),
    xv = (xv bullet),
    yv = (yv bullet),
    damage = (damage bullet)
  }
  restOfBullets = moveBullets rest
  newBulletsList = append newBullet restOfBullets


updateTimer :: VideoGame -> VideoGame
updateTimer game = game { shootTimer = newShootTimerVal }
  where
    newShootTimerVal = 
      if (shootMachineGun game)
        then (shootTimer game) + 1
        else 0

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

handleKeys (EventKey (Char 'h') Down _ _) game = game {shootMachineGun = True}
handleKeys (EventKey (Char 'h') Up _ _) game = game {shootMachineGun = False}

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
  update seconds = updateBullets . addBullet . updateTimer . movePlayer . moveBG seconds
