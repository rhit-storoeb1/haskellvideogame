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

bombTimerMax :: Float
bombTimerMax = 125

explosionColor :: Color
explosionColor = yellow
explosionRadius :: Float
explosionRadius = 50

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
appendEnemy :: Enemy -> [Enemy] -> [Enemy]
appendEnemy b [] = [b]
appendEnemy b (x:xs) = x : appendEnemy b xs
                        
data Enemy = Enemy {
loc :: (Float, Float),
velocity :: (Float, Float),
exploding:: Bool,
explodeDuration :: Float,
health :: Float,
updateEnemy :: (Enemy -> Enemy),
enemyPic :: Picture,
size :: (Float, Float),
enemyBullets :: [Bullet]
}

updateEnemyPosition :: Enemy -> Enemy
updateEnemyPosition enemy = enemy { loc = (x', y')}
                         where
                          (x,y) = loc enemy
                          (xv, yv) = velocity enemy
                          x' = x + xv
                          y' = y + yv

bounceOffWall :: Enemy -> Enemy
bounceOffWall enemy = enemy { velocity = (xv', yv), enemyBullets = newEnemyBullets }            
                         where
                          (x,y) = loc enemy
                          (xv, yv) = velocity enemy
                          hitWall = (x >= (fromIntegral width / 4) && xv > 0) || (x <= -(fromIntegral width / 4) && xv < 0)
                          xv' = if hitWall
                                 then -xv                               
                                 else xv
                          newEnemyBullets = if hitWall 
                            then enemyShootBullet enemy 0 (-4) 10
                            else (enemyBullets enemy) 

enemyShootBullet :: Enemy -> Float -> Float -> Float -> [Bullet]
enemyShootBullet enemy bxv byv bdamage = newEnemyBullets
  where
    (ex,ey) = loc enemy
    (ew,eh) = size enemy
    newBullet = Bullet {
      x = ex,
      y = ey-(eh/2),
      xv = bxv,
      yv = byv,
      damage = round bdamage
    }
    newEnemyBullets = append newBullet (enemyBullets enemy)

enemyOneUpdater :: Enemy -> Enemy
enemyOneUpdater = bounceOffWall . updateEnemyPosition . updateEnemyBullets

zigZagger = Enemy {
loc = (-50, fromIntegral height + 100),
velocity = (5, -2),
exploding = False,
explodeDuration = 15,
health = 50,
updateEnemy = enemyOneUpdater,
enemyPic = unsafePerformIO $ loadBMP "assets/enemy1.bmp",
size = (74, 100),
enemyBullets = []
}

mkEnemy :: [Enemy] -> Picture
mkEnemy [] = pictures []
mkEnemy (enemy:rest) = 
  pictures [drawnEnemy, restOfEnemies]
  where
    (x,y) = loc enemy
    drawnEnemy = translate x y $ enemyPic enemy
    restOfEnemies = mkEnemy rest

mkEnemyBullets :: [Enemy] -> Picture
mkEnemyBullets [] = pictures []
mkEnemyBullets (enemy:rest) = pictures [drawnEnemyBullet, restOfEnemyBullets]
  where
    drawnEnemyBullet = drawEnemyBullet (enemyBullets enemy)
    restOfEnemyBullets = mkEnemyBullets rest

drawEnemyBullet :: [Bullet] -> Picture
drawEnemyBullet [] = pictures []
drawEnemyBullet bullets = pictures [mkBullet bullets]

updateEnemyBullets :: Enemy -> Enemy
updateEnemyBullets enemy =
  if length (enemyBullets enemy) == 0 
      then enemy 
    else newEnemy
      where
        bullet = head (enemyBullets enemy)
        rest = tail (enemyBullets enemy)
        newBullet = Bullet {
          x = (x bullet),
          y = (y bullet) + (yv bullet),
          xv = (xv bullet),
          yv = (yv bullet),
          damage = (damage bullet)
        }
        isNotOOB = (y newBullet) <= (fromIntegral height/2) && (y newBullet) >= -(fromIntegral height/2)

        ne = updateEnemyBullets enemy { enemyBullets = rest }
        newEnemy = 
          if isNotOOB
            then enemy { enemyBullets = append newBullet (enemyBullets ne) }
            else enemy { enemyBullets = (enemyBullets ne) } 

updatePlayerHealth :: [Enemy] -> Float
updatePlayerHealth (firstEnemy:rest) = if (exploding firstEnemy)
                                        then 1 + (updatePlayerHealth rest)
                                        else (updatePlayerHealth rest)
updatePlayerHealth [] = 0

updateEnemies :: VideoGame -> VideoGame
updateEnemies game = game {enemies = newEnemy, playerHealth = ph}
                      where
                       e = enemies game
                       newEnemy = updateEnemiesList e game
                       ph = (playerHealth game) - (updatePlayerHealth (enemies game))
                       
detectEnemyCollision :: Enemy -> (Float, Float) -> Enemy
detectEnemyCollision enemy (x,y) = enemy {exploding = isExploding, velocity = (vx, vy), enemyPic = pic, explodeDuration = d}
                                    where                                    
                                     (ex, ey) = (loc enemy)
                                     (w, h) = (size enemy)
                                     isExploding = ((x + playerWidth / 2 >= ex - w / 2) && 
                                        (x - playerWidth / 2 <= ex + w / 2) && 
                                        (y + playerHeight / 2 >= ey - h / 2) && 
                                        (y - playerHeight / 2 <= ey + h / 2)) || (exploding enemy)
                                     (vx, vy) = if isExploding
                                                 then (0, 0)
                                                 else velocity enemy
                                     pic = if isExploding
                                            then color explosionColor $ circleSolid explosionRadius
                                            else enemyPic enemy              
                                     d = if isExploding
                                            then (explodeDuration enemy) - 1
                                            else (explodeDuration enemy)

detectEnemyBulletCollision :: Enemy -> [Bullet] -> Enemy
detectEnemyBulletCollision e [] = e
detectEnemyBulletCollision enemy (bullet:rest) = newEnemy
  where
    (ex,ey) = (loc enemy)
    (ew,eh) = (size enemy)
    bx = x bullet
    by = (y bullet)      
    newEnemy = if bulletColliding 
      then enemy { health = ((health enemy) - (fromIntegral (damage bullet))) }
      else detectEnemyBulletCollision enemy rest
    bulletColliding = ((ex + ew / 2 >= bx - smallBulletDims / 2) && 
                       (ex - ew / 2 <= bx + smallBulletDims / 2) && 
                       (ey + eh / 2 >= by - smallBulletDims / 2) && 
                       (ey - eh / 2 <= by + smallBulletDims / 2))
                                                                                                                                    
updateEnemiesList :: [Enemy] -> VideoGame -> [Enemy]
updateEnemiesList (firstEnemy:rest) game = newEnemyList
                                      where
                                       (x,y) = playerLoc game
                                       updateFunc = updateEnemy firstEnemy
                                       newFirstEnemy = updateFunc firstEnemy
                                       newFirstEnemyCollision = detectEnemyCollision newFirstEnemy (x,y)
                                       newFirstEnemyBulletCollision = detectEnemyBulletCollision newFirstEnemyCollision (bullets game)
                                       restUpdated = updateEnemiesList rest game                                       
                                       newEnemyList = if ((explodeDuration firstEnemy)==0 || ((health newFirstEnemyBulletCollision) <= 0))
                                                         then restUpdated
                                                         else appendEnemy newFirstEnemyBulletCollision restUpdated
updateEnemiesList [] _ = []

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
    background2Y :: Float,
    shootTimer :: Int,
    bullets :: [Bullet],
    shootMachineGun :: Bool,
    enemies :: [Enemy],
    playerHealth :: Float
  }
  
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
    background2Y = bgHeight,
    shootTimer = 0,
    bullets = [],
    shootMachineGun = False,
    enemies = [zigZagger],
    playerHealth = 100
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
                         else blank,
                        mkBullet (bullets game),
                        mkEnemy (enemies game),
                        translate ((fromIntegral width / 2)-65) ((fromIntegral height / 2)-50) healthText,
                        mkEnemyBullets (enemies game)
                        ]
               where
               healthText :: Picture
               healthText = color white $ scale 0.25 0.25 $ text (show (round (playerHealth game)))
               bgy = backgroundY game
               bg2y = background2Y game
               (x,y) = playerLoc game
               
               (bx, by) = bombLoc game
               frameNum = floor $ bombTimer game
               bombColor = if (frameNum `div` 10) `mod` 2 == 0 then black else red
               

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
        damage = 5
      }
    newBulletsList = 
      if (time `mod` 10 == 1) 
        then append newBullet oldBullets
        else oldBullets
-- todo: consolidate updateBullets and moveBullets
updateBullets :: VideoGame -> VideoGame
updateBullets game = ng
  where
    ng = moveBullets game

moveBullets :: VideoGame -> VideoGame
moveBullets game = 
    if length (bullets game) == 0 
      then game 
    else newGame
      where
        bullet = head (bullets game)
        rest = tail (bullets game)
        newBullet = Bullet {
          x = (x bullet),
          y = (y bullet) + (yv bullet),
          xv = (xv bullet),
          yv = (yv bullet),
          damage = (damage bullet)
        }

        isNotOOB = (y newBullet) <= (fromIntegral height/2)

        enemyColliding = bulletCollidingWithEnemy newBullet (enemies game)

        ng = moveBullets game { bullets = rest }
        newGame = 
          if isNotOOB && not(enemyColliding)
            then game { bullets = append newBullet (bullets ng) }
            else game { bullets = (bullets ng) } 

bulletCollidingWithEnemy :: Bullet -> [Enemy] -> Bool
bulletCollidingWithEnemy bullet [] = False
bulletCollidingWithEnemy bullet (enemy:rest) = isColliding
  where
    (ex, ey) = loc enemy
    (ew, eh) = size enemy
    bx = x bullet
    by = y bullet
    isColliding = ((ex + ew / 2 >= bx - smallBulletDims / 2) && 
                   (ex - ew / 2 <= bx + smallBulletDims / 2) && 
                   (ey + eh / 2 >= by - smallBulletDims / 2) && 
                   (ey - eh / 2 <= by + smallBulletDims / 2))

-- detectHeroShot :: VideoGame -> VideoGame
-- detectHeroShot game = game { enemies = newEnemies, playerHealth = newHealth }
--   where

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

handleKeys (EventKey (Char 'j') Down _ _) game = if droppingBomb game
                                                  then game
                                                  else game {droppingBomb = True, bombLoc = playerLoc game, explosionDuration = 10, explosionActive = False}
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
  update seconds = updateBullets . addBullet . updateTimer . movePlayer . moveBG seconds . updateBomb . checkExplosion . updateEnemies




















