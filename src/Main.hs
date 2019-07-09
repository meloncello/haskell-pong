module Main
(main) 
where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data PongState = PongState 
  { ballVel       :: Point
  , ballLoc       :: Point  -- type Point = (Float, Float)
  , player1Y      :: Float
  , player2Y      :: Float }

data GameWindow = GameWindow
  { width         :: Float
  , height        :: Float 
  , background    :: Color
  , paddle        :: Point } -- (width, height)

type Radius = Float 
data Ball = Ball
  { radius  :: Radius
  , ballColor   :: Color  }

data GameState = GameState
  { state   :: PongState
  , window  :: GameWindow
  , ball    :: Ball }

initialPongState :: PongState
initialPongState = PongState 
  { ballVel       = (15, -15)
  , ballLoc       = (15,-15)
  , player1Y      = 50
  , player2Y      = 0 }

gameWin :: GameWindow
gameWin = GameWindow
  { width         = 300
  , height        = 300
  , paddle        = (20, 100) 
  , background    = black }

gameBall :: Ball
gameBall = Ball
  { radius      = 20
  , ballColor   = yellow  }

gameState :: GameState
gameState   = GameState
  { state   = initialPongState  
  , window  = gameWin
  , ball    = gameBall  }

---------------------------------------------------------------------------
windowed :: String -> GameWindow -> Display
windowed title gwin = InWindow title (width', height') (100,100)
  where
    width' :: Int
    width'  = (floor $ width gwin)
    height' :: Int
    height' = (floor $ height gwin)

render :: GameState -> Picture
render gstate = pictures  [ ball''
                          , walls
                          , divider
                          , paddle'
                          , paddle'' ] 
  where
    pstate    = state gstate
    win       = window gstate
    ball'     = ball gstate
    width'    = width win
    height'   = height win   
    (pw, ph)  = paddle win     
    
    ball''    = uncurry translate (ballLoc pstate) $
                Color (ballColor ball') $ circleSolid (radius ball')

    mkPaddle cl  = pictures [ Color yellow $ uncurry rectangleSolid $ (\(a,b) n -> (a+n, b+n)) (pw,ph) 10
                            , Color cl $ rectangleSolid pw ph ]
    paddle'   = translate (-width' / 2) (player1Y pstate) $ mkPaddle red  --player 1
    paddle''  = translate ( width' / 2) (player2Y pstate) $ mkPaddle blue -- player 2
    
    walls = let wall offset = translate 0 offset $ color (light black) $ rectangleSolid width' 5
            in  pictures [wall (height' / 2), wall (-height' / 2)]
    divider = let circles = repeat (circleSolid 1)
                  zipped  = zip circles [(height'/2), (height'/2 - 10)..(-height'/2)]
               in color white $ pictures $ foldl (\acc (a,b) -> (translate 0 b a):acc) [] zipped 

moveBall :: Float -> GameState -> GameState
moveBall seconds gstate = gstate { state = pstate { ballLoc = (x',y') }}
  where
    pstate    = state gstate
    (x, y)    = ballLoc pstate 
    (vx, vy)  = ballVel pstate
    [x', y']  = (\(a,b) [x, y] -> [x+a, y+b]) (x,y) $ (*seconds) <$> [vx, vy] 

wallCollision :: Point -> Radius -> GameWindow -> Bool
wallCollision (_,y) radius gwin = bottomCollision || topCollision
  where
    width' = width gwin
    bottomCollision = y - radius <= -width' / 2
    topCollision    = y + radius >=  width' / 2

wallBounce :: GameState -> GameState
wallBounce gstate = if wallCollision (x,y) (radius $ ball gstate) (window gstate)
                      then gstate { state = pstate { ballVel = (x, -y) }}
                      else gstate
  where
    pstate  = state gstate
    (x,y)   = ballLoc pstate

paddleCollision :: Point -> Point -> Radius -> GameWindow -> Bool
paddleCollision (x,y) (mp1, mp2) radius gwin = leftCollision || rightCollision
  where
    width'          = width gwin
    (pw,ph)         = paddle gwin 
    leftCollision   = x - radius <=  (-width' + pw) / 2 && hasSameY mp1
    rightCollision  = x + radius >=  ( width' - pw) / 2 && hasSameY mp2
    hasSameY mp     = (y + radius > mp - ph / 2 && y - radius <= ph)

paddleBounce :: GameState -> GameState 
paddleBounce gstate = if paddleCollision (x,y) (p1Y,p2Y) (radius $ ball gstate) (window $ gstate)
                        then gstate { state = pstate { ballVel = (-x, y) }}
                        else gstate
    where
      pstate  = state gstate
      (x,y)   = ballLoc pstate
      p1Y     = player1Y $ pstate
      p2Y     = player2Y $ pstate

main :: IO ()
main = play (windowed "pong - test" gameWin) (background gameWin) 60 gameState render handleKeys update
  where
    frame :: Float -> Picture
    frame seconds = render $ moveBall seconds gameState
    
    update time = paddleBounce . wallBounce . moveBall time 
    
    handleKeys :: Event -> GameState -> GameState
    handleKeys (EventKey key Down _ _) game 
      | key == SpecialKey KeyUp || key == SpecialKey KeyDown  = player1Handler key
      | key == Char 'w' || key == Char 's'                    = player2Handler key
      | otherwise                                             = game
      where
        pstate  = state game
        win         = window game
        pOneY       = player1Y pstate
        pTwoY       = player2Y pstate
        halfPaddle  = (snd $ paddle win) / 2
        halfHeight  = (height win) / 2

        player1Handler key 
          | key == SpecialKey KeyUp && pOneY + halfPaddle + 10 <= halfHeight    = game { state = pstate { player1Y = pOneY + 10 } }
          | key == SpecialKey KeyDown && pOneY - halfPaddle - 10 >= -halfHeight = game { state = pstate { player1Y = pOneY - 10 } }
          | otherwise                                                           = game 
        player2Handler key
          | key == Char 'w' && pTwoY + halfPaddle + 10 <= halfHeight  = game { state = pstate { player2Y = pTwoY + 10 } }
          | key == Char 's' && pTwoY - halfPaddle - 10 >= -halfHeight = game { state = pstate { player2Y = pTwoY - 10 } }
          | otherwise                                                 = game
    handleKeys _ game = game                                         
