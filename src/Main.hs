module Main
(main) 
where

import Graphics.Gloss

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
  { ballVel       = (10, -30)
  , ballLoc       = (15,-15)
  , player1Y      = 0
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

width'  = width gameWin  
height' = height gameWin 
(pw,ph) = paddle gameWin 
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
    pstate  = state gstate
    win     = window gstate
    ball'   = ball gstate
    width'  = width win
    height' = height win   

    ball''  = uncurry translate (ballLoc pstate) $
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

wallCollision :: Point -> Radius -> Bool
wallCollision (_,y) radius = bottomCollision || topCollision
  where
    bottomCollision = y - radius <= -width' / 2
    topCollision    = y + radius >=  width' / 2

wallBounce :: GameState -> GameState
wallBounce gstate = if wallCollision (x,y) (radius $ ball gstate)
                      then gstate { state = pstate { ballVel = (x, -y) }}
                      else gstate
  where
    pstate  = state gstate
    (x,y)   = ballLoc pstate

paddleCollision :: Point -> Radius -> (Float, Float) -> Bool
paddleCollision (x,y) radius (middlePL, middlePR) = leftCollision || rightCollision
  where
    leftCollision             = x + radius >=  width'/2 - pw / 2 && hasSameY middlePL
    rightCollision            = x - radius >=  width'/2 + pw / 2 && hasSameY middlePR
    hasSameY mp = (y + radius < ph || y + radius > mp - ph / 2 || y - radius <= ph)
paddleBounce :: GameState -> GameState 
paddleBounce gstate = if paddleCollision (x,y) (radius $ ball gstate) (player1Y pstate, player2Y pstate)
                        then gstate { state = pstate { ballVel = (-x, y) }}
                        else gstate
    where
      pstate  = state gstate
      (x,y)   =  ballLoc pstate

main :: IO ()
main = simulate (windowed "pong - test" gameWin) (background gameWin) 60 gameState render update
  where
    frame :: Float -> Picture
    frame seconds = render $ moveBall seconds gameState
    update _ time = paddleBounce . wallBounce . moveBall time
     
