module Main
(main) 
where

import Graphics.Gloss

data PongState = PongState 
                        { ballVel       :: Point
                        , ballLoc       :: Point  -- type Point = (Float, Float)
                        , player1Y      :: Float
                        , player2Y      :: Float }

initialState :: PongState
initialState = PongState 
                        { ballVel       = (10, -30)
                        , ballLoc       = (15,-15)
                        , player1Y      = 40
                        , player2Y      = 40 } 

width, height, ballRadius, paddleHeight, paddleWidth :: Num number => number
width         = 300
height        = 300
ballRadius    = 20
paddleHeight  = 100
paddleWidth   = 20

windowed :: String -> Display
windowed title = InWindow title (width, height) (100, 100)

background :: Color
background = black

render :: PongState -> Picture
render state = pictures [ ball
                        , walls
                        , mkPaddle rose (-width / 2)  $ player1Y state
                        , mkPaddle blue (width  / 2)  $ player2Y state ] 
  where
    ball = uncurry translate (ballLoc state) $ color white $ circleSolid ballRadius
    mkPaddle cl x y = let paddle = rectangleSolid paddleWidth paddleHeight
                      in translate x y $ pictures [ color yellow $ rectangleSolid (paddleWidth + 10) $ paddleHeight + 10
                                                  , color cl paddle ]
    walls = let wallColor = greyN 0.5
                wall offset = translate 0 offset $ color wallColor $ rectangleSolid width 10
            in  pictures [wall (height/2), wall (-height/2)]

moveBall :: Float -> PongState -> PongState
moveBall seconds state = state { ballLoc = (x',y') }
  where
    (x, y)   = ballLoc state 
    (vx, vy) = ballVel state
    [x', y'] = (\(a,b) [x, y] -> [x+a, y+b]) (x,y) $ (*seconds) <$> [vx, vy] 

wallCollision :: Point -> Float -> Bool
wallCollision (_,y) radius = bottomCollision || topCollision
  where
    bottomCollision = y - radius <= -width / 2
    topCollision    = y + radius >=  width / 2

wallBounce :: PongState -> PongState
wallBounce state = if wallCollision (ballLoc state) ballRadius
                      then state { ballVel = (x, -y) }
                      else state
  where
    (x,y) = ballLoc state
    


main :: IO ()
main = simulate (windowed "pong - test") background 60 initialState render update
  where
    frame :: Float -> Picture
    frame seconds = render $ moveBall seconds initialState
    update _ time = wallBounce . moveBall time
  
