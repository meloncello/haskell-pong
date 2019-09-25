{-# LANGUAGE RecordWildCards #-}

module Lib 
( playMultiPong
, GameState(..)
) 
where

import Control.Monad
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

---------------------------------
data PongState = PongState 
  { ballVel     :: Point
  , ballLoc     :: Point  -- type Point = (Float, Float)
  , playerOneY  :: Float
  , playerTwoY  :: Float }

type Radius = Float
data GameWindow = GameWindow
  { area          :: Point 
  , paddle        :: Point 
  , radius    :: Radius
  , ballColor     :: Color
  , paddleColors  :: (Color, Color) }

data GameState = GameState
  { state   :: PongState
  , window  :: GameWindow 
  , paused  :: Bool }

data GameMode = SinglePlayer | Multiplayer
-----------------------------------------

mkWall :: Point -> Picture
mkWall (w, h) = let wall = color (light black) $ rectangleSolid w 5
                in pictures [ translate 0 (-h/2) wall, translate 0 (h/2) wall ]

mkDivider :: Float -> Float -> Picture
mkDivider h n = let 
                    circles = repeat (color white (circleSolid n))
                    zipped  = zip circles [(h/2), (h/2 - 10)..(-h/2)]
                  in pictures $ foldl (\ acc (pic, pos) -> translate 0 (pos - 5) pic : acc) [] zipped 

mkPaddle :: Point -> Color -> Float -> Picture
mkPaddle (w,h) cl n = pictures [  color yellow $ rectangleSolid (w+n) (h+n)
                               ,  color cl $ rectangleSolid w h ]

mkBall :: Radius -> Color -> Picture
mkBall r cl = pictures [color white $ circle (r+1), color cl $ circleSolid r]

mkArena :: GameWindow -> Picture
mkArena GameWindow{..} = pictures [ mkWall (w,h), mkDivider h (r/10) ]
  where
    (w,h) = area 
    (pw,ph) = paddle
    r = radius 
  
render :: GameState -> Picture
render GameState{..} = if paused
                 then pictures [ Blank, Text "Paused"]
                 else pictures  $ mkArena window : gamePics
  where
    gamePics = let f PongState{..} = [uncurry translate ballLoc ball, players state] in f state
    (w,h)                 = area window
    (pw,ph)               = paddle window
    ball                  = mkBall (radius window) (ballColor window)
    players PongState{..} = pictures  [ translate (-w/2) playerOneY $ mkPaddle (pw,ph) red  (pw/3)
                                      , translate (w/2)  playerTwoY $ mkPaddle (pw,ph) blue (pw/3) ]

-------------------------------------------------------------------------------

moveBall :: Float -> GameState -> GameState
moveBall time game = game { state = pstate { ballLoc = (x', y') } }
  where
    pstate    = state game
    (x,y)     = ballLoc pstate
    (vx, vy)  = ballVel pstate
    [x', y']  =  (\[a,b] -> [a + vx / 20, b + vy / 20]) $ (+ time) <$> [x,  y]

wallCollision :: GameState -> Bool
wallCollision GameState{ window = GameWindow{..},state = PongState{..}, ..} 
              = bottomCollision || topCollision
  where
        (_,y)           = ballLoc
        (_,h)           = area 
        bottomCollision = y - radius <= -h  / 2
        topCollision    = y + radius >=  h  / 2

wallBounce :: GameState -> GameState
wallBounce GameState{..}  = if wallCollision GameState{..}
                            then let ballVel = (vx, -vy)  in GameState{..}
                            else GameState{..}
  where
    (vx,vy)   = ballVel state

paddleCollision :: GameState -> Bool 
paddleCollision GameState{ window = GameWindow{..}
                        , state  = PongState{..} } = leftCollision || rightCollision
  where
  (x,y)     = ballLoc
  (width,_) = area
  (pw,ph)   = paddle 
  paddleX   = width + pw
  leftCollision   =  x - radius <=  paddleX  / 2 && hasSameY playerOneY
  rightCollision  =  x + radius >= -paddleX  / 2 && hasSameY playerTwoY
  hasSameY mp     =  y + radius > mp - ph / 2 && y - radius <= ph 

paddleBounce :: GameState -> GameState
paddleBounce GameState{..} = 
  if paddleCollision GameState{..}
  then  let (vx, vy) = ballVel state
        in GameState{..}
  else  GameState{..}
-------------------------------------------------------------------------------

mkInitialState :: Point -> GameState
mkInitialState (w,h) = let 
                        window = GameWindow { 
                          area         = (w,h)
                        , paddle       = (w/15, h/6)
                        , radius   = w/30
                        , ballColor    = yellow
                        , paddleColors = (blue, red) }
    
                        state = PongState {
                           ballVel      = (30,15)
                        ,  ballLoc     = (0,0)
                        ,  playerOneY  = 0
                        ,  playerTwoY  = 0 }

                        paused = False
                        
                        in GameState{..}

windowed :: Point -> Display
windowed (w,h)  = InWindow "Pong" (w',h') (100,100)
  where (w',h') = (floor w, floor h)

mkPongGame :: Point -> Color -> Int -> (Event -> GameState -> GameState) -> IO ()
mkPongGame (w,h) cl fps eventHandler = play 
                                        (windowed (w,h))
                                        cl                -- background color
                                        fps                
                                        (mkInitialState (w,h))
                                        render
                                        eventHandler
                                        update
  where
    update t = wallBounce . paddleBounce . moveBall t

createHandler :: Event -> (GameState -> Key -> PongState) -> (GameState -> Key -> PongState) -> GameState -> GameState
createHandler (EventKey key Down _ _) pTwoHandler pOneHandler game
  | key == SpecialKey KeyUp || key == SpecialKey KeyDown = game {state = playerOneHandler GameState{..} key}
  | key == Char 'w' || key == Char 's'                   = game {state = playerTwoHandler GameState{..} key} 
  | otherwise = game 
createHandler _ _ game = game

playerOneHandler :: GameState -> Key -> PongState
playerOneHandler GameState{..} (SpecialKey sk)
  | sk == KeyUp   &&  pOneY + (ph/2) + 15 <=  (h/2) = state { playerOneY = pOneY + 15 }
  | sk == KeyDown &&  pOneY - (ph/2) - 15 >= -(h/2) = state { playerOneY = pOneY - 15 }
  | otherwise = state
  where
    pOneY   = playerOneY state
    (_,h)   = area window
    (_,ph)  = paddle window

playerOneHandler game _ = state game

playerTwoHandler :: GameState -> Key -> PongState
playerTwoHandler GameState{..} (Char c) 
  | c == 'w' && pTwoY + (ph/2) + 10 <= (h/2)  = let playerTwoY = pTwoY + 10 in PongState{..}
  | c == 's' && pTwoY - (ph/2) - 10 >= -(h/2) = let playerTwoY = pTwoY - 10 in PongState{..}
  | otherwise = PongState{..}
  where
    pTwoY   = playerTwoY state
    (_,h)   = area window
    (_,ph)  = paddle window

multiPlayerHandler :: Event -> GameState -> GameState
multiPlayerHandler e = createHandler e playerOneHandler playerTwoHandler                                    

-----------------------------------------------------------------------------------
playMultiPong :: Point -> Color -> Int -> IO ()
playMultiPong (w,h) cl fps = mkPongGame (w,h) cl fps multiPlayerHandler

--playSoloPong :: Point -> Color -> Int -> IO ()
--playSoloPong (w,h) cl fps = mkPongGame (w,h) cl fps singlePlayerHandler
