Module Lib (playMultiPong) where

import Graphics.Gloss
import Graphics.Gloss.Pure.Game

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
  , ballRadius    :: Radius
  , ballColor     :: Color
  , paddleColors  :: (Color, Color) }

data GameState = GameState
  { state   :: PongState
  , window  :: GameWindow 
  , paused  :: Bool }

-----------------------------------------

mkWall :: Point -> Picture
mkWalls (w, h) = let wall = color grey $ rectangleSolid w 5
                in pictures [ translate 0 (-h/2) wall, translate 0 (h/2) wall ]

mkDivider :: Float -> Float -> Picture
mkDivider (h, n) = let circles = repeat (circleSolid n)
                    zipped  = zip circles [(height/2), (height/2 - 10)..(-height/2)]
                in pictures $ foldl (\(pic, pos) -> translate 0 pos pic)

mkPaddle :: Point -> Color -> Float -> Picture
mkPaddle (w,h) cl n = pictures [  color yellow $ rectangleSolid (w+n) (h+n)
                               ,  color cl $ rectangleSolid w h ]

mkBall :: Radius -> Color -> Picture
mkBall r cl = pictures [color white $ circle (r+1), color cl $ circleSolid r]

mkArena :: GameWindow -> Picture
mkArena gwin = pictures [ mkWalls (w,h), mkDivider (h,r) ]
  where
    (w,h) = area gwin
    (pw,ph) = paddle gwin
    r = ballRadius gwi
  
render :: GameState -> Picture
render game = if (paused game)
                 then pictures [ Blank, Text "Paused"]
                 else pictures  [ mkArena gwin
                                , mkBall (ballRadius gwin) (ballColor gwin)
                                , playerOne
                                , playerTwo ]
  where
    gwin      = window game 
    (w,h)     = area gwin
    (pw,ph)   = paddle gwin
    pOneY     = playerOneY $ state game
    pTwoY     = playerTwoY $ state game
    playerOne = translate (-w/2) pOneY $ mkPaddle (pw,ph) cl  (pw/3)
    playerTwo = translate (w/2)  pTwoY $ mkPaddle (pw,ph) cl' (pw/3)

-------------------------------------------------------------------------------
moveBall :: Float -> GameState -> GameState
moveBall time game = game { state = pstate { ballLoc = (x', y') } }
  where
    pstate    = state game
    (x,y)     = ballLoc state
    (vx, vy)  = ballVel state
    [x', y']  = (*y) <$> (+x) <$> [vx*time, vy*time]

wallCollision :: GameWindow -> Bool
wallCollision gwin = bottomCollision || topCollision
  where
        (_,y)           = ballLoc gwin
        (w,_)           = area gwin
        radius          = ballRadius gwin
        bottomCollision = y - radius <= -w  / 2
        topCollision    = y + radius >=  w  / 2

wallBounce :: GameState -> GameState
wallBounce gstate = if wallCollision (x,y) (window state)
                      then gstate { state = pstate { ballVel = (x, -y) } }
                      else gstate
  where
    pstate  = state gstate
    (x,y)   = ballLoc pstate

paddleBounce :: GameState -> GameState 
paddleCollision game  
  | leftCollision || rightCollision = game { state = pstate { ballVel = (-vx, vy) } }
  | otherwise = game
  where
        gwin            = window game
        pstate          = state game

        (width,_)       = area gwin
        (pw,ph)         = paddle gwin
        mp1             = playerOneY game
        mp2             = playerTwoY game
        (x,y)           = ballLoc pstate
        (vx, vy)        = ballVel pstate

        leftCollision   = x - radius <=  (-width + pw) / 2 && hasSameY mp1
        rightCollision  = x + radius >=  ( width - pw) / 2 && hasSameY mp2
        hasSameY mp     = (y + radius > mp - ph / 2 && y - radius <= ph)

-------------------------------------------------------------------------------

mkInitialState :: Point -> GameState
mkinitialState (w,h) = GameState { state :: pstate, window :: win, paused = False }
  where
    win = GameWindow  { area         = (w,h)
                      , paddle       = (w/15, h/6)
                      , ballRadius   = w/30
                      , ballColor    = yellow
                      , paddleColors = (blue, red)
                      , ballColor    = yellow }
    
    pstate = PongState {  ballVel     = (30,15)
                       ,  ballLoc     = (0,0)
                       ,  playerOneY  = 0
                       ,  playerTwoY  = 0 }

windowed :: Point -> Display
windowed (w,h)  = InWindow "Pong" (w',h')
  where (w',h') = (floor w, floor h)

mkPongGame :: Point -> Color -> Int -> (Event -> a -> a) -> IO ()
mkPongGame (w,h) cl fps eventHandler = play 
                                        (windowed (w,h))
                                        cl                -- background color
                                        fps                
                                        mkInitialState (w,h)
                                        render
                                        eventHandler
                                        update
  where
    update = wallBounce . paddleBounce . moveBall time

createHandler :: Event -> (Key -> GameState) -> GameState -> GameState
createHandler (EventKey key Down _ _) playerTwoHandler game
  | key == SpecialKey Up || key == SpecialKey Down  = playerOneHandler key
  | key == Char 'w' || key == Char 's' = playerTwoHandler key
  | otherwise = game
  where
    pstate = state game
    pOneY = playerOneY pstate
    gwin = window game
    (_,h) = area gwin
    (_,ph) = paddle gwin
    playerOneHandler :: Key -> GameState
    playerOneHandler (SpecialKey sk) 
      | sk == Up && pOneY + (ph/2) + 10 <= (h/2) =  game 
                                                      { state = 
                                                        pstate  
                                                          { playerOneY = pOneY + 10 } }
      | sk == Down && pOneY - (ph/2) - 10 <= -(h/2) = game 
                                                        { state =
                                                          pstate
                                                            { playerOneY = pOneY - 10 } }
      | otherwise = game

singlePlayerHandler :: Event -> GameState -> GameState
singlePlayerHandler e game =  let pTwoHandler _ = game
                              in createHandler e pTwoHandler game

multiPlayerHandler :: Event -> GameState -> GameState
multiPlayerHandler e game = createHandler e pTwoHandler game
  where
    pstate  = state game
    gwin    = window game
    (_,h)   = area gwin
    (_,ph)  = paddle gwin
    pTwoY   = playerTwoY pstate    

    pTwoHandler :: Key -> GameState
    pTwoHandler (Char c)
      | c == 'w' && pTwoY + (ph/2) + 10 <= (h/2) = game 
                                                    { state = 
                                                      pstate 
                                                        { playerTwoY = pTwoY + 10 } }
      | c == 's' && pTwoY - (ph/2) - 10 >= -(h/2) = game 
                                                      { state = 
                                                        pstate 
                                                          { playerTwoY = pTwoY - 10 } }
      | otherwise = game
-----------------------------------------------------------------------------------
playMultiPong :: Point -> Color -> Int -> IO ()
playMultiPong (w,h) cl fps = mkPongGame (w,h) cl fps multiPlayerHandler

playSoloPong :: Point -> Color -> Int -> IO ()
playSoloPong (w,h) cl fps = mkPongGame (w,h) cl (fps) singlePlayerHandler
