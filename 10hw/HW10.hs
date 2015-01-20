module Main where

import Graphics.Gloss.Interface.Pure.Game
import Data.Monoid

type Location = (Float, Float)
type Velocity = (Float, Float)
type Size = Float
type Score = Int

data State = NotStarted | Playing | Ended

data Paddle = Paddle { pad_height :: Size,
                       pad_loc :: Location,
                       pad_vel :: Velocity }
              deriving Show

data Ball = Ball { ball_rad :: Size,
                   ball_loc :: Location,
                   ball_vel :: Velocity }
            deriving Show

data World = World { w_opp :: Paddle,
                     w_player :: Paddle,
                     w_ball :: Ball,
                     w_pscore :: Score,
                     w_oscore :: Score,
                     w_state :: State }

playerSpeed :: Float
playerSpeed = 200

opponentSpeed :: Float
opponentSpeed = 200

windowWidth :: Int
windowWidth = 800

windowHeight :: Int
windowHeight = 540

windowWidthF :: Float
windowWidthF = fromIntegral windowWidth

windowHeightF :: Float
windowHeightF = fromIntegral windowHeight

winUpper :: Float
winUpper = windowHeightF/2

winLower :: Float
winLower = -winUpper


winRight :: Float
winRight = windowWidthF/2

winLeft :: Float
winLeft = -winRight

padHeight :: Size
padHeight = windowHeightF / 5

padWidth :: Size
padWidth = padHeight / 10

frameRate :: Int
frameRate = 50

opponent :: Paddle
opponent = Paddle { pad_height = padHeight,
                    pad_loc = (-windowWidthF / 2 + padWidth, 0),
                    pad_vel = (0, 0) }

player :: Paddle
player = Paddle { pad_height = padHeight,
                  pad_loc = (windowWidthF / 2 - padWidth, 0),
                  pad_vel = (0, 0) }

ball :: Ball
ball = Ball { ball_rad = padWidth, ball_loc = (0, 0), ball_vel = (200, 100) }

initialWorld :: IO World
initialWorld = do
  return $ World { w_opp = opponent,
                   w_player = player,
                   w_ball = ball,
                   w_pscore = 0,
                   w_oscore = 0,
                   w_state = NotStarted}

renderPaddle :: Paddle -> Picture
renderPaddle (Paddle {pad_height = height, pad_loc = (x, y)}) =
  color (light blue) (polygon path)
  where
    width = padWidth
    path = [(x-width, y+height/2), (x+width, y+height/2),
            (x+width, y-height/2), (x-width, y-height/2)]

renderBall :: Ball -> Picture
renderBall (Ball {ball_rad = r, ball_loc = (x, y)}) =
  color (light red) (translate x y $ thickCircle (r/2) r)

renderFrame :: Picture
renderFrame = color (dark white) $
              rectangleSolid windowWidthF windowHeightF

renderScores :: Int -> Int -> Picture
renderScores pscore oscore = color white $ translate 0 (winUpper*1.2) $
                             scale 0.2 0.2 $ Text "|" <>
                             (translate (windowWidthF*2) 0 $ Text $ show pscore) <>
                             (translate (-windowWidthF*2) 0 $ Text $ show oscore)

render :: World -> Picture
render (World { w_opp = o,
                w_player = p,
                w_ball = b,
                w_pscore = pscore,
                w_oscore = oscore,
                w_state = s })
  = renderFrame <> renderBall b <> renderPaddle o <> renderPaddle p <>
    renderScores pscore oscore

react :: Event -> World -> World
react ev w@(World { w_player = p,
                    w_state = state})
  = case ev of
     EventKey (SpecialKey KeyUp) Down _ _ ->
       w { w_player = p {pad_vel = (0, playerSpeed)} }
     EventKey (SpecialKey KeyDown) Down _ _ ->
       w { w_player = p {pad_vel = (0, -playerSpeed)} }
     EventKey (SpecialKey KeyUp) Up _ _ ->
       w { w_player = p {pad_vel = (0, 0)} }
     EventKey (SpecialKey KeyDown) Up _ _ ->
       w { w_player = p {pad_vel = (0, 0)} }
     EventKey (SpecialKey KeySpace) Down _ _ ->
       w { w_state = Playing }
     _ -> w

tryMovePaddle :: Float -> Paddle -> Paddle
tryMovePaddle elapsed p@(Paddle {pad_height = h,
                                 pad_loc = (x, y),
                                 pad_vel = (vx, vy)})
  | inBounds = p {pad_height = h, pad_loc = (x', y')}
  | otherwise = p
  where
    x' = x + vx*elapsed
    y' = y + vy*elapsed
    inBounds = (y' + h/2 <= winUpper) && (y' - h/2 >= winLower) -- &&
               -- (x' - padWidth >= winLeft) && (x' + padWidth <= winRight)

tryMoveBall :: Float -> World -> Ball
tryMoveBall elapsed w@(World {w_opp = o@(Paddle {pad_loc = (ox, oy)}),
                              w_player = p@(Paddle {pad_loc = (px, py)}),
                              w_ball = b@(Ball {ball_rad = r,
                                                ball_loc = (x, y),
                                                ball_vel = (vx, vy)})
                             })
  | collidedY = b {ball_vel = (vx, -vy)}
  | collidedOpp = collideBallPaddle b o
  | collidedPlayer = collideBallPaddle b p
  | otherwise = b {ball_loc = (x', y')}
  where
    x' = x + vx * elapsed
    y' = y + vy * elapsed
    collidedY = (y' + r > winUpper) || (y' - r < winLower)
    collidedOpp = x' - (r + 2*padWidth) < winLeft  &&
                  y' - (r + padHeight/2) < oy &&
                  y' + (r + padHeight/2) > oy
    collidedPlayer = x' + (r + 2*padWidth) > winRight  &&
                     y' - (r + padHeight/2) < py &&
                     y' + (r + padHeight/2) > py


collideBallPaddle :: Ball -> Paddle -> Ball
collideBallPaddle b@(Ball {ball_loc = (bx, by),
                           ball_vel = (bvx, bvy)})
                  p@(Paddle {pad_loc = (px, py),
                             pad_vel = (pvx, pvy)})
  = b {ball_vel = (-bvx, bvy + (by - py) * 5)}

step :: Float -> World -> World
step elapsed w@(World {w_opp = o@(Paddle {pad_loc = (ox, oy)}),
                       w_player = p,
                       w_ball = b@(Ball {ball_loc = (bx, by)}),
                       w_pscore = pscore,
                       w_oscore = oscore,
                       w_state = Playing})
  | (pscore >= 7) || (oscore >= 7) = w {w_state = Ended}
  | bx + padWidth > winRight = w {w_oscore = oscore + 1,
                                  w_ball = ball {ball_vel = (-200, 100)}}
  | bx - padWidth < winLeft = w {w_pscore = pscore + 1,
                                 w_ball = ball}
  | otherwise = w {w_opp = tryMovePaddle elapsed o {pad_vel = oppVel},
                   w_player = tryMovePaddle elapsed p,
                   w_ball = tryMoveBall elapsed w}
  where
    oppVel
      | by > oy = (0, opponentSpeed)
      | by < oy = (0, -opponentSpeed)
      | otherwise = (0, 0)
step _ w = w

main :: IO()
main = do
  world <- initialWorld
  play (InWindow "Pong" (windowWidth, windowHeight) (100, 100))
        (dark $ dark white) frameRate world render react step
