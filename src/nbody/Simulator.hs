module Simulator where

import GHC.Float
import qualified Graphics.Gloss as Gl
import Graphics.Gloss.Data.ViewPort
import NBody

background :: Gl.Color
background = Gl.black

window :: Gl.Display
window = Gl.InWindow "Nice Window" (1000, 1000) (10, 10)

bodyColor :: Gl.Color
bodyColor = Gl.red

putBody :: Double -> Body -> Gl.Picture
putBody scale (Body (x, y) v m) = Gl.translate (double2Float (x / scale)) (double2Float (y / scale)) $ Gl.color bodyColor $ Gl.circleSolid 5

drawBodies :: Double -> [Body] -> Gl.Picture
drawBodies scale bs = Gl.pictures (map (putBody scale) bs)

fps :: Int
fps = 60

dt :: TimeStep
dt = 60 * 60

oneSec :: TimeStep
oneSec = 24 * 10

update :: ViewPort -> Float -> [Body] -> [Body]
update _ t = moveBodies (float2Double t) dt oneSec

main :: IO ()
main = Gl.simulate window background fps planets (drawBodies 10e8) update
