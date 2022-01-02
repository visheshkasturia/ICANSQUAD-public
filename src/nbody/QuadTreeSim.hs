module QuadTreeSim where

import GHC.Float
import qualified Graphics.Gloss as Gl
import Graphics.Gloss.Data.ViewPort
import NBody
import NBodyQuad
import Quad
import Simulator

lineColor :: Gl.Color
lineColor = Gl.white

putBox :: Double -> Box -> Gl.Picture
putBox scale box@(Rect (xMin, yMin) (xMax, yMax)) =
  Gl.translate (double2Float (fst (midpoint box) / scale)) (double2Float (snd (midpoint box) / scale)) $
    Gl.color lineColor $ Gl.rectangleWire (double2Float ((xMax - xMin) / scale)) (double2Float ((yMax - yMin) / scale))

drawBoxAndBodies :: Double -> Q Body -> Gl.Picture
drawBoxAndBodies scale qb = Gl.pictures (map (putBody scale) (getLeafValues qb) ++ map (putBox scale) (getAllBoxes qb))

updateQt :: ViewPort -> Float -> Q Body -> Q Body
updateQt _ t = moveBodiesQuad (float2Double t) dt oneSec

bodyQT :: Q Body
bodyQT = qtPolarBodies

main :: IO ()
main = Gl.simulate window background fps qtPlanets (drawBoxAndBodies 10e8) updateQt
