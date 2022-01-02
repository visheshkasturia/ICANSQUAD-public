import qualified ImageTest as IT
import qualified NBodyTest as NBT
import qualified QuadTest as QT
import qualified VectorsTest as VT

main :: IO ()
main = do
  QT.main
  IT.main
  NBT.main
  VT.main
