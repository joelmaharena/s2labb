-- Define the Vec3 type
data Vec3 = Vec3 Double Double Double
  deriving (Show, Eq)

-- Implement vector operations using an instance of Num
instance Num Vec3 where
  (Vec3 x1 y1 z1) + (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)
  (Vec3 x1 y1 z1) - (Vec3 x2 y2 z2) = Vec3 (x1 - x2) (y1 - y2) (z1 - z2)
  (Vec3 x1 y1 z1) * (Vec3 x2 y2 z2) = Vec3 (x1 * x2) (y1 * y2) (z1 * z2) -- Not standard vector multiplication
  negate (Vec3 x y z) = Vec3 (-x) (-y) (-z)
  abs (Vec3 x y z) = Vec3 (abs x) (abs y) (abs z)
  signum (Vec3 x y z) = Vec3 (signum x) (signum y) (signum z)
  fromInteger n = Vec3 (fromInteger n) (fromInteger n) (fromInteger n)


dot :: Vec3 -> Vec3 -> Double
dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

scalar :: Double -> Vec3 -> Vec3 
scalar x (Vec3 x1 y1 z1) = Vec3 (x*x1) (x*y1) (x*z1)

reflect :: Vec3 -> Vec3 -> Vec3
reflect incoming normal = incoming - scalar (2 * dot incoming normal) normal 

removeNegativeZ :: [Vec3] -> [Vec3]
removeNegativeZ rays = filter (\(Vec3 x1 y1 z1) -> z1 > 0) rays

reflectAndRemoveNegativeZ :: Vec3 -> [Vec3] -> [Vec3]
reflectAndRemoveNegativeZ normal incomingRays = (removeNegativeZ . map (`reflect` normal)) incomingRays

