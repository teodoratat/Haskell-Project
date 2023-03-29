module Vec3.Vec3Ops where

class Vec3Ops v where
  x :: v -> Double
  y :: v -> Double
  z :: v -> Double

  constructor :: Double -> Double -> Double -> v

  mapX :: (Double -> Double) -> v -> v
  mapX f v = constructor (f $ x v) (y v) (z v)

  mapY :: (Double -> Double) -> v -> v
  mapY f v = constructor (x v) (f $ y v) (z v)

  mapZ :: (Double -> Double) -> v -> v
  mapZ f v = constructor (x v) (y v) (f $ z v)

  toTuple :: v -> (Double, Double, Double)
  toTuple v = (x v, y v, z v)

  fromTuple :: (Double, Double, Double) -> v
  fromTuple (a, b, c) = constructor a b c

  convert :: Vec3Ops v1 => v -> v1
  convert = fromTuple . toTuple

  add :: v -> v -> v
  add v1 v2 = constructor (x v1 + x v2) (y v1 + y v2) (z v1 + z v2)

  neg :: v -> v
  neg v = constructor (- x v) (- y v) (- z v)

  sub :: v -> v -> v
  sub v1 v2 = add v1 (neg v2)

  timesConst :: v -> Double -> v
  timesConst v1 t = constructor (x v1 * t) (y v1 * t) (z v1 * t)

  times :: v -> v -> v
  times v1 v2 = constructor (x v1 * x v2) (y v1 * y v2) (z v1 * z v2)

  divideConst :: v -> Double -> v
  divideConst v1 t = timesConst v1 (1 / t)

  lenSqrd :: v -> Double
  lenSqrd v1 = (x v1) ^ 2 + (y v1) ^ 2 + (z v1) ^ 2

  len :: v -> Double
  len v1 = sqrt $ lenSqrd v1

  get :: (Eq a, Num a) => a -> v -> Double
  get 0 = x
  get 1 = y
  get 2 = z
  get _ = error "Invalid index"

  dot :: v -> v -> Double
  dot v1 v2 = x v1 * x v2 + y v1 * y v2 + z v1 * z v2

  cross :: v -> v -> v
  cross v1 v2 = constructor vx vy vz
    where
      vx = y v1 * z v2 - z v1 * y v2
      vy = z v1 * x v2 - x v1 * z v2
      vz = x v1 * y v2 - y v1 * x v2

  unit :: v -> v
  unit v1 = v1 `divideConst` (len v1)