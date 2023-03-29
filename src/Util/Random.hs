module Util.Random where

import Control.Monad
import Data.Bits
import qualified Data.Word as W
import Vec3.Vec3
import Vec3.Vec3Ops

newtype Random = Random W.Word64 deriving (Show)

newtype Rnd g a = Rnd (g -> (a, g))

runRandom :: (RandomSeed s) => s -> Rnd s a -> a
runRandom seed (Rnd f) = fst $ f seed

evalRandom :: (RandomSeed s) => s -> Rnd s a -> s
evalRandom seed (Rnd f) = snd $ f seed

varyRandom :: Random -> Int -> Random
varyRandom (Random r) v = Random $ (iterate nextXor r) !! v

class RandomSeed a where
  newSeed :: a

instance RandomSeed Random where
  newSeed = newRandom 0x248729837123

instance Functor (Rnd g) where
  fmap f (Rnd a) = Rnd $ \s ->
    let (v, seed) = a s
     in (f v, seed)

instance (RandomSeed g) => Applicative (Rnd g) where
  pure a = Rnd $ \s -> (a, s)
  (Rnd f) <*> (Rnd v) = Rnd $ \s ->
    let (g, seed) = f s
        (x, seed') = v seed
     in (g x, seed')

instance (RandomSeed g) => Monad (Rnd g) where
  return a = Rnd $ \s -> (a, s)
  (Rnd a) >>= f = Rnd $ \s ->
    let (b, seed) = a s
        Rnd g = f b
     in g seed

nextXor :: W.Word64 -> W.Word64
nextXor w =
  let a = w `xor` (w `shiftR` 12)
      b = a `xor` (a `shiftR` 25)
      c = b `xor` (b `shiftR` 27)
   in c * 0x2545F4914F6CDD1D

newRandom :: W.Word64 -> Random
newRandom seed = Random seed

nextDouble :: Rnd Random Double
nextDouble = Rnd nd
  where
    maxW = maxBound :: W.Word64
    nd (Random w) =
      (fromIntegral w / fromIntegral maxW, Random $ nextXor w)

nextDoubleRange :: Double -> Double -> Rnd Random Double
nextDoubleRange lo hi = do
  d <- nextDouble
  return $ lo + (hi - lo) * d

nextInt :: Rnd Random Int
nextInt = Rnd ni
  where
    maxW = maxBound :: W.Word64
    maxI = maxBound :: Int
    ni (Random w) =
      (fromIntegral w `div` ((fromIntegral maxW) `div` maxI), Random $ nextXor w)

nextIntRange :: Int -> Int -> Rnd Random Int
nextIntRange lo hi = do
  n <- nextInt
  return (lo + n `mod` (hi - lo))

randomVec :: Rnd Random Vec3
randomVec = do
  vx <- nextDouble
  vy <- nextDouble
  vz <- nextDouble
  return $ Vec3 vx vy vz

randomVecRange :: Double -> Double -> Rnd Random Vec3
randomVecRange lo hi = do
  vx <- nextDoubleRange lo hi
  vy <- nextDoubleRange lo hi
  vz <- nextDoubleRange lo hi
  return $ Vec3 vx vy vz

randomVecInUnitSphere :: Rnd Random Vec3
randomVecInUnitSphere = do
  v <- randomVecRange (-1) 1
  if lenSqrd v >= 1
    then randomVecInUnitSphere
    else return v

randomVecInHemisphere :: Vec3 -> Rnd Random Vec3
randomVecInHemisphere n = do
  inUnitS <- randomVecInUnitSphere
  if inUnitS `dot` n > 0
    then return inUnitS
    else return $ neg inUnitS

randomUnitVector :: Rnd Random Vec3
randomUnitVector = unit `fmap` randomVecInUnitSphere

randomInUnitDisk :: Rnd Random Vec3
randomInUnitDisk = do
  vx <- nextDoubleRange (-1) 1
  vy <- nextDoubleRange (-1) 1
  let v = vec vx vy 0
  if lenSqrd v >= 1
    then randomInUnitDisk
    else return v

randomSample :: a -> [a] -> Rnd Random a
randomSample a [] = return a
randomSample a as = do
  let l = length as + 1
  idx <- nextIntRange 0 l
  return $ (a : as) !! idx