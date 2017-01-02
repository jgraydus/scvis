{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simplex (
  Simplex,
  toList,
  simplex0,
  simplex1,
  simplex2,
  BoundaryMatrix(..),
  boundaryMatrix,
  rank,
  makeSimplicialComplex
) where

import GHC.TypeLits
import Data.Int (Int32)
import Data.Word (Word64)
import Data.Bits (setBit)
import Data.IntSet (IntSet,isSubsetOf)
import qualified Data.IntSet as IntSet
import Data.Vector (Vector,(!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad (when,forM_)
import Control.Monad.Reader (MonadIO)
import Data.Maybe (catMaybes)
import Graphics.Rendering.OpenGL (Vertex3(..))


-- | represents a non-oriented 1-, 2-, or 3-simplex
newtype Simplex (n :: Nat) = Simplex IntSet deriving Show

toList (Simplex s) = IntSet.toList s

-- | aka vertex
simplex0 :: Int -> Simplex 0
simplex0 = Simplex . IntSet.singleton

-- | aka edge. arguments must be distinct integers but no error checking is done
simplex1 :: Int -> Int -> Simplex 1
simplex1 x y = Simplex $ IntSet.fromList [x,y]

-- | aka triangle. arguments must be distinct integers but no error checking is done
simplex2 :: Int -> Int -> Int -> Simplex 2
simplex2 x y z = Simplex $ IntSet.fromList [x,y,z]

size :: Simplex n -> Int
size (Simplex s) = IntSet.size s

-- | the matrix representation of a boundary homomorphism 
data BoundaryMatrix (n :: Nat) = B { rows :: Int, cols :: Int, _data :: Vector Word64 } deriving Show

-- | create the nth boundary homomorphism matrix given the
-- (n-1)-simplices and the n-simplices of a complex
boundaryMatrix :: Vector (Simplex (n-1)) -> Vector (Simplex n) -> BoundaryMatrix n
boundaryMatrix s1 s2 = B { rows = r, cols = c, _data = d }
  where
    -- number of rows and columns
    r = V.length s1
    c = V.length s2

    -- 64 columns are packed into each Word64
    wordsPerRow = c `div` 64 + if c `mod` 64 /= 0 then 1 else 0

    -- index into vector and index into Word64
    index x y = (wordsPerRow * x + y `div` 64, y `mod` 64)

    -- pack the matrix into a vector
    d = V.create $ do
      -- initialize a vector to 0s
      m <- MV.replicate (r * wordsPerRow) 0
      -- for each row (n-simplex) and each column (n+1 simplex)
      flip V.imapM_ s1 $ \row (Simplex s1') -> do
        flip V.imapM_ s2 $ \col (Simplex s2') -> do
          -- when the row represents a simplex that is a face of the
          -- simplex the column represents, write a 1 to that position
          when (s1' `isSubsetOf` s2') $ do
            let (i,b) = index row col
            MV.modify m (`setBit` b) i
      return m

-- | compute the rank of the given boundary homomorphism matrix
rank :: MonadIO m => BoundaryMatrix n -> m Int
rank = error "not implemented"

-- | returns 1-, and 2- simplices of Rips complex determined by thresh on vertices
makeSimplicialComplex ::
     Vector (Vertex3 Float)      -- | vertex coordinates
  -> Float                         -- | distance threshold
  -> Vector Float                -- | distance array
  -> Vector Int32                -- | indices corresponding to distance array
  -> (Vector (Simplex 1), Vector (Simplex 2))
makeSimplicialComplex vertices thresh distances indices = (s1,s2)
  where
    n  = V.length vertices
    
    s1 = V.fromList $ [0..n-1] >>= make1
    s2 = V.fromList $ [0..n-1] >>= make2

    sq = thresh * thresh

    adj = V.create $ do
      m <- MV.new n
      forM_ [0..n-1] $ \i -> do
        let j  = V.length . V.filter (< sq) . V.drop 1 $ V.slice (i*n) n distances
            vs = fmap fromIntegral . V.toList . V.take j . V.drop 1 $ V.slice (i*n) n indices
        MV.write m i (IntSet.fromList vs)
      return m

    make1 i = fmap (simplex1 i) . filter (> i) . IntSet.toList $ adj ! i

    make2 i = do
      let adjToi = adj ! i
      j <- IntSet.toList . IntSet.filter (> i) $ adjToi
      k <- IntSet.toList . IntSet.intersection adjToi . IntSet.filter (> j) $ adj ! j
      return $ simplex2 i j k
