{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Model (
  Model(..),
  torusModel,
  computeDistances,
  addLinesAndTriangles,
  writeModel,
  readModel,
  makeVertex,
  makeModel
) where

import Data.Int (Int32)
import System.Random (newStdGen,split,randomRs)
import System.IO (withFile,IOMode(..),hPutStrLn,hGetLine)
import Data.Vector (Vector,(!))
import Control.Monad (forM_)
import Control.Monad.ST
import Data.Ord (comparing)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import Data.Vector.Algorithms.Merge (sortBy)
import Graphics.Rendering.OpenGL (Vertex3(..),GLfloat)
import Simplex (makeSimplicialComplex,toList)

type P3d = Vertex3 GLfloat

-- | A representation of the model 
data Model = Model { modelVertices  :: !(Vector P3d)
                   , modelLines     :: !(Vector (P3d,P3d))
                   , modelTriangles :: !(Vector (P3d,P3d,P3d))
                   }

-- | random points on a torus. TODO fix the sampling strategy to make
-- this uniform over the surface
torus :: Int -> IO [Vertex3 GLfloat]
torus n = do
  (g1,g2) <- split <$> newStdGen
  let samples = take n $ zip (randomRs (0,2*pi) g1) (randomRs (0,2*pi) g2)
  return $ fmap f samples
    where r1 = 0.5  -- distance from center to middle of tube
          r2 = 0.25 -- radius of the tube
          f (theta, phi) = Vertex3 x y z
            where x = cos phi * (r2 * cos theta + r1)
                  y = sin phi * (r2 * cos theta + r1)
                  z = r2 * sin theta

-- | generate a model by sampling n points randomly from the surface of a torus                
torusModel :: Int -> IO Model
torusModel n = do
  vertices <- torus n
  return $ Model (V.fromList vertices) V.empty V.empty

-- | computes the distances from each vertex to each other vertex
-- sorted from nearest to farthest. the output consists of two vectors
-- each of which represents a matrix in row major format. row i,
-- column j of the vector containing Floats corresponds to the
-- distance from vertex j to the vertex given by the i,j element of
-- the vector containing Int32s
computeDistances :: Model -> (Vector Float, Vector Int32)
computeDistances m =
  let vs = modelVertices m
   in V.unzip . V.concat $ map (sortDistances . flip distances vs) $ V.toList vs

-- | really the squared distance but ain't no one got time to type
-- 'squared' everywhere
distance :: P3d -> P3d -> Float
distance (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2) =
  let x = x1 - x2
      y = y1 - y2
      z = z1 - z2
   in x*x + y*y + z*z

distances :: P3d -> Vector P3d -> Vector Float
distances p ps = V.map (distance p) ps

sortDistances :: Vector Float -> Vector (Float, Int32)
sortDistances ps = runST $ do
  let n = V.length ps
      ps' = V.zip ps $ V.enumFromN 0 n
  arr <- V.thaw ps'
  sortBy (comparing fst) arr
  V.freeze arr
  

-- | write a model data structure to a file
writeModel :: Model -> FilePath -> IO ()
writeModel m path = withFile path WriteMode $ \handle -> do
  -- write the number of vertices to the first line
  hPutStrLn handle (show $ V.length (modelVertices m))
  -- write the vertices, one per line
  V.forM_ (modelVertices m) $ \vertex -> hPutStrLn handle (show vertex)
  
-- | read a model data structure from a file
readModel :: FilePath -> IO Model
readModel path = withFile path ReadMode $ \handle -> do
  s <- fmap read $ hGetLine handle :: IO Int
  vertices <- M.new s :: IO (M.IOVector P3d)
  forM_ [0..s-1] $ \i -> do
    v <- fmap read $ hGetLine handle :: IO P3d
    M.write vertices i v
  vertices' <- V.freeze vertices
  return $ Model vertices' V.empty V.empty

-- | build a simplicial complex on the vertices of the model with the given
-- threshold distance, then add lines and triangles to the model's geometry
-- to visualize the complex
addLinesAndTriangles :: Monad m
  => Vector Float     -- | the sorted square distance matrix
  -> Vector Int32     -- | the indices matrix
  -> Float            -- | the threshold distance
  -> Model            -- | the model to update
  -> m Model          -- | model with lines between points less than d apart
addLinesAndTriangles distances indices d m = do
  let vs = modelVertices m
      (s1, s2) = makeSimplicialComplex vs d distances indices
      lines = V.map ((\[p1,p2] -> (vs ! p1, vs ! p2)) . Simplex.toList) $ s1
      triangles = V.map ((\[p1,p2,p3] -> (vs ! p1, vs ! p2, vs ! p3)) . Simplex.toList) $ s2
  return $ m { modelLines = lines, modelTriangles = triangles }



-- stuff for interactive use

makeVertex :: Float -> Float -> Float -> Vertex3 Float
makeVertex = Vertex3

makeModel :: [Vertex3 Float] -> Model
makeModel vs = Model (V.fromList vs) V.empty V.empty
