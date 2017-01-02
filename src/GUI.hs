module GUI (
  startGUI
) where

import Prelude hiding (mapM_,length)

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (
  GLfloat,
  Color4(..),
  clearColor,
  ($=),
  viewport,
  matrixMode,
  Position(..),
  Size(..),
  MatrixMode(..),
  loadIdentity,
  perspective,
  ortho2D,
  clear,
  ClearBuffer(ColorBuffer,DepthBuffer),
  preservingMatrix,
  rotate,
  renderPrimitive,
  Vector3(..),
  vertex,
  flush,
  rasterPos,
  color,
  Vertex3(..),
  PrimitiveMode(Points,Lines,Triangles))

import qualified Graphics.UI.GLFW as GLFW
import Graphics.UI.GLFW (
  defaultWindowHints,
  makeContextCurrent,
  setWindowSizeCallback,
  setKeyCallback,
  setWindowCloseCallback,
  setMouseButtonCallback,
  setCursorPosCallback,
  pollEvents,
  Key(Key'Escape,Key'Left,Key'Right),
  KeyState(KeyState'Pressed),
  destroyWindow,
  terminate,
  MouseButton(MouseButton'1),
  MouseButtonState(MouseButtonState'Pressed,MouseButtonState'Released),
  swapBuffers)

import Data.IORef (
  newIORef,
  readIORef,
  modifyIORef,
  writeIORef)

import Control.Concurrent (MVar,newMVar,takeMVar,putMVar,readMVar,forkIO)

import Control.Monad (forever)
import Data.Functor (void)
import System.Exit (exitWith,ExitCode(..))
import Data.Vector.Generic (mapM_,length)
import Text.Printf (printf)

import Model (
  Model(..),
  torusModel,
  computeDistances,
  addLinesAndTriangles)

import Graphics.UI.GLUT.Initialization as GLUT (initialize)
import Graphics.UI.GLUT.Fonts

windowResized win w h = do
  viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
  matrixMode $= Projection
  loadIdentity
  perspective 60 1 1 3200
  ortho2D 0 (realToFrac w) (realToFrac h) 0

-- | callback to handle key presses
makeKeyPressHandler distances indices model dThresh = handler
  where handler win Key'Escape _ KeyState'Pressed _ = shutdown win
        handler win Key'Left   _ KeyState'Pressed _ = void $ forkIO $ alterThreshAndUpdateModel (-0.01)
        handler win Key'Right  _ KeyState'Pressed _ = void $ forkIO $ alterThreshAndUpdateModel 0.01
        handler _   _          _ _                _ = return ()

        alterThreshAndUpdateModel amt = do
          modifyIORef dThresh (max 0 . (+ amt))
          d <- readIORef dThresh
          m <- readMVar model
          m' <- addLinesAndTriangles distances indices d m
          takeMVar model
          putMVar model m'
          return ()


-- | closes the window and exits the application
shutdown win = do
  destroyWindow win
  terminate
  void $ exitWith ExitSuccess

-- while the mouse button is down, the mouse position listener should
-- change the rotation angle
mouseClickListener cursorPos alpha beta win MouseButton'1 MouseButtonState'Pressed _ = do
  (x,y) <- readIORef cursorPos
  a     <- readIORef alpha
  b     <- readIORef beta
  setCursorPosCallback win $ Just $ \_ x' y' -> do
    writeIORef alpha $ a + realToFrac (x' - x) / 2
    writeIORef beta  $ b + realToFrac (y' - y) / 2
-- when the mouse button is not held down, the callback should simply
-- remember the position
mouseClickListener cursorPos _ _ win MouseButton'1 MouseButtonState'Released _ = do
  setCursorPosCallback win $ Just $ \_ x y -> writeIORef cursorPos (x,y)
mouseClickListener _ _ _ _ _ _ _ = return ()

-- the direction vectors of alpha and beta rotations
aDir, bDir :: Vector3 GLfloat
aDir = Vector3 0 1 0
bDir = Vector3 1 0 0

-- | this action is responsible for drawing the model into the scene
drawScene alpha beta dThresh model win = do
  m <- readMVar model
  a <- readIORef alpha
  b <- readIORef beta
  clear [ColorBuffer, DepthBuffer]
  loadIdentity

  let vertices  = modelVertices  m
      lines     = modelLines     m
      triangles = modelTriangles m

  preservingMatrix $ do
    -- first rotate by the necessary amount
    rotate a aDir
    rotate b bDir
    -- now render the vertices, lines, and triangles
    color (Color4 0.5 0.5 0.5 0 :: Color4 Float)    
    renderPrimitive Triangles $ mapM_ (\(v1,v2,v3) -> vertex v1 >> vertex v2 >> vertex v3) triangles    
    color (Color4 1 1 1 0 :: Color4 Float)
    renderPrimitive Points    $ mapM_ vertex vertices
    renderPrimitive Lines     $ mapM_ (\(v1,v2) -> vertex v1 >> vertex v2) lines


  -- print the number of vertices, lines, and triangles in the top
  -- left corner
  rasterPos (Vertex3 (-1.0) 0.95 0.0 :: Vertex3 GLfloat)
  renderString Helvetica12 $ printf "vertices: %d, edges: %d, triangles: %d" (length vertices) (length lines) (length triangles)

  -- render the distance threshold value at the lower left corner
  d <- readIORef dThresh
  rasterPos (Vertex3 (-1.0) (-1.0) 0.0 :: Vertex3 GLfloat)
  renderString Helvetica12 $ printf "d: %.2f" d

  flush
  swapBuffers win


createWindow = do
  defaultWindowHints
  Just win <- GLFW.createWindow 1000 1000 "Simplicial Complex Visualizer" Nothing Nothing
  makeContextCurrent $ Just win
  return win

startGUI :: IO ()
startGUI = do
  GLUT.initialize "" []
  GLFW.init

  -- | the position of the cursor
  pos   <- newIORef ((0,0) :: (Double,Double))
  -- | the rotation angles
  beta  <- newIORef (0 :: GLfloat)
  alpha <- newIORef (0 :: GLfloat)

  model <- torusModel 1000 >>= newMVar
  (distances,indices) <- computeDistances <$> readMVar model
  dThresh <- newIORef (0.0 :: Float)

  win <- createWindow
  setKeyCallback         win $ Just $ makeKeyPressHandler distances indices model dThresh
  setWindowCloseCallback win $ Just shutdown
  setWindowSizeCallback  win $ Just windowResized
  setMouseButtonCallback win $ Just $ mouseClickListener pos alpha beta
  setCursorPosCallback   win $ Just $ \_ x y -> writeIORef pos (x,y)

  -- set the background color of the scene
  clearColor $= Color4 0 0 0 0

  forever $ do
    pollEvents
    drawScene alpha beta dThresh model win

