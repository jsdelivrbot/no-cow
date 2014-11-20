{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Concurrent
import Control.Monad

import Haste
import Haste.Foreign
 
newtype Pen = Pen Int deriving (Eq, Read, Show, Pack, Unpack)

newtype Box = Box String deriving (Eq, Read, Show, Pack, Unpack)

newtype Move = Move String deriving (Eq, Read, Show, Pack, Unpack)

data CowState = CowState
  { pen1 :: Box
  , pen2 :: Box
  , lastPen :: Pen
  , lastBox :: Box
  , is60 :: Bool
  } deriving (Show, Read, Eq)


-- Get the current state of the maze
getState :: IO CowState
getState = do
  (pen1, pen2, lastPen, lastBox, is60) <- ffi "(function() { return [appl.wac.pens.pen1.box.id, appl.wac.pens.pen2.box.id, appl.wac.last_pen_moved_ndx, appl.wac.last_move_box_id, appl.wac.rule60on]; })"
  return $ CowState pen1 pen2 lastPen lastBox is60

-- Set the maze's state to a state of your choosing
-- Making new states of your own is cheating!
setState :: CowState -> IO ()
setState (CowState pen1 pen2 lastPen lastBox is60) = go pen1 pen2 lastPen lastBox is60
  where go = ffi "(function(a,b,c,d,e) { appl.wac.setup(a,b,c,d,e); })" 

-- Get the list of possible moves from the current state
possibleMoves :: IO [Move]
possibleMoves = ffi "(function(){ var out = []; for (var n in appl.clickers) { var c = appl.clickers[n], p = c.box.pens_held, top = (c.top_or_bottom === 'top'); if (c.box && (p[1] && top) || (p[2] && !top)) out.push(n); }; return out; })"

-- Execute some particular move from the current state
executeMove :: Move -> IO ()
executeMove = ffi "(function(m) { appl.clickers[m].init_poise().execute() })"

-- Expose a few javascript things
delay :: Int -> IO () -> IO ()
delay = ffi "(function(n, action) { setTimeout(action, n); })"

random :: IO Float
random = ffi "(Math.random)"

-- A slow random walk around the state space
randomWalk :: IO ()
randomWalk = do
  moves <- possibleMoves
  unless (null moves) $ do
    randomNumber <- random
    let move = moves !! (floor $ (fromIntegral $ length moves) * randomNumber)
    executeMove move
    delay 1000 randomWalk

main = do
  -- export some useful functions, for playing around in the console
  export "printState" $ fmap show getState
  export "restoreState" $ setState . read
  export "possibleMoves" $ possibleMoves
  export "executeMove" $ executeMove
  -- start a random walk
  putStrLn "Loaded; starting random walk."
  randomWalk
