module Possessive where

import qualified Control.Concurrent.MVar as MV

data MainT = MainT
data Lfork t = Lfork t
data Rfork t = Rfork t

class Thread t
instance Thread MainT
instance Thread t => Thread (Lfork t)
instance Thread t => Thread (Rfork t)

-- how to hide this implementation?
-- K (writing_destination writing_action)
newtype Thread t =>
    K t a = K (IO (MV.MVar a), IO a)

receive :: K t a -> IO a
receive (K (d, _)) = d >>= MV.readMVar

-- which remote to take?  Experiment!

remote :: (a -> IO b) -> K t a -> K t b
remote f (K (_, x)) = K (e, y)
  where
    y = x >>= f
    e = MV.newEmptyMVar

ret :: a -> K t a
ret y = K (e, return y)
  where
    e = MV.newEmptyMVar

-- remote :: (a -> IO b) -> K t a -> K t b
-- remote f (K (_, x)) = K (e, y)
--   where
--     y = x >>= f
--     e = y >>= MV.newMVar

-- these are internal

-- action of sending the result to the shared box
job :: K t a -> IO ()
job (K (d, c)) = do
  d' <- d
  c' <- c
  MV.putMVar d' c'
