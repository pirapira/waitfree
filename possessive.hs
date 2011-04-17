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
newtype Thread t => K t a = K (IO (MV.MVar a), IO a)

class IOFunctor w where
  wmap :: (a -> IO b) -> w a -> IO (w b)

instance IOFunctor (K t) where
  wmap f (K (_, x)) = return $ K (e, y)
    where
      e = MV.newEmptyMVar
      y = x >>= f

(>=>)       :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g     = \x -> f x >>= g

          
-- IOComonad. to be moved 
class IOFunctor w => IOComonad w where
   extract :: w a -> IO a
   duplicate :: w a -> IO (w (w a))
   extend :: (w a -> IO b) -> w a -> IO (w b)

   extend f =  duplicate >=> wmap f
--   duplicate = extend id

-- (=>>) :: IOComonad w => w a -> (w a -> IO b) -> IO (w b)

    
-- instance F.Representable (K t) where
    


-- -- which remote to take?  Experiment!

-- remote :: (a -> IO b) -> K t a -> K t b
-- remote f (K (_, x)) = K (e, y)
--   where
--     y = x >>= f
--     e = MV.newEmptyMVar

-- ret :: a -> K t a
-- ret y = K (e, return y)
--   where
--     e = MV.newEmptyMVar

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
