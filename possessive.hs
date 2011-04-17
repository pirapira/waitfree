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
   duplicate = extend return

-- add nice notation for IOComonad
-- add law for IOComonad

-- | 'extend' with the arguments swapped. Dual to '>>=' for monads.
(=>>) :: IOComonad w => w a -> (w a -> IO b) -> IO (w b)
(=>>) = flip extend

-- | Injects a value into the comonad.
(.>>) :: IOComonad w => w a -> b -> IO (w b)
w .>> b = extend (\_ -> return b) w

instance IOComonad (K t) where
    extract (K (d, _)) = d >>= MV.readMVar
    duplicate (K (d, x)) =
        return $ K (d', return $ K (d, x))
            where d' = MV.newEmptyMVar

-- (=>>) :: IOComonad w => w a -> (w a -> IO b) -> IO (w b)

    
-- instance F.Representable (K t) where
    


-- -- which remote to take?  Experiment!

-- remote :: (a -> IO b) -> K t a -> IO (K t b)
-- remote = wmap


-- ret :: a -> K t a
-- ret y = K (e, return y)
--   where
--     e = MV.newEmptyMVar

-- remote :: (a -> IO b) -> K t a -> K t b
-- remote f (K (_, x)) = K (e, y)
--   where
--     y = x >>= f
--     e = y >>= MV.newMVar


-- the waitfree communication
-- waitfree :: K t a -> K s b -> Eigher (K t b) (K s a)
-- waitfree = ?

-- these are internal

-- action of sending the result to the shared box
job :: K t a -> IO ()
job (K (d, c)) = do
  d' <- d
  c' <- c
  MV.putMVar d' c'

-- example

-- a takes input
-- b takes input
-- either a or b