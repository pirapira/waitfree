module Possessive where

import qualified Control.Concurrent.MVar as MV

data MainT = MainT
data Lfork t = Lfork t
data Rfork t = Rfork t

-- xxx internal data for thread id's
data Thread_ = MainT_ | Lfork_ Thread_ | Rfork_ Thread_

class Thread t where
    t :: t
    t_ :: t -> Thread_

instance Thread MainT where
    t = MainT
    t_ _ = MainT_
instance Thread t => Thread (Lfork t) where
    t = Lfork t
    t_ = Lfork_ . t_
instance Thread t => Thread (Rfork t) where
    t = Rfork t
    t_ = Rfork_ . t_

-- how to hide this implementation?
-- K (writing_destination writing_action)
newtype K t a = K (t, IO (MV.MVar a), IO a)

spawn :: Thread t => K t ()
spawn = K (t, d, return ())
    where d = MV.newEmptyMVar
    
class IOFunctor w where
  wmap :: (a -> IO b) -> w a -> IO (w b)

instance Thread t => IOFunctor (K t) where
  wmap f (K (_, _, x)) = return $ K (t, e, y)
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

-- xxx add law for IOComonad

-- | 'extend' with the arguments swapped. Dual to '>>=' for monads.
(=>>) :: IOComonad w => w a -> (w a -> IO b) -> IO (w b)
(=>>) = flip extend

-- | Injects a value into the comonad.
(.>>) :: IOComonad w => w a -> b -> IO (w b)
w .>> b = extend (\_ -> return b) w

instance Thread t => IOComonad (K t) where
    extract (K (_, d, _)) = d >>= MV.readMVar
    duplicate (K (s, d, x)) =
        return $ K (t, d', return $ K (s, d, x))
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
job (K (_, d, c)) = do
  d' <- d
  c' <- c
  MV.putMVar d' c'

--------------------------------------------------
-- example simple

-- Main shows "main\n"
-- Left shows "left\n"
-- and they are done.

spawnMain :: K MainT ()
spawnMain = spawn




-- example waitfree

-- a takes input
-- b takes input
-- either a or b


