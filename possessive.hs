module Possessive where

import qualified Control.Concurrent.MVar as MV

data ZeroT = ZeroT
data SucT t = SucT t

-- xxx internal data for thread id's -- hidden
newtype AbstractThreadId = ATId Integer

asucc :: AbstractThreadId -> AbstractThreadId
asucc (ATId x) = ATId $ succ x

class Thread t where
    t :: t
    atid  :: t -> AbstractThreadId

instance Thread ZeroT where
    t = ZeroT
    atid _ = ATId 0
instance Thread t => Thread (SucT t) where
    t = SucT t
    atid = asucc . atid

-- how to hide this implementation?
-- K (writing_destination writing_action)
newtype K t a = K (t, IO (MV.MVar a), IO a)

spawn :: Thread t => K t ()
spawn = K (t, d, return ())
    where d = MV.newEmptyMVar
    
class IOFunctor w where
  wmap :: (a -> IO b) -> w a -> w b

instance Thread t => IOFunctor (K t) where
  wmap f (K (_, _, x)) = K (t, e, y)
    where
      e = MV.newEmptyMVar
      y = x >>= f

(>=>)       :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g     = \x -> f x >>= g

          
-- IOComonad. to be moved 
class IOFunctor w => IOComonad w where
   extract :: w a -> IO a
   duplicate :: w a -> w (w a)
   extend :: (w a -> IO b) -> w a -> w b

   extend f = g . duplicate
       where g = wmap f
   duplicate = extend return

-- xxx add law for IOComonad

-- | 'extend' with the arguments swapped. Dual to '>>=' for monads.
(=>>) :: IOComonad w => w a -> (w a -> IO b) -> w b
(=>>) = flip extend

-- | Injects a value into the comonad.
(.>>) :: IOComonad w => w a -> IO b -> w b
w .>> b = extend (\_ -> b) w

instance Thread t => IOComonad (K t) where
    extract (K (_, d, _)) = d >>= MV.readMVar
    duplicate (K (s, d, x)) =
        K (t, d', return $ K (s, d, x))
            where d' = MV.newEmptyMVar

-- (=>>) :: IOComonad w => w a -> (w a -> IO b) -> IO (w b)

    
-- instance F.Representable (K t) where
    


-- -- which remote to take?  Experiment!

remote :: Thread t => (a -> IO b) -> K t a -> K t b
remote = wmap


ret :: Thread t => a -> K t a
ret y = K (t, e, return y)
   where
     e = MV.newEmptyMVar


-- internal representation of a job
newtype L a = L (AbstractThreadId, IO (MV.MVar a), IO a)

ktol :: Thread t => K t a -> L a
ktol (K (th, d, f)) = L (atid th, d, f)

(//) :: Thread t => K t () -> [L ()] -> [L ()]
(//) hd tl = (ktol hd) : tl


-- the waitfree communication
-- waitfree :: K t a -> K s b -> Eigher (K t b) (K s a)
-- waitfree = ?

-- these are internal


-- action of sending the result to the shared box
-- this should not be visible to the user.
job_action :: L a -> IO ()
job_action (L (_, d, c)) = do
  d' <- d
  c' <- c
  MV.putMVar d' c'


    
--------------------------------------------------
-- example simple

-- Main shows "main\n"
-- Left shows "left\n"
-- and they are done.

l :: K (SucT ZeroT) ()
l = spawn .>> putStrLn "left"

m :: K ZeroT ()
m = spawn .>> putStrLn "main"

-- xxx joblist should contain no parenthesis

joblist :: [L ()]
joblist = l // (m // [])

    
-- example waitfree

-- a takes input
-- b takes input
-- either a or b


