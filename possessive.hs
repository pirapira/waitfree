

import Control.Concurrent (ThreadId, forkIO, killThread)
import qualified Control.Concurrent.MVar as MV
import qualified Data.Map as Map
import GHC.IO.Handle (hFlush)
import System.IO (stdout)

data ZeroT = ZeroT
data SucT t = SucT t

-- xxx internal data for thread id's -- hidden
type AbstractThreadId = Integer

class Thread t where
    t :: t
    atid  :: t -> AbstractThreadId

instance Thread ZeroT where
    t = ZeroT
    atid ZeroT = 0
instance Thread t => Thread (SucT t) where
    t = SucT t
    atid (SucT x) = succ $ atid x

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

mute :: K t a -> IO ()
mute _ = return ()

-- xxx add law for IOComonad

-- | 'extend' with the arguments swapped. Dual to '>>=' for monads.
(=>>) :: IOComonad w => w a -> (w a -> IO b) -> w b
(=>>) = flip extend

-- | Injects a value into the comonad.
(.>>) :: IOComonad w => w a -> IO b -> w b
w .>> b = extend (\_ -> b) w

instance Thread t => IOComonad (K t) where
    extract (K (_, d, _)) = do
      putStrLn "waiting"
      val <- (d >>= MV.readMVar)
      putStrLn "got value"
      return val
    extend trans (K (s, d, f)) = K (t, d', f')
        where
          d' = MV.newEmptyMVar
          f' = do
            d_ <- d'
            f_ <- f
            MV.putMVar d_ f_
            trans $ K (s, d, f)

-- (=>>) :: IOComonad w => w a -> (w a -> IO b) -> IO (w b)

    
-- -- which remote to take?  Experiment!

remote :: Thread t => (a -> IO b) -> K t a -> K t b
remote = wmap


ret :: Thread t => IO a -> K t a
ret y = K (t, e, y)
   where
     e = MV.newEmptyMVar


-- internal representation of a job
newtype L a = L (AbstractThreadId, IO ())

ktol :: Thread t => Show a => K t a -> L a
ktol (K (th, d, f)) = L (atid th, job_action d f)

infixr 5 //

(//) :: Thread t => K t () -> [L ()] -> [L ()]
(//) hd tl = (ktol hd) : tl


-- the waitfree communication
-- waitfree :: K t a -> K s b -> Eigher (K t b) (K s a)
-- waitfree = ?



-- these are internal

-- action of sending the result to the shared box
-- this should not be visible to the user.
job_action :: Show a => IO (MV.MVar a) -> IO a -> IO ()
job_action d c = do
  d' <- d
  c' <- c
  putStrLn $ show c'
  MV.putMVar d' c'

type JobChannel = [IO ()]
-- Just x is a next job
-- Nothing means this is the end

worker :: JobChannel -> MV.MVar () -> IO ()
worker [] fin = MV.putMVar fin ()
worker (hd : tl) fin = do
  hd
  worker tl fin

-- ThreadPool is finite map
type ThreadPool =
    Map.Map AbstractThreadId (ThreadId, MV.MVar ())

type JobPool = 
    Map.Map AbstractThreadId JobChannel

execute :: [L ()] -> IO ()
execute = spawnPool >=> waitThread

spawnPool :: [L ()] -> IO ThreadPool
spawnPool = run . constructJobPool

run :: JobPool -> IO ThreadPool
run = Map.foldrWithKey threadSpawn $ return Map.empty

threadSpawn :: AbstractThreadId -> JobChannel -> IO ThreadPool -> IO ThreadPool
threadSpawn aid ch p = do
    p' <- p
    fin <- MV.newEmptyMVar
    thid <- forkIO $ worker ch fin
    return $ Map.insert aid (thid, fin) p'

constructJobPool :: [L ()] -> JobPool
constructJobPool [] = Map.empty
constructJobPool (L (aid, action) : tl) =
  Map.insertWith (++) aid [action] rest
     where
       rest = constructJobPool tl

waitThread :: ThreadPool -> IO ()
waitThread = Map.fold threadWait $ return ()

threadWait :: (ThreadId, MV.MVar ()) -> IO () -> IO ()
threadWait (thid, fin) w = do
    MV.readMVar fin
    putStrLn "detect finish"
    killThread thid
    w
    
--------------------------------------------------
-- example embarrasingly parallel

l :: K (SucT ZeroT) ()
l = spawn .>> putStrLn "one"

m :: K ZeroT ()
m = spawn .>> putStrLn "zero"

embarassingly_parallel = l // m // []    


--------------------------------------------------
-- example ping_pong

reader :: K ZeroT String
reader = ret $ do
           putStrLn $ "reader start"
           hFlush stdout
           str <- getLine
           putStrLn $ "reader got: " ++ str
           return str

reader' :: K ZeroT ()
reader' = extend mute reader
         
writer :: K (SucT ZeroT) ()
writer = ret $ do
           putStrLn $ "writer start"
           str <- extract reader
           putStrLn $ "writer got" ++ str
           
reader_writer = reader' // writer // []

main :: IO ()
main = execute $ reader_writer
    
-- example waitfree

-- a takes input
-- b takes input
-- either a or b


