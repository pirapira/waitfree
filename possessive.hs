

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
-- K (writing_action)
-- (destination should be a premise)
newtype K t a = K (t, IO a)

spawn :: Thread t => K t ()
spawn = K (t, return ())
    
class IOFunctor w where
  wmap :: (a -> IO b) -> w a -> w b

instance Thread t => IOFunctor (K t) where
  wmap f (K (_, x)) = K (t, y)
    where
      y = x >>= f

(>=>)       :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g     = \x -> f x >>= g

          
-- IOComonad. to be moved 
class IOFunctor w => MVComonad w where
   -- extract sharedBox producer = (newProducer, receiver)
   extract :: MV.MVar a -> w a -> (w a, IO a)
   duplicate :: w a -> w (w a)
   extend :: (w a -> IO b) -> w a -> w b

   extend f = g . duplicate
       where g = wmap f
   duplicate = extend return

mute :: K t a -> IO ()
mute _ = return ()

-- xxx add law for IOComonad

-- | 'extend' with the arguments swapped. Dual to '>>=' for monads.
(=>>) :: MVComonad w => w a -> (w a -> IO b) -> w b
(=>>) = flip extend

-- | Injects a value into the comonad.
(.>>) :: MVComonad w => w a -> IO b -> w b
w .>> b = extend (\_ -> b) w

instance Thread t => MVComonad (K t) where
    extract box (K (c, f)) = (newProducer, receiver)
      where
        newProducer = K (c, f')
        f' = do
          x <- f
          MV.putMVar box x
          return x
        receiver = do 
          putStrLn "waiting_for_remote_value"
          val <- MV.readMVar box
          putStrLn "got value"
          return val
    extend trans r@(K (_, f)) = K (t, g)
      where
        g = do
          f
          trans r

-- -- which remote to take?  Experiment!

remote :: Thread t => (a -> IO b) -> K t a -> K t b
remote = wmap

ret :: Thread t => IO a -> K t a
ret y = K (t, y)

-- internal representation of a job
newtype L = L (AbstractThreadId, IO ())

ktol :: Thread t => K t () -> L
ktol (K (th, f)) = L (atid th, f)

infixr 5 //

(//) :: Thread t => K t () -> [L] -> [L]
(//) hd tl = (ktol hd) : tl


-- the waitfree communication
-- waitfree :: K t a -> K s b -> Eigher (K t b) (K s a)
-- waitfree = ?



-- these are internal

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

execute :: [L] -> IO ()
execute = spawnPool >=> waitThread

spawnPool :: [L] -> IO ThreadPool
spawnPool = run . constructJobPool

run :: JobPool -> IO ThreadPool
run = Map.foldrWithKey threadSpawn $ return Map.empty

threadSpawn :: AbstractThreadId -> JobChannel -> IO ThreadPool -> IO ThreadPool
threadSpawn aid ch p = do
    p' <- p
    fin <- MV.newEmptyMVar
    thid <- forkIO $ worker ch fin
    return $ Map.insert aid (thid, fin) p'

constructJobPool :: [L] -> JobPool
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

writer :: K ZeroT String
writer = ret $ do
           putStrLn $ "writer start"
           hFlush stdout
           str <- getLine
           putStrLn $ "writer got: " ++ str
           return str

writer' :: MV.MVar String -> K ZeroT ()
writer' box = extend mute $ new_writer box

new_writer :: MV.MVar String -> K ZeroT String
new_writer box = fst $ reader_writer box
         
reader_writer :: MV.MVar String -> (K ZeroT String, K (SucT ZeroT) ())
reader_writer box = (w,  reader)
  where
    (w, str) = extract box writer
    reader = K (t, readeraction)
    readeraction = do
      putStrLn "reader start"
      str' <- str
      putStrLn $ "reader got: " ++ str'

rw :: MV.MVar String -> [L]           
rw box = reader // writer' box // []
    where reader = snd $ reader_writer box

main :: IO ()
main = do
    box <- MV.newEmptyMVar
    execute $ rw box
    
-- example waitfree

-- a takes input
-- b takes input
-- either a or b


