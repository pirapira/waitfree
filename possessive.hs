{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

import Control.Concurrent (ThreadId, forkIO, killThread)
import qualified Control.Concurrent.MVar as MV
import qualified Data.Map as Map
import GHC.IO.Handle (hFlush)
import System.IO (stdout)

data ZeroT = ZeroT
data SucT t = SucT t

-- xxx internal data for thread id's -- hidden
type AbstractThreadId = Int

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
-- hypersequent as heterogeneous list


---
--- Hypersequent
--- 

data HNil = HNil
data HCons e l = HCons e l

infixr 5 :*:
infixr 5 .*.

type e :*: l = HCons e l
e .*. l      = HCons e l

class HyperSequent l
instance HyperSequent HNil
instance HyperSequent l => HyperSequent (HCons e l)

-- we need HAppend for describing comm
class HAppend l l' l'' | l l' -> l''
 where hAppend :: l -> l' -> l''

instance HyperSequent l => HAppend HNil l l
 where hAppend HNil = id

instance (HyperSequent l, HAppend l l' l'')
    => HAppend (HCons x l) l' (HCons x l'')
 where hAppend (HCons x l) = HCons x. hAppend l


---
--- Hypersequent with box list and computation list
--- 

type WithL a = ([L], a)
-- first in [L] -> first in the queue
-- the above effect -> should be first in the queue -> earlier in [L]

writeMVar :: MV.MVar a -> Maybe a -> IO ()
writeMVar box (Just v) = do
    MV.tryPutMVar box v
    return ()
writeMVar _ Nothing = return ()
                             
comm :: Thread t => Thread s => HAppend l l' l'' => MV.MVar a -> MV.MVar c ->
        WithL ((K t a) :*: l) -> WithL ((K s c) :*: l') -> WithL ((K t (a,c)) :*: (K s (c,a)) :*: l'')
comm abox cbox (s0, HCons (K (taT, ta)) l) (s1, HCons (K (scT, sc)) l') =
    (news, K (taT, tatc) .*. K (scT, scsa) .*. hAppend l l')
        where
          tatc = do
            tc' <- tc
            ta' <- ta
            case (ta', tc') of
              (Nothing, _) -> return Nothing
              (_, Nothing) -> return Nothing
              (Just ta_, Just tc_) -> return $ Just (ta_, tc_)
          scsa = do
            sc' <- sc
            sa' <- sa
            case (sc', sa') of
              (Nothing, _) -> return Nothing
              (_, Nothing) -> return Nothing
              (Just sc_, Just sa_) -> return $ Just (sc_, sa_)
          tc = MV.tryTakeMVar cbox
          sa = MV.tryTakeMVar abox
          news = s0 ++ s1 ++ [ta_task] ++ [sc_task]
          ta_task = (atid taT, ta >>= writeMVar abox)     
          sc_task = (atid scT, sc >>= writeMVar cbox)     

merge :: MV.MVar a -> WithL (IO (Maybe a) :*: IO (Maybe a) :*: l) -> WithL (IO (Maybe a) :*: l)
merge box (s, (HCons x (HCons y l))) = (news, (HCons reader l))
  where
    news = s ++ [(-1, xwriter)] ++ [(-1, ywriter)]
    xwriter = x >>= writeMVar box
    ywriter = y >>= writeMVar box
    reader = MV.tryTakeMVar box

data K t a = K (t, IO (Maybe a))

spawn :: Thread t => K t ()
spawn = K (t, return $ Just ())
    
class IOMaybeFunctor w where
  wmap :: (a -> IO (Maybe b)) -> w a -> w b

-- xxx I need IO (Maybe a) composition

instance Thread t => IOMaybeFunctor (K t) where
  wmap f (K (_, x)) = K (t, y)
    where
      y = do
        x' <- x
        case x' of
          Nothing -> return Nothing
          Just x'' -> f x''

(>=>)       :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g     = \x -> f x >>= g

          
-- IOComonad. to be moved 
class IOMaybeFunctor w => MVComonad w where
   -- extract sharedBox producer = (newProducer, receiver)
   extract :: MV.MVar a -> w a -> (w a, IO (Maybe a))
   duplicate :: w a -> w (w a)
   extend :: (w a -> IO (Maybe b)) -> w a -> w b

   extend f = g . duplicate
       where g = wmap f
   duplicate = extend (return . Just)

mute :: K t a -> IO ()
mute _ = return ()

-- xxx add law for IOComonad

-- | 'extend' with the arguments swapped. Dual to '>>=' for monads.
(=>>) :: MVComonad w => w a -> (w a -> IO (Maybe b)) -> w b
(=>>) = flip extend

-- | Injects a value into the comonad.
(.>>) :: MVComonad w => w a -> IO (Maybe b) -> w b
w .>> b = extend (\_ -> b) w

instance Thread t => MVComonad (K t) where
    extract box (K (c, f)) = (newProducer, receiver)
      where
        newProducer = K (c, f')
        f' = do
          x <- f
          writeMVar box x
          return x
        receiver = do 
          putStrLn "waiting_for_remote_value"
          val <- MV.tryTakeMVar box
          putStrLn "got value"
          return val
    extend trans r@(K (_, f)) = K (t, g)
      where
        g = do
          f
          trans r

-- -- which remote to take?  Experiment!

remote :: Thread t => (a -> IO (Maybe b)) -> K t a -> K t b
remote = wmap

ret :: Thread t => IO (Maybe a) -> K t a
ret y = K (t, y)

-- internal representation of a job
type L = (AbstractThreadId, IO ())


-- the waitfree communication
-- waitfree :: K t a -> K s b -> Eigher (K t b) (K s a)
-- waitfree = ?

---
--- Example
---
rline :: IO (Maybe String)
rline = do
  l <- getLine
  return $ Just l

rlineZero :: WithL ((K ZeroT String) :*: HNil)
rlineZero = ([], ret rline .*. HNil)

-- these are internal

type JobChannel = [IO ()]

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
constructJobPool ((aid, action) : tl) =
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

le :: K (SucT ZeroT) ()
le = spawn .>> (putStrLn "one" >>= \_ -> return $ Just ())

me :: K ZeroT ()
me = spawn .>> (putStrLn "zero" >>= \_ -> return $ Just ())

main :: IO ()
main = undefined
    
-- example waitfree

-- a takes input
-- b takes input
-- either a or b


