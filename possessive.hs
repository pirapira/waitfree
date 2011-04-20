{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar (MVar, tryPutMVar, putMVar, readMVar, newEmptyMVar, tryTakeMVar)
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

writeMVar :: MVar a -> Maybe a -> IO ()
writeMVar box (Just v) = do
    tryPutMVar box v
    return ()
writeMVar _ Nothing = return ()
                             
comm :: Thread t => Thread s => HAppend l l' l'' => MVar a -> MVar c ->
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
          tc = tryTakeMVar cbox
          sa = tryTakeMVar abox
          news = s0 ++ s1 ++ [ta_task] ++ [sc_task]
          ta_task = (atid taT, ta >>= writeMVar abox)     
          sc_task = (atid scT, sc >>= writeMVar cbox)     

merge :: MVar a -> WithL (IO (Maybe a) :*: IO (Maybe a) :*: l) -> WithL (IO (Maybe a) :*: l)
merge box (s, (HCons x (HCons y l))) = (news, (HCons reader l))
  where
    news = s ++ [(-1, xwriter)] ++ [(-1, ywriter)]
    xwriter = x >>= writeMVar box
    ywriter = y >>= writeMVar box
    reader = tryTakeMVar box

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
   extract :: MVar a -> w a -> (w a, IO (Maybe a))
   duplicate :: w a -> w (w a)
   extend :: (w a -> IO (Maybe b)) -> w a -> w b

   extend f = g . duplicate
       where g = wmap f
   duplicate = extend (return . Just)

mute :: Thread t => K t a -> L
mute (K (t, a)) = (atid t, a >>= \_ -> return ()) 

mute' :: IO (Maybe ()) -> L
mute' e = (-1, e >>= \_ -> return ())

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
          val <- tryTakeMVar box
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

rlineSucZero :: WithL ((K (SucT ZeroT) String) :*: HNil)
rlineSucZero = ([], ret rline .*. HNil)

commF :: MVar String
      -> MVar String
      -> WithL
         (K ZeroT (String, String)
                :*: (K (SucT ZeroT) (String, String) :*: HNil))
commF b0 b1 = comm b0 b1 rlineZero rlineSucZero

printTwo :: Thread t => K t (String, String) -> IO (Maybe ())
printTwo (K (t, s0s1)) = do
  s0s1' <- s0s1
  case s0s1' of
    Nothing -> return Nothing
    Just (s0, s1) -> do
        putStrLn $ "Thread " ++ (show $ atid t) ++ " got: " ++ s0 ++ ", " ++ s1
        return $ Just ()

eitherPrint ::  WithL
                (K ZeroT (String, String)
                :*: (K (SucT ZeroT) (String, String) :*: HNil)) ->
                WithL
                (K ZeroT () :*: (K (SucT ZeroT) () :*: HNil))
eitherPrint (s, HCons e0 (HCons e1 l)) = (s, HCons e0' (HCons e1' l))
    where
      e0' = extend printTwo e0
      e1' = extend printTwo e1

step1 :: MVar String -> MVar String ->
         WithL (K ZeroT () :*: (K (SucT ZeroT) () :*: HNil))
step1 b0 b1 = eitherPrint $ commF b0 b1

trivialize :: MVar () -> MVar () ->
    WithL (K ZeroT () :*: (K (SucT ZeroT) () :*: HNil)) ->
    WithL (IO (Maybe ()) :*: IO (Maybe ()) :*: HNil)
trivialize box0 box1 (s, HCons e0 (HCons e1 l)) = (news, HCons e0' (HCons e1' l))
    where
      news = s ++ [mute k0] ++ [mute k1]
      (k0, e0') = extract box0 e0
      (k1, e1') = extract box1 e1


step2 :: MVar String -> MVar String -> MVar () -> MVar () ->
             WithL (IO (Maybe ()) :*: IO (Maybe ()) :*: HNil)
step2 b0 b1 b2 b3 = trivialize b2 b3 $ step1 b0 b1

merged :: MVar () ->
          WithL (IO (Maybe ()) :*: IO (Maybe ()) :*: HNil) ->
          WithL (IO (Maybe ()) :*: HNil)
merged box conf = merge box conf

final :: MVar String -> MVar String -> MVar () -> MVar () -> MVar () ->
         WithL (IO (Maybe ()) :*: HNil)
final b0 b1 b2 b3 b4 = merged b4 $ step2 b0 b1 b2 b3


-- these are internal

type JobChannel = [IO ()]

worker :: JobChannel -> MVar () -> IO ()
worker [] fin = putMVar fin ()
worker (hd : tl) fin = do
  hd
  worker tl fin

-- ThreadPool is finite map
type ThreadPool =
    Map.Map AbstractThreadId (ThreadId, MVar ())

type JobPool = 
    Map.Map AbstractThreadId JobChannel

--- Produce [L]
hypersequentToL :: WithL (IO (Maybe ()) :*: HNil) -> [L]
hypersequentToL (s, HCons e HNil) = s ++ [lastjob]
 where
   lastjob = mute' e


finalL :: MVar String -> MVar String -> MVar ()
          -> MVar ()
          -> MVar ()
          -> [L]
finalL b0 b1 b2 b3 b4 = hypersequentToL $ final b0 b1 b2 b3 b4 

---
--- What to do with [L]
---

execute :: [L] -> IO ()
execute = spawnPool >=> waitThread

spawnPool :: [L] -> IO ThreadPool
spawnPool = run . constructJobPool

run :: JobPool -> IO ThreadPool
run = Map.foldrWithKey threadSpawn $ return Map.empty

threadSpawn :: AbstractThreadId -> JobChannel -> IO ThreadPool -> IO ThreadPool
threadSpawn aid ch p = do
    p' <- p
    fin <- newEmptyMVar
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

threadWait :: (ThreadId, MVar ()) -> IO () -> IO ()
threadWait (thid, fin) w = do
    readMVar fin
    killThread thid
    w
    
--------------------------------------------------
-- example embarrasingly parallel

le :: K (SucT ZeroT) ()
le = spawn .>> (putStrLn "one" >>= \_ -> return $ Just ())

me :: K ZeroT ()
me = spawn .>> (putStrLn "zero" >>= \_ -> return $ Just ())

main :: IO ()
main = do
  b0 <- newEmptyMVar
  b1 <- newEmptyMVar
  b2 <- newEmptyMVar
  b3 <- newEmptyMVar
  b4 <- newEmptyMVar
  execute $ finalL b0 b1 b2 b3 b4
    
-- example waitfree

-- a takes input
-- b takes input
-- either a or b


