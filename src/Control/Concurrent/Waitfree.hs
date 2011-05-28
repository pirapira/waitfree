{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

module Control.Concurrent.Waitfree
    ( ZeroT
    , SucT
    , HNil
    , HCons
    , (:*:)
    , K
    , single
    , Thread (t, atid)
    , AbstractThreadId
    , comm
    , follows
    , cycling
    , execute
    , (-*-)
    , ThreadStatus (TryAnotherJob, Finished)
    )
    where

-- how to export only certain things?    

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar (MVar, tryPutMVar, putMVar, readMVar, newEmptyMVar, tryTakeMVar, takeMVar)
import qualified Data.Map as Map

-- | An abstract representation of a thread.  Threads are actually implemented using 'forkIO'.
class Thread t where
    t :: t
    atid :: t -> AbstractThreadId

-- | 'ZeroT' is a 'Thread'
data ZeroT = ZeroT
instance Thread ZeroT where
    t = ZeroT
    atid ZeroT = 0

-- | 'SucT t' is a 'Thread' if t is a 'Thread'.  The name 'SucT' comes from the successor function.
data SucT t = SucT t
instance Thread t => Thread (SucT t) where
    t = SucT t
    atid (SucT x) = succ $ atid x

-- | Each 'Thread' type has 'AbstractThreadId'
type AbstractThreadId = Int

-- | ThreadStatus shows whether a thread is finished or have to try executing another job.
data ThreadStatus = TryAnotherJob | Finished

-- | JobStatus shows whether a job is finished or not.
data JobStatus a = Having a | Done ThreadStatus

jth2th :: JobStatus ThreadStatus -> ThreadStatus 
jth2th (Having x) = x
jth2th (Done x) = x

-- | A value of type 'K t a' represents a remote computation returning 'a' that is performed by a thread 't'.
newtype K t a = K (t, IO (JobStatus a))

---
--- IOersequent
--- 

-- | 'IOerSequent' represents a sequence of remote computations, possibly owned by different threads.
-- | When a 'IOerSequent' is executed, at least one remote computation is successful.
class IOerSequent l

-- | 'HNil' is the empty 'IOerSequent'
data HNil = HNil
instance IOerSequent HNil

-- | 'HCons (K t e)' adds a remote computation in front of a 'IOerSequent'
data HCons e l = HCons e l
instance IOerSequent l => IOerSequent (HCons (K t e) l)

-- | an abreviation for 'HCons'
infixr 5 :*:
type e :*: l = HCons e l


-- we need HAppend for describing comm
class HAppend l l' l'' | l l' -> l''
 where hAppend :: l -> l' -> l''

instance IOerSequent l => HAppend HNil l l
 where hAppend HNil = id

instance (IOerSequent l, HAppend l l' l'')
    => HAppend (HCons x l) l' (HCons x l'')
 where hAppend (HCons x l) = HCons x. hAppend l

class HLast l a heads | l -> a, l -> heads
 where hLast :: l -> (a, heads)

instance HLast (HCons a HNil) a HNil
    where hLast (HCons x HNil) = (x, HNil)

instance (HLast (HCons lh ll) a heads) => (HLast (HCons b (HCons lh ll)) a (HCons b heads))
    where hLast (HCons y rest) =
              case hLast rest of
                (x, oldheads) -> (x, HCons y oldheads)

follows :: HAppend l l' l'' => IO l -> IO l' -> IO l''
follows l0 l1 = do
  h0 <- l0
  h1 <- l1
  return $ hAppend h0 h1

cycling_ :: HLast l a heads => l -> HCons a heads
cycling_ lst = case hLast lst of
                (last_, heads) -> HCons last_ heads

cycling :: HLast l last heads => IO l -> IO (HCons last heads)
cycling = fmap cycling_

---
--- IOersequent with box list and computation list
--- 

-- first in [L] -> first in the queue
-- the above effect -> should be first in the queue -> earlier in [L]

remote :: Thread t => IO a -> K t a
remote y = K (t, fmap Having y)

-- | 'single' creates a IO hypersequent consisting of a single remote computation.
single :: Thread t => (t -> IO a) -> IO (K t a :*: HNil)
single f = return $ HCons (remote f') HNil
  where
    f' = do
      x <- f t
      return x

infixr 4 -*-

-- | extend a IO hypersequent with another computation
(-*-) :: (Thread t, IOerSequent l, IOerSequent l') =>
            (t -> a -> IO b) -> (l -> IO l') ->
            HCons (K t a) l -> IO (HCons (K t b) l')
(-*-) hdf = progress_ (extend $ peek $ lmaybe hdf) 

lmaybe :: (t -> a -> IO b) -> t -> JobStatus a -> IO (JobStatus b)
lmaybe _ _  (Done x) = return (Done x)
lmaybe f th (Having x) =  do
  y <- f th x
  return $ Having y

-- | 'peek' allows to look at the result of a remote computation
peek :: Thread t => (t -> JobStatus a -> IO b) -> K t a -> IO b 
peek f (K (th, content)) = content >>= f th 

-- | 'comm' stands for communication.  'comm' combines two hypersequents with a communicating component from each hypersequent.
-- | 'comm hypersequent1 error1 hypersequent2 error2' where 'error1' and 'error2' specifies what to do in case of read failure.
comm :: (Thread s, Thread t, HAppend l l' l'') =>
        IO (HCons (K t (b,a)) l)
         -> (t -> b -> IO ThreadStatus)
         -> IO (HCons (K s (d,c)) l')
         -> (s -> d -> IO ThreadStatus)
         -> IO (K t (b, c) :*: (K s (d, a) :*: l''))
comm x terror y serror = do
  HCons (K (taT, ta)) l <- x
  HCons (K (scT, sc)) l' <- y
  abox <- newEmptyMVar
  cbox <- newEmptyMVar
  bbox <- newEmptyMVar
  dbox <- newEmptyMVar
  return $ comm_ abox cbox bbox dbox (HCons (K (taT, ta)) l) terror (HCons (K (scT, sc)) l') serror

-- internal implementation of comm
comm_ :: Thread t => Thread s => HAppend l l' l'' => MVar a -> MVar c -> MVar (JobStatus b) -> MVar (JobStatus d) ->
        (K t (b,a) :*: l) -> (t -> b -> IO ThreadStatus) ->
        (K s (d,c) :*: l') -> (s -> d -> IO ThreadStatus) ->
        (K t (b,c) :*: K s (d,a) :*: l'')
comm_ abox cbox bbox dbox (HCons (K (taT, tba)) l) terror (HCons (K (scT, sdc)) l') serror =
    HCons (K (taT, tbc)) (HCons (K (scT, sda)) (hAppend l l'))
        where
          tbc = do
            maybeba <- tba
            case maybeba of
              Done thStatus -> do writeMVar abox Nothing
                                  putMVar bbox $ Done thStatus
              Having (tb, ta) -> do
                           writeMVar abox $ Just ta
                           writeMVar bbox $ Just $ Having tb
            cval <- tryTakeMVar cbox
            case cval of
              Nothing -> do
                        maybetb <- takeMVar bbox
                        case maybetb of
                          Having tb -> do
                            terror_result <- terror taT tb
                            return $ Done terror_result
                          Done x -> return $ Done x
              Just cva -> do
                        maybetb <- takeMVar bbox
                        case maybetb of
                          Having tb -> return $ Having (tb, cva)
                          Done x -> return $ Done x
          sda = do
            maybedc <- sdc
            case maybedc of
              Done thStatus -> do writeMVar cbox Nothing
                                  writeMVar dbox $ Just $ Done thStatus
              Having (sd, sc) -> do
                           writeMVar cbox $ Just sc
                           writeMVar dbox $ Just $ Having sd
            aval <- tryTakeMVar abox
            case aval of
              Nothing -> do
                        maybesd <- takeMVar dbox
                        case maybesd of
                          Having sd -> do
                            serror_result <- serror scT sd
                            return $ Done serror_result
                          Done x -> return $ Done x
              Just ava -> do
                        maybesd <- takeMVar dbox
                        case maybesd of
                          Having sd -> return $ Having (sd, ava)
                          Done x -> return $ Done x

writeMVar :: MVar a -> Maybe a -> IO ()
writeMVar box (Just v) = do
    _ <- tryPutMVar box v
    return ()
writeMVar _ Nothing = return ()


-- | 'execute' executes a 'IO' hypersequent.
execute :: Lconvertible l => IO l -> IO ()
execute ls = do
  withl <- ls
  execute' $ htol withl

-- below is internal

extend :: Thread t => (K t a -> IO (JobStatus b)) -> K t a -> K t b
extend trans r = K (t, trans r)

type JobChannel = [IO ThreadStatus]

worker :: JobChannel -> MVar () -> IO ()
worker [] fin = putMVar fin ()
worker (hd : tl) fin = do
  result <- hd
  case result of
    TryAnotherJob -> worker tl fin
    Finished -> putMVar fin ()

-- ThreadPool is finite map
type ThreadPool =
    Map.Map AbstractThreadId (ThreadId, MVar ())

type JobPool = 
    Map.Map AbstractThreadId JobChannel

type L = (AbstractThreadId, IO ThreadStatus)

class Lconvertible l where
    htol :: l -> [L]

instance Lconvertible HNil where
    htol _ = []

instance (Thread t, Lconvertible l) => Lconvertible (HCons (K t ThreadStatus) l) where
    htol (HCons (K (th, result)) rest) = (atid th, fmap jth2th result) : htol rest

---
--- What to do with [L]
---
execute' :: [L] -> IO ()
execute' l = spawnPool l >>= waitThread

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

-- this is introduced in order to remove HCons
progress_ :: (IOerSequent l, IOerSequent l') =>
            (a -> b) -> (l -> IO l') -> HCons a l -> 
            IO (HCons b l')
progress_ hdf tlf (HCons ax bl) = do
  newtl <- tlf bl
  return $ HCons (hdf ax) newtl
