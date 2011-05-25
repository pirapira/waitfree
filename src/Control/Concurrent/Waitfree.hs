{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

module Control.Concurrent.Waitfree
    ( ZeroT
    , SucT
    , HNil
    , HCons
    , (:*:)
    , K
    , single
    , Hyp
    , Thread (t, atid)
    , AbstractThreadId
    , comm
    , execute
    , (-*-)
    )
    where

-- how to export only certain things?    

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar (MVar, tryPutMVar, putMVar, readMVar, newEmptyMVar, tryTakeMVar)
import qualified Data.Map as Map
import Data.IORef (readIORef, newIORef, IORef, writeIORef)

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
            

-- | A value of type 'K t a' represents a remote computation returning 'a' that is performed by a thread 't'.
newtype K t a = K (t, IO (Maybe a))

---
--- Hypersequent
--- 

-- | 'HyperSequent' represents a sequence of remote computations, possibly owned by different threads.
-- | When a 'HyperSequent' is executed, at least one remote computation is successful.
class HyperSequent l

-- | 'HNil' is the empty 'HyperSequent'
data HNil = HNil
instance HyperSequent HNil

-- | 'HCons (K t e)' adds a remote computation in front of a 'HyperSequent'
data HCons e l = HCons e l
instance HyperSequent l => HyperSequent (HCons (K t e) l)

-- | an abreviation for 'HCons'
infixr 5 :*:
type e :*: l = HCons e l


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

-- first in [L] -> first in the queue
-- the above effect -> should be first in the queue -> earlier in [L]

-- | hypersequent is always put in 'Hyp' monad
newtype Hyp a = MakeHyp (IO ([L], a))

instance Monad Hyp where
    return x = MakeHyp $ return ([], x)
    (>>=) = flip cappy

cappy :: (l -> Hyp l') -> Hyp l -> Hyp l'
cappy f (MakeHyp x) = MakeHyp $ do
  (ls, lx) <- x
  let MakeHyp y = f lx in do
    (newls, newlx) <- y
    return (ls ++ newls, newlx)

remote :: Thread t => IO (Maybe a) -> K t a
remote y = K (t, y)

-- | 'single' creates a Hyp hypersequent consisting of a single remote computation.
single :: Thread t => IO a -> Hyp ((K t a) :*: HNil)
single f = MakeHyp $ return $ ([], HCons (remote f') HNil)
  where
    f' = do
      x <- f
      return $ Just x

infixr 4 -*-

-- | extend a Hyp hypersequent with another computation
(-*-) :: (Thread t, HyperSequent l, HyperSequent l') =>
            (t -> a -> IO b) -> (l -> Hyp l') ->
            HCons (K t a) l -> Hyp (HCons (K t b) l')
(-*-) hdf = progress_ (extend $ peek $ lmaybe hdf) 

lmaybe :: (t -> a -> IO b) -> (t -> Maybe a -> IO (Maybe b))
lmaybe _ _  Nothing = return Nothing
lmaybe f th (Just x) =  do
  y <- f th x
  return $ Just y

-- | 'peek' allows to look at the result of a remote computation
peek :: Thread t => (t -> (Maybe a) -> IO b) -> K t a -> IO b 
peek f (K (th, content)) = do
  content >>= f th 

-- | 'comm' stands for communication.  'comm' combines two hypersequents with a communicating component from each hypersequent.
comm :: (Thread s, Thread t, HAppend l l' l'') =>
        Hyp (HCons (K t (b,a)) l)
         -> Hyp (HCons (K s (d,c)) l')
         -> Hyp (K t (b, c) :*: (K s (d, a) :*: l''))
comm (MakeHyp x) (MakeHyp y) = MakeHyp $ do
  (s0, HCons (K (taT, ta)) l) <- x
  (s1, HCons (K (scT, sc)) l') <- y
  abox <- newEmptyMVar
  cbox <- newEmptyMVar
  bbox <- newIORef Nothing
  dbox <- newIORef Nothing
  return $ comm_ abox cbox bbox dbox (s0, HCons (K (taT, ta)) l) (s1, HCons (K (scT, sc)) l')

-- internal implementation of comm
comm_ :: Thread t => Thread s => HAppend l l' l'' => MVar a -> MVar c -> IORef (Maybe b) -> IORef (Maybe d) ->
        ([L], ((K t (b,a)) :*: l)) -> ([L], ((K s (d,c)) :*: l')) -> ([L], (K t (b,c)) :*: (K s (d,a) ):*: l'')
comm_ abox cbox bbox dbox (s0, HCons (K (taT, tba)) l) (s1, HCons (K (scT, sdc)) l') =
    (news, HCons (K (taT, tbc)) (HCons (K (scT, sda)) (hAppend l l')))
        where
          tbc = do
            cval <- tryTakeMVar cbox
            case cval of
              Nothing ->  return Nothing
              Just cva -> do
                        maybetb <- readIORef bbox
                        case maybetb of
                          Just tb -> return $ Just (tb, cva)
                          Nothing -> return Nothing
          sda = do
            aval <- tryTakeMVar abox
            case aval of
              Nothing -> do
                       return Nothing
              Just ava -> do
                        maybesd <- readIORef dbox
                        case maybesd of
                          Nothing -> return Nothing
                          Just sd -> return $ Just (sd, ava)
          news = s0 ++ s1 ++ [ta_task] ++ [sc_task]
          ta_task = (atid taT,
                          do
                            maybeba <- tba
                            case maybeba of
                              Nothing -> writeMVar abox Nothing
                              Just (tb, ta) -> do
                                           writeMVar abox $ Just ta
                                           writeIORef bbox $ Just tb
                    )     
          sc_task = (atid scT,
                          do
                            maybedc <- sdc
                            case maybedc of
                              Nothing -> writeMVar cbox Nothing
                              Just (sd, sc) -> do writeMVar cbox $ Just sc
                                                  writeIORef dbox $ Just sd
                    ) 


writeMVar :: MVar a -> Maybe a -> IO ()
writeMVar box (Just v) = do
    _ <- tryPutMVar box v
    return ()
writeMVar _ Nothing = return ()



-- | 'execute' executes a 'Hyp' hypersequent.
execute :: Lconvertible l => Hyp l -> IO ()
execute (MakeHyp ls) = do
  withl <- ls
  execute' $ hypersequentToL' withl

-- below is internal

extend :: Thread t => (K t a -> IO (Maybe b)) -> K t a -> K t b
extend trans r = K (t, trans r)

mute :: Thread t => K t a -> L
mute (K (th, a)) = (atid th, a >>= \_ -> return ()) 


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

type L = (AbstractThreadId, IO ())

class Lconvertible l where
    htol :: l -> [L]

instance Lconvertible HNil where
    htol _ = []

instance (Thread t, Lconvertible l) => Lconvertible (HCons (K t a) l) where
    htol (HCons e rest) = mute e : htol rest

hypersequentToL' :: Lconvertible l => ([L], l) -> [L]
hypersequentToL' (s, ls) = s ++ htol ls

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
progress_ :: (HyperSequent l, HyperSequent l') =>
            (a -> b) -> (l -> Hyp l') -> HCons a l -> 
            Hyp (HCons b l')
progress_ hdf tlf (HCons ax bl) = MakeHyp $ do
  let MakeHyp x = tlf bl in do
    (newls, newtl) <- x
    return (newls, HCons (hdf ax) newtl)





