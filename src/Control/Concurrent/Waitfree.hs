{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

module Control.Concurrent.Waitfree
    ( ZeroT
    , SucT
    , HNil
    , (:*:)
    , K
    , single
    , Hyp
    , Thread
    , t
    , atid
    , AbstractThreadId
    , peek
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

data ZeroT = ZeroT
data SucT t = SucT t

type AbstractThreadId = Int
            
-- xxx internal data for thread id's -- hidden
class Thread t where
    t :: t
    atid :: t -> AbstractThreadId

instance Thread ZeroT where
    t = ZeroT
    atid ZeroT = 0
instance Thread t => Thread (SucT t) where
    t = SucT t
    atid (SucT x) = succ $ atid x

--- how to make Thread as showable?
--- and hide atid

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
infixr 4 -*-

type e :*: l = HCons e l
(.*.) :: e -> l -> HCons e l
e .*. l      = HCons e l

class HyperSequent l
instance HyperSequent HNil
instance HyperSequent l => HyperSequent (HCons (K t e) l)

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


--- arrangements on Hyp l helps
--- when deconstructed, all contents of [L] belongs to head.
--- or rather, split, do something, and merge kind of higher function??
    
writeMVar :: MVar a -> Maybe a -> IO ()
writeMVar box (Just v) = do
    _ <- tryPutMVar box v
    return ()
writeMVar _ Nothing = return ()

             
-- I guess the six thing come from this.   Maybe explicit weakening helps.
-- self first
                
comm_ :: Thread t => Thread s => HAppend l l' l'' => MVar a -> MVar c -> IORef (Maybe b) -> IORef (Maybe d) ->
        ([L], ((K t (b,a)) :*: l)) -> ([L], ((K s (d,c)) :*: l')) -> ([L], (K t (b,c)) :*: (K s (d,a) ):*: l'')
comm_ abox cbox bbox dbox (s0, HCons (K (taT, tba)) l) (s1, HCons (K (scT, sdc)) l') =
    (news, K (taT, tbc) .*. K (scT, sda) .*. hAppend l l')
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

newtype K t a = K (t, IO (Maybe a))

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
mute (K (th, a)) = (atid th, a >>= \_ -> return ()) 


-- xxx add law for IOComonad

-- | 'extend' with the arguments swapped. Dual to '>>=' for monads.
-- (=>>) :: MVComonad w => w a -> (w a -> IO (Maybe b)) -> w b
-- (=>>) = flip extend

-- | Injects a value into the comonad.
-- (.>>) :: MVComonad w => w a -> IO (Maybe b) -> w b
-- w .>> b = extend (\_ -> b) w

instance Thread t => MVComonad (K t) where
    extract box (K (c, f)) = (newProducer, receiver)
      where
        newProducer = K (c, f')
        f' = do
          x <- f
          writeMVar box x
          return x
        receiver = do 
          val <- tryTakeMVar box
          return val
    extend trans r = K (t, trans r)

-- -- which remote to take?  Experiment!

-- remote :: Thread t => (a -> IO (Maybe b)) -> K t a -> K t b
-- remote = wmap

ret :: Thread t => IO (Maybe a) -> K t a
ret y = K (t, y)

-- internal representation of a job
type L = (AbstractThreadId, IO ())


-- the waitfree communication
-- waitfree :: K t a -> K s b -> Eigher (K t b) (K s a)
-- waitfree = ?

single :: Thread t => IO a -> Hyp ((K t a) :*: HNil)
single f = MakeHyp $ return $ ([], (ret f') .*. HNil)
  where
    f' = do
      x <- f
      return $ Just x


peek :: Thread t => (t -> (Maybe a) -> IO b) -> K t a -> IO b 
peek f (K (th, content)) = do
  content >>= f th 


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
execute :: Lconvertible l => Hyp l -> IO ()
execute (MakeHyp ls) = do
  withl <- ls
  execute' $ hypersequentToL' withl

execute' :: [L] -> IO ()
execute' = spawnPool >=> waitThread

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

(-*-) :: (Thread t, HyperSequent l, HyperSequent l') =>
            (K t a -> IO (Maybe b)) -> (l -> Hyp l') ->
            HCons (K t a) l -> Hyp (HCons (K t b) l')
(-*-) hdf = progress_ (extend hdf) 

-- use Hyp 
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



