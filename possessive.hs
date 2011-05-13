{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

module Possessive where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar (MVar, tryPutMVar, putMVar, readMVar, newEmptyMVar, tryTakeMVar, takeMVar)
import qualified Data.Map as Map

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
(.*.) :: e -> l -> HCons e l
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
    _ <- tryPutMVar box v
    return ()
writeMVar _ Nothing = return ()

             
-- I guess the six thing come from this.   Maybe explicit weakening helps.
                
comm_ :: Thread t => Thread s => HAppend l l' l'' => MVar a -> MVar c ->
        WithL ((K t a) :*: l) -> WithL ((K s c) :*: l') -> WithL ((K t (a, c)) :*: (K s (c, a)) :*: l'')
comm_ abox cbox (s0, HCons (K (taT, ta)) l) (s1, HCons (K (scT, sc)) l') =
    (news, K (taT, tac) .*. K (scT, sca) .*. hAppend l l')
        where
          tac = do
            cval <- tryTakeMVar cbox
            case cval of
              Nothing -> do
                        return Nothing
              Just cva -> do
                        aval <- takeMVar abox -- this should not block
                        return $ Just (aval, cva)
          sca = do
            aval <- tryTakeMVar abox
            case aval of
              Nothing -> do
                       return Nothing
              Just ava -> do
                       cval <- takeMVar cbox
                       return $ Just (cval, ava)
          news = s0 ++ s1 ++ [ta_task] ++ [sc_task]
          ta_task = (atid taT,
                          do
                            ta' <- ta
                            writeMVar abox ta'
                    )     
          sc_task = (atid scT,
                          do
                            sc' <- sc
                            writeMVar cbox sc'
                    ) 

data K t a = K (t, IO (Maybe a))

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

mute' :: IO (Maybe ()) -> L
mute' e = (-1, e >>= \_ -> return ())

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

---
--- Example
---
local :: Thread t => IO a -> IO (WithL ((K t a) :*: HNil))
local f = return $ ([], (ret f') .*. HNil)
  where
    f' = do
      x <- f
      return $ Just x


trivialize :: Thread t => Thread s => MVar () -> MVar () ->
    WithL (K t () :*: (K s () :*: HNil)) ->
    WithL (IO (Maybe ()) :*: IO (Maybe ()) :*: HNil)
trivialize box0 box1 (s, HCons e0 (HCons e1 l)) = (news, HCons e0' (HCons e1' l))
    where
      news = s ++ [mute k0] ++ [mute k1]
      (k0, e0') = extract box0 e0
      (k1, e1') = extract box1 e1


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

comm :: (Thread s, Thread t, HAppend l l' l'', Show c, Show a) =>
        IO ([L], HCons (K t a) l)
         -> IO ([L], HCons (K s c) l')
         -> IO (WithL (K t (a, c) :*: (K s (c, a) :*: l'')))
comm x y = do
  (s0, HCons (K (taT, ta)) l) <- x
  (s1, HCons (K (scT, sc)) l') <- y
  abox <- newEmptyMVar
  cbox <- newEmptyMVar
  return $ comm_ abox cbox (s0, HCons (K (taT, ta)) l) (s1, HCons (K (scT, sc)) l')

katamari2_library :: Thread s => Thread t =>
                     IO (WithL (K s () :*: (K t () :*: HNil))) ->
                     IO (WithL (IO (Maybe ()) :*: (IO (Maybe ()) :*: HNil)))
katamari2_library katamari1 = do
  b2 <- newEmptyMVar
  b3 <- newEmptyMVar
  k <- katamari1
  return $ trivialize b2 b3 k

merge :: IO (WithL (IO (Maybe a) :*: (IO (Maybe a) :*: l)))
       -> IO (WithL (IO (Maybe a) :*: l))
merge above = do
  box <- newEmptyMVar
  (s, (HCons x (HCons y l))) <- above
  return $ (s ++ [(-1, x >>= writeMVar box)] ++ [(-1, y >>= writeMVar box)], (HCons (reader box) l))
   where
    reader box = do
      val <- tryTakeMVar box
      return val
      
    
    
-- example waitfree

-- a takes input
-- b takes input
-- either a or b


