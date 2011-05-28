{-# LANGUAGE TypeOperators #-}

import Control.Concurrent.Waitfree
import Control.Concurrent (threadDelay)
import System.Random (randomRIO)

-- Can everything be parametric to this type? 
type FirstT = SucT ZeroT

ownID :: Thread t => t -> IO (t, t)
ownID _ = do
  wait <- randomRIO (0, 20000)
  threadDelay wait
  return (t, t)

hOwnId :: Thread t => Hyp (K t (t, t) :*: HNil)
hOwnId = single ownID

putWithName :: Thread t => t -> String -> IO ()
putWithName th content = putStrLn $ "Thread " ++ (show $ atid th) ++ ": " ++ content

putResult :: Thread t => t -> String -> IO ThreadStatus
putResult th str = do
  putWithName th $ "obtained " ++ str
  return Finished

putOneResult :: (Thread t, Thread s) => s -> t -> IO ThreadStatus
putOneResult owner content = putResult owner $ show $ [atid content]

two :: (Thread s, Thread t) => Hyp (K s (s, t) :*: (K t (t, s) :*: HNil))
two = comm hOwnId putOneResult hOwnId putOneResult

putTwoResults :: (Thread s, Thread t, Thread u) => u -> (s,t) -> IO ThreadStatus
putTwoResults owner (c0, c1) = putResult owner $ show $ [atid c0, atid c1]

twoFin :: Hyp (HCons (K ZeroT ThreadStatus) (HCons (K FirstT ThreadStatus) HNil))
twoFin = two >>= (putTwoResults -*- putTwoResults -*- return)

duplicateTwo :: t -> a -> IO (a,a)
duplicateTwo _ x = return (x,x)

twoBeforeComm :: (Thread s, Thread t) => 
                     Hyp (HCons
                        (K s ((s, t), (s, t)))
                        (HCons (K t ((t, s), (t, s))) HNil))
twoBeforeComm = two >>= (duplicateTwo -*- duplicateTwo -*- return)

main :: IO ()
main = execute twoFin

