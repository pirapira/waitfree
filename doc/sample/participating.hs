{-# LANGUAGE TypeOperators #-}

import Control.Concurrent.Waitfree
import Control.Concurrent (threadDelay)
import System.Random (randomRIO)

-- Can everything be parametric to this type? 
type FirstT = SucT ZeroT
type SecondT = SucT FirstT

ownID :: Thread t => t -> IO (t, t)
ownID _ = do
  wait <- randomRIO (0, 20000)
  threadDelay wait
  return (t, t)

hOwnId :: Thread t => IO (K t (t, t) :*: HNil)
hOwnId = single ownID

putWithName :: Thread t => t -> String -> IO ()
putWithName th content = putStrLn $ "Thread " ++ (show $ atid th) ++ ": " ++ content

failOut :: t -> a -> IO ThreadStatus
failOut _ _ = return TryAnotherJob

putResult :: Thread t => t -> String -> IO ThreadStatus
putResult th str = do
  putWithName th $ "obtained " ++ str
  return Finished

putOneResult :: (Thread t, Thread s) => s -> t -> IO ThreadStatus
putOneResult owner content = putResult owner $ show $ [atid content]

two :: (Thread s, Thread t) => IO (K s (s, t) :*: (K t (t, s) :*: HNil))
two = comm hOwnId failOut hOwnId failOut

putTwoResults :: (Thread s, Thread t, Thread u) => u -> (s,t) -> IO ThreadStatus
putTwoResults owner (c0, c1) = putResult owner $ show $ [atid c0, atid c1]

twoFin :: (Thread s, Thread t) => IO (HCons (K s ThreadStatus) (HCons (K t ThreadStatus) HNil))
twoFin = two >>= (putTwoResults -*- putTwoResults -*- return)

duplicateTwo :: t -> a -> IO (a,a)
duplicateTwo _ x = return (x,x)

twoBeforeComm :: IO (HCons
                        (K ZeroT ((ZeroT, FirstT), (ZeroT, FirstT)))
                        (HCons (K FirstT ((FirstT, ZeroT), (FirstT, ZeroT))) HNil))
twoBeforeComm = two >>= (duplicateTwo -*- duplicateTwo -*- return)

three_ :: IO
          (K ZeroT ((ZeroT, FirstT), SecondT)
            :*: (K SecondT (SecondT, (ZeroT, FirstT))
                 :*: HCons (K FirstT ((FirstT, ZeroT), (FirstT, ZeroT))) HNil))
three_ = comm twoBeforeComm failOut hOwnId failOut


three__ :: IO (HCons
                (K FirstT ((FirstT, ZeroT), (FirstT, ZeroT)))
                (HCons
                 (K ZeroT ((ZeroT, FirstT), SecondT))
                 (HCons (K SecondT (SecondT, (ZeroT, FirstT))) HNil)))
three__ = cycling three_

three___ :: IO
                (K FirstT ((FirstT, ZeroT), SecondT)
                 :*: (K SecondT (SecondT, (FirstT, ZeroT))
                      :*: HCons
                            (K ZeroT ((ZeroT, FirstT), SecondT))
                            (HCons (K SecondT (SecondT, (ZeroT, FirstT))) HNil)))
three___ = comm three__ failOut hOwnId failOut

printThreeResults0 :: (Thread u, Thread s, Thread t, Thread v) => u -> (s,(t,v)) -> IO ThreadStatus
printThreeResults0 owner (c0, (c1, c2)) = putResult owner $ show $ [atid c0, atid c1, atid c2]
           
printThreeResults1 :: (Thread u, Thread s, Thread t, Thread v) => u -> ((s,t),v) -> IO ThreadStatus
printThreeResults1 owner ((c0, c1), c2) = putResult owner $ show $ [atid c0, atid c1, atid c2]

three :: IO
           (HCons
            (K FirstT ThreadStatus)
            (HCons
             (K SecondT ThreadStatus)
             (HCons
              (K ZeroT ThreadStatus) (HCons (K SecondT ThreadStatus) HNil))))
three = three___ >>= (printThreeResults1 -*- printThreeResults0 -*- printThreeResults1 -*- printThreeResults0 -*- return)
           
main :: IO ()
main = execute three

