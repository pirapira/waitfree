{-# LANGUAGE TypeOperators #-}

-- this listens on port 6000 and 6001.

import Control.Concurrent.Waitfree
import Network
import IO (Handle, hSetBuffering, BufferMode (NoBuffering), hPutStrLn, hGetLine, hPutStr)

handle :: PortID -> IO Handle
handle p = withSocketsDo $ do
  s <- listenOn p
  (h,_,_) <- accept s
  hSetBuffering h NoBuffering
  return h

prepareHandle :: Thread t => PortID -> Hyp (K t Handle :*: HNil)
prepareHandle p = single $ handle p

readLine :: Thread t => t -> Maybe Handle -> IO (Maybe ((Handle, String), String))
readLine _ Nothing = return Nothing
readLine th (Just h) = do
  hPutStr h $ (show $ atid th) ++ " requiring input: "
  str <- hGetLine h
  return $ Just ((h, str), str)

readH :: Thread t => PortID -> Hyp (K t ((Handle, String), String) :*: HNil)
readH p = prepareHandle p >>= (readLine -*- return)

printTaken :: (Thread t) => t -> Maybe ((Handle, String), String) -> IO (Maybe ())
printTaken _ Nothing = return Nothing
printTaken th (Just ((h, selfs), peers)) = do
        hPutStrLn h $ (show $ atid th) ++ " got: " ++ show (selfs, peers)
        return $ Just ()

content ::  Hyp (K ZeroT () :*: (K (SucT ZeroT) () :*: HNil))
content =
    comm (readH $ PortNumber 6000) (readH $ PortNumber 6001) >>=
    (printTaken -*- printTaken -*- return)

main :: IO ()
main = execute content
