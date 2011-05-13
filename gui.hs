import Network
import IO
import Control.Concurrent
import Control.Monad

port :: PortID
port = PortNumber 6000

main = withSocketsDo $ do
 sock <- listenOn port 
 handler sock

handler :: Socket -> IO ()
handler s = do
  (h,_,_) <- accept s 
  hSetBuffering h NoBuffering
  echo h

echo :: Handle -> IO ()
echo h= do
  text <- liftM (filter (/='\r')) $ hGetLine h 
  if text=="exit"
     then hClose h
     else do
          hPutStrLn h text
          echo h
