{-# LANGUAGE TypeOperators #-}

import Possessive
import Network
import IO (Handle, hSetBuffering, BufferMode (NoBuffering), hPutStrLn, hGetLine, hPutStr)

rline :: Thread t => t -> Hyp ((K t ((Handle, String), String)) :*: HNil)
rline th = single $ withSocketsDo $ do
  s <- listenOn $ PortNumber $ fromIntegral (6000 + atid th)
  (h,_,_) <- accept s
  hSetBuffering h NoBuffering
  hPutStr h $ name th ++ " requiring input: "
  str <- hGetLine h
  return ((h, str), str)

printTaken_inner :: (Thread t) => t -> Maybe ((Handle, String), String) -> IO (Maybe ())
printTaken_inner _ Nothing = return Nothing
printTaken_inner th (Just ((handle, selfs), peers)) = do
        hPutStrLn handle $ name th ++ " got: " ++ show (selfs, peers)
        return $ Just ()

printPeeked :: Thread t => K t ((Handle, String), String) -> IO (Maybe ())
printPeeked known = peek (printTaken_inner) known

content ::  Hyp (K ZeroT () :*: (K (SucT ZeroT) () :*: HNil))
content =
    comm (rline t) (rline t) >>=
    (printPeeked -*- printPeeked -*- return)

main :: IO ()
main = execute content
