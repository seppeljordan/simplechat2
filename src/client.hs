module Main where

import System.IO
import Network.Socket
import Network.SimpleChat.Classes
import Network.SimpleChat.Callback
import Network.SimpleChat.Message
import Control.Concurrent
import Control.Concurrent.Async
import Data.IP
import Data.Functor
import Control.Monad
import Reactive.Banana.Frameworks hiding (register)
import Reactive.Banana
import Control.Concurrent

hostName :: String
hostName = "127.0.0.1"

main :: IO ()
main = do
  putStrLn $ "Connect to "++hostName
  sock <- socket AF_INET Stream defaultProtocol
  connect sock (SockAddrInet (fromInteger 4141) (toHostAddress (read "127.0.0.1")))
  putStrLn $ "...Connected"
  (inputHandler, inputHandlerProc) <- installInputHandler stdin :: IO (EvHandler String, ThreadId)
  (networkHandler, networkHandlerProc) <- installNetworkHandler sock :: IO (EvHandler String, ThreadId)
  quitT <- async $ forever $ threadDelay (10 * 10^(6::Int))
  network <- compile $ do
               inputEvents <- getEvents inputHandler
               networkEvents <- getEvents networkHandler
               let quitE = filterE isQuit (TextMessage <$> inputEvents)
                   inputLinesE = (|>'\n') <$> inputEvents
               reactimate $ (\str -> sendString sock str >> return ()) <$> inputLinesE
               reactimate $ putStr <$> networkEvents
               reactimate $ cancel quitT <$ quitE
  actuate network
  _ <- waitCatch quitT
  mapM_ killThread [inputHandlerProc, networkHandlerProc]
  putStrLn $ "Disconnect"
  disconnect sock
  putStrLn $ "... Disconnected"
  return ()
