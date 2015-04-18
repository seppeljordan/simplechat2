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
import System.Environment

hostName :: String
hostName = "127.0.0.1"

withQuit :: (IO () -> IO a) -> IO a
withQuit process = do
  blockingLoop <- async $ forever $ threadDelay (10*10^(6::Int)) :: IO (Async ())
  quitHandler <- newEventHandler :: IO (EvHandler ())
  _ <- register quitHandler $ const (cancel blockingLoop)
  result <- process (fire quitHandler ())
  _ <- waitCatch blockingLoop
  return result

runClient :: Integer -> IPv4 -> IO ()
runClient port ipaddr = do
  sock <- socket AF_INET Stream defaultProtocol
  connect sock (SockAddrInet (fromInteger port) (toHostAddress ipaddr))
  putStrLn $ "...Connected"
  (inputHandler, inputHandlerProc) <- installInputHandler stdin :: IO (EvHandler String, ThreadId)
  (networkHandler, networkHandlerProc) <- installNetworkHandler sock :: IO (EvHandler String, ThreadId)
  withQuit $ \quit -> do
    network <- compile $ do
                 inputEvents <- getEvents inputHandler
                 networkEvents <- getEvents networkHandler
                 let quitE = filterE isQuit (TextMessage <$> inputEvents)
                     inputLinesE = (|>'\n') <$> inputEvents
                 reactimate $ (\str -> sendString sock str >> return ())   <$> inputLinesE
                 reactimate $ putStr <$> networkEvents
                 reactimate $ quit <$ quitE
    actuate network
  mapM_ killThread [inputHandlerProc, networkHandlerProc]
  putStrLn $ "Disconnect"
  disconnect sock
  putStrLn $ "... Disconnected"
  return ()

main :: IO ()
main = do
  args <- getArgs
  let port = read $ args !! 0
      ipaddr = read $ args !! 1
      host = args !! 1
  putStrLn $ "Connect to "++host++" at port "++(show port)
  runClient port ipaddr
