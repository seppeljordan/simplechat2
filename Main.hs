import Data.Foldable
import Network.Socket
import Reactive.Banana
import Prelude hiding
    ( mapM_ )
import Control.Monad hiding
    ( mapM_ )
import Network.Socket
import Control.Concurrent.Async

class User a where
    userId :: a -> Integer

class Message a where
    messageText :: a -> String

class NetworkTarget a where
    targetSocket :: a -> Socket

instance NetworkTarget Socket where
    targetSocket = id

-- | Broadcast a message to every user except the "owner" of the
-- message.  This method calls asynchronous threads.
broadcastMessage :: (User u, Message m, User m, Foldable t) =>
                    (u -> m -> IO ()) -> t u -> m -> IO ()
broadcastMessage sendFun users message =
    mapM_
    ( \target -> do when (userId target /= userId message)
                         (withAsync (sendFun target message) wait)
    )
    users

-- | This method sends a message to a network target, e.g. client.
-- This method "ensure" that everything is sent.  That means that if
-- only a part of the message could be sent the function tries to sent
-- the rest afterwards.
sendMessage :: (NetworkTarget s, Message m) =>
               s -> m -> IO ()
sendMessage target message = do
  go text
  where text = messageText message
        socket = targetSocket target
        transmit :: String -> IO Int
        transmit = send socket
        go s = if null s
               then return ()
               else do bytesSent <- transmit s
                       go (drop bytesSent s)

