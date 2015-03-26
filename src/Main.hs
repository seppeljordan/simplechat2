import Data.Foldable
import Network.Socket
import Reactive.Banana
import Prelude hiding
    ( mapM_ )
import Control.Monad hiding
    ( mapM_ )
import Network.Socket
import Control.Concurrent.Async
import qualified Control.Event.Handler as Handler
import Control.Exception

-- | This class modells data that is associated with a user.
class User a where
    userId :: a -> Integer

class Message a where
    messageText :: a -> String

-- | This class modells data that is associated with a socket.  You
-- can use this for example to modell a user that is connected on a
-- specific socket.
class NetworkTarget a where
    targetSocket :: a -> Socket

instance NetworkTarget Socket where
    targetSocket = id

class EventHandler a where
    newEventHandler :: (b -> IO ()) -> a b
    -- | Runs all registered IO actions
    fire :: a b -> b -> IO ()
    -- | This function registers an IO action that is executed when
    -- the handler is fired.  The returned function should unregister
    -- the formerly registered function
    register :: a b -> (b -> IO ()) -> IO (IO ())

class EventSource a where
    getEvents :: a b -> Moment t (Event t b)

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

-- | Poll a listening socket for new connections and trigger an event
-- handler when a connection occurs.
runSocketHandler :: (EventHandler h, NetworkTarget a) =>
                    a -> (Socket -> IO b) -> h b -> IO (Async ())
runSocketHandler target f handler =
    bracket
    -- start the process
    ( do ok <- isListening sock
         when (not ok) (fail "runSocketHandler: socket not listening")
         (async $ go sock)
    )
    -- catch clause
    (\t -> do cancel t
              close sock)
    -- just return the process itself
    return
    where
      -- | accept a connection on a socket, fire the event handler and
      -- continue with listening on the socket.
      go :: Socket -> IO ()
      go listenSock= do (s,_) <- accept listenSock
                        f s >>= fire handler
                        go listenSock
      sock = targetSocket target

-- | Execute a IO action and issue a callback, when the action was
-- performed.
withCallbackDo :: (EventHandler h) =>
                  u -> (u -> IO b) -> h u -> h b -> IO ()
withCallbackDo user action userReady resultReady =
    bracket
    ( async $ do result <- action user
                 fire resultReady result
                 fire userReady user )
    cancel
    ( const (return ()))
