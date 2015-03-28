-- | This module defines functions for handling asyncronous IO events.
module Network.SimpleChat.Callback
    ( -- * Generell utility
      withCallbackDo
    , installNetworkHandler
    , installInputHandler
    -- * Concrete implementation
    , EvHandler
    , newEvHandler
    )

where

import qualified Control.Event.Handler as Handler
import Network.SimpleChat.Classes
import Control.Concurrent
import Control.Concurrent.Async
import Reactive.Banana.Frameworks hiding
    ( register )
import Data.Functor

-- | Implements a simple `EventHandler` and an `EventSource`
newtype EvHandler a = EvHandler (Handler.AddHandler a, Handler.Handler a)

instance EventHandler EvHandler where
    newEventHandler = newEvHandler
    fire (EvHandler (_,trigger)) x = trigger x
    register (EvHandler (addHandle, _)) action = Handler.register addHandle action

instance EventSource EvHandler where
    getEvents (EvHandler (addHandler,_)) =
        fromAddHandler addHandler

-- | Instanciate a new `EvHandler`
newEvHandler :: IO (EvHandler a)
newEvHandler = EvHandler <$> Handler.newAddHandler

-- | Execute a IO action and issue a callback, when the action was
-- performed.  This prcedure spawnes an asyncronous computation that
-- is only blocking in case of unsafe calls.
withCallbackDo :: (EventHandler h, Forceable b, Forceable u) =>
                  u -> (u -> IO b) -> h u -> h b -> IO (Async ())
withCallbackDo user action userReady resultReady =
    async ( do result <- force user `seq` action user
               fire resultReady result
               fire userReady user
          )

-- | Run a process that polls an `InputSource` and fires a newly
-- created event handler when input is received.
installInputHandler :: (InputSource i, EventHandler h) =>
                       i -> IO (h String, ThreadId)
installInputHandler inputSource = do
  inputHandler <- newEventHandler
  procId <- forkFinally ( let go = inputLine inputSource >>= fire inputHandler >> go
                          in go ) (const (return ()))

  return (inputHandler, procId)

-- | Run a process that polls an `NetworkTarget` and fires a newly
-- created event handler when input is received.
installNetworkHandler :: (NetworkTarget n, EventHandler h) =>
                         n -> IO (h String, ThreadId)
installNetworkHandler network = do
  networkHandler <- newEventHandler
  procId <- forkFinally ( let go = readString network >>= fire networkHandler >> go
                          in go ) (const (return ()))
  return (networkHandler, procId)
