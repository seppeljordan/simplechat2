import Data.Foldable
import Network.Socket
import Reactive.Banana
import Reactive.Banana.Frameworks hiding
    ( register )
import Prelude hiding
    ( mapM_ )
import Control.Monad hiding
    ( mapM_ )
import Network.Socket
import Control.Concurrent.Async
import qualified Control.Event.Handler as Handler
import Control.Exception
import Control.Concurrent
import Data.Maybe (isJust)

newtype SentMessage = SentMessage (Integer, String)

newtype Client = Client (Integer, Socket)

newtype EvHandler a = EvHandler (Handler.AddHandler a, Handler.Handler a)

newtype TextMessage = TextMessage String

-- | This class modells data that is associated with a user.
class User a where
    userId :: a -> Integer

instance User SentMessage where
    userId (SentMessage (uid,_)) = uid

instance User Client where
    userId (Client (uid,_)) = uid

class Message a where
    messageText :: a -> String

instance Message SentMessage where
    messageText (SentMessage (_,text)) = text

instance Message TextMessage where
    messageText (TextMessage s) = s

-- | This class modells data that is associated with a socket.  You
-- can use this for example to modell a user that is connected on a
-- specific socket.
class NetworkTarget a where
    targetSocket :: a -> Socket

instance NetworkTarget Client where
    targetSocket (Client (_,s)) = s

instance NetworkTarget Socket where
    targetSocket = id

class EventHandler a where
    newEventHandler :: IO (a b)
    -- | Runs all registered IO actions
    fire :: a b -> b -> IO ()
    -- | This function registers an IO action that is executed when
    -- the handler is fired.  The returned function should unregister
    -- the formerly registered function
    register :: a b -> (b -> IO ()) -> IO (IO ())

instance EventHandler EvHandler where
    newEventHandler = EvHandler <$> Handler.newAddHandler
    fire (EvHandler (_,trigger)) x = trigger x
    register (EvHandler (addHandle, _)) action = Handler.register addHandle action

class EventSource a where
    getEvents :: (Frameworks t) =>
                 a b -> Moment t (Event t b)

instance EventSource EvHandler where
    getEvents (EvHandler (addHandler,_)) =
        fromAddHandler addHandler

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
        sock = targetSocket target
        transmit :: String -> IO Int
        transmit = send sock
        go s = if null s
               then return ()
               else do bytesSent <- transmit s
                       go (drop bytesSent s)


readN :: (NetworkTarget u) =>
         Int -> u -> IO String
readN n user = recv (targetSocket user) n


-- | Poll a listening socket for new connections and trigger an event
-- handler when a connection occurs.
runSocketHandler :: (EventHandler h, NetworkTarget a) =>
                    a -> (Socket -> IO b) -> h b -> IO ThreadId
runSocketHandler target f handler = do
  putStrLn "Installing socket handler"
  ok <- isListening sock
  when (not ok) (fail "runSocketHandler: socket not listening")
  forkIO $ go sock
    where
      -- | accept a connection on a socket, fire the event handler and
      -- continue with listening on the socket.
      go :: Socket -> IO ()
      go listenSock = do (s,_) <- accept listenSock
                         f s >>= fire handler
                         go listenSock
      sock = targetSocket target

-- | Execute a IO action and issue a callback, when the action was
-- performed.
withCallbackDo :: (EventHandler h) =>
                  u -> (u -> IO b) -> h u -> h b -> IO (Async ())
withCallbackDo user action userReady resultReady =
    async $ do result <- action user
               fire resultReady result
               fire userReady user

getId :: Event t (Integer -> a) -> Event t a
getId ev =
    let curId = accumB 0 ( (+1) <$ ev )
    in flip ($) <$> curId <@> ev

main :: IO ()
main = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet port iNADDR_ANY)
  listen sock 5
  connectEvents <- (newEventHandler :: IO (EvHandler Socket))
  messageEvents <- (newEventHandler :: IO (EvHandler SentMessage))
  userReady <- (newEventHandler :: IO (EvHandler Client))
  thread <- runSocketHandler sock return connectEvents
  network <- compile $ do
               newConnections <- getEvents connectEvents
               userReadyEvent <- getEvents userReady
               newMessages <- getEvents messageEvents
               let users = accumB [] ( unions [ userAdd
                                              , userQuit
                                              ]
                                     )
                   userAdd = (:) <$> newUser
                   userQuit = never
                   newUser = getId ((\sock uid -> Client (uid,sock)) <$> newConnections)
                   greet user = do
                         putStrLn $ "User connected, user id is "++show (userId user)
                         sendMessage user $ TextMessage (greeting user)
                         fire userReady user
                   greeting user = unlines [ "Hello", "Your id is "++show (userId user),"Happy Chatting" ]
                   getMessage user = do
                         msg <- readN 100 user
                         return (SentMessage (userId user, msg))
               reactimate $ greet <$> newUser
               reactimate $ (\user -> do
                               withCallbackDo
                                   user
                                   getMessage
                                   userReady
                                   messageEvents
                               return ()
                            ) <$> userReadyEvent
               reactimate $ broadcastMessage sendMessage <$> users <@> newMessages
  actuate network
  _ <- forever $ threadDelay (10*10^6)
  return ()
    where port = 4141
