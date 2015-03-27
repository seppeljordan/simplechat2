import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Event.Handler as Handler
import Control.Monad hiding
    ( mapM_ )
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Network.SimpleChat.Classes
import Network.Socket
import Prelude hiding
    ( mapM_
    , foldl
    )
import Reactive.Banana
import Reactive.Banana.Frameworks hiding
    ( register )
import Text.Parsec

newtype SentMessage = SentMessage (Integer, String)

newtype Client = Client (Integer, String, Socket)

clientSocket :: Client -> Socket
clientSocket (Client (_,_,s)) = s

newtype EvHandler a = EvHandler (Handler.AddHandler a, Handler.Handler a)

newtype TextMessage = TextMessage String

data Cmd = CmdQuit
         | CmdHelp
         | CmdBroadcast String
         | CmdWho
         | CmdRename String
         | CmdError

instance User SentMessage where
    userId (SentMessage (uid,_)) = uid

instance User Client where
    userId (Client (uid, _, _)) = uid

instance Named Client where
    name (Client (_,n,_)) = n
    rename newName (Client (uid,_,sock)) = Client (uid,newName,sock)

instance Message SentMessage where
    messageText (SentMessage (_,text)) = text

instance Message TextMessage where
    messageText (TextMessage s) = s

instance NetworkTarget Client where
    disconnect cl = disconnect (clientSocket cl)
    sendString cl = sendString (clientSocket cl)
    readLine cl = readLine (clientSocket cl)

instance EventHandler EvHandler where
    newEventHandler = EvHandler <$> Handler.newAddHandler
    fire (EvHandler (_,trigger)) x = trigger x
    register (EvHandler (addHandle, _)) action = Handler.register addHandle action

instance EventSource EvHandler where
    getEvents (EvHandler (addHandler,_)) =
        fromAddHandler addHandler

instance ChatCommand Cmd where
    getBroadcastMessage (CmdBroadcast s) = Just s
    getBroadcastMessage _ = Nothing
    getDirectionalMessage _ = Nothing
    getRenameText (CmdRename s) = Just s
    getRenameText _ = Nothing
    isQuit CmdQuit = True
    isQuit _ = False
    isHelp CmdHelp = True
    isHelp _ = False
    isError CmdError = True
    isError _ = False
    isWho CmdWho = True
    isWho _ = False

instance ChatCommand TextMessage where
    getBroadcastMessage (TextMessage s) = getBroadcastMessage $ parseCmd s
    getDirectionalMessage (TextMessage s) = getDirectionalMessage $ parseCmd s
    getRenameText (TextMessage s) = getRenameText $ parseCmd s
    isHelp (TextMessage s) = isHelp $ parseCmd s
    isError (TextMessage s) = isError $ parseCmd s
    isQuit (TextMessage s) = isQuit $ parseCmd s
    isWho (TextMessage s) = isWho $ parseCmd s

instance ChatCommand SentMessage where
    getBroadcastMessage (SentMessage (_,s)) = getBroadcastMessage $ parseCmd s
    getDirectionalMessage (SentMessage (_,s)) = getDirectionalMessage $ parseCmd s
    getRenameText (SentMessage (_,s)) = getRenameText $ parseCmd s
    isHelp (SentMessage (_,s)) = isHelp $ parseCmd s
    isError (SentMessage (_,s)) = isError $ parseCmd s
    isQuit (SentMessage (_,s)) = isQuit $ parseCmd s
    isWho (SentMessage (_,s)) = isWho $ parseCmd s

instance Forceable SentMessage where
    force (SentMessage (uid,m)) = uid `seq` m `seq` ()

instance Forceable Client where
    force (Client (uid,n,sock)) = uid `seq` n `seq` sock `seq` ()

parseCmd :: String -> Cmd
parseCmd text =
    case parse ( do res <- eventually command `alternative` broadcast
                    _ <- clearRest
                    return res
               ) "" text of
      Left _ -> CmdError
      Right x -> x
    where
      alternative = (Text.Parsec.<|>)
      eventually = Text.Parsec.try
      broadcast = do beginChar <- noneOf "/"
                     rest <- Text.Parsec.many anyChar
                     return (CmdBroadcast (beginChar:rest))
      command = char '/' >>
                ( eventually quit `alternative`
                  eventually help `alternative`
                  eventually who `alternative`
                  renameCmd
                )
      help = string "help" >> return CmdHelp
      quit = string "quit" >> return CmdQuit
      who = string "who" >> return CmdWho
      renameCmd = string "rename" >>
                  char ' ' >>
                  Text.Parsec.many alphaNum >>= \newName ->
                  clearRest >>
                  if null newName
                  then return CmdError
                  else return (CmdRename newName)
      clearRest = Text.Parsec.many anyChar

helpText :: String
helpText =
    unlines [ "Help:"
            , "\t/help: Show this message"
            , "\t/quit: Quit this program"
            , "\t/who: Show who is online"
            , "\t/rename NICK: Change name to NICK"
            ]

-- | Execute the corresponding io action to a chat command.
execChatCommand :: ( ChatCommand c, User c
                   , User u, NetworkTarget u, Named u
                   , Foldable f) =>
                   f u -> c -> IO ()
execChatCommand users chatCmd = do
  when (isHelp chatCmd) (sendMessage originalUser (TextMessage $ helpText))
  when (isWho chatCmd) (sendMessage originalUser (TextMessage $ who users))
  when (isRename chatCmd) (do broadcastMessage
                                sendMessage
                                users
                                (SentMessage ( userId chatCmd
                                             , show (userId chatCmd)++" -> "++newName++"\n"
                                             )
                                )
                              sendMessage originalUser (TextMessage $ "Renamed to "++newName++"\n")
                          )
  when (isBroadcast chatCmd) ( case getBroadcastMessage chatCmd of
                                 Nothing -> return ()
                                 Just s -> broadcastMessage sendMessage users (SentMessage (userId chatCmd,s))
                             )
  when (isQuit chatCmd) (do sendMessage originalUser $ TextMessage "Goodbye :)\n"
                            disconnect originalUser
                            putStrLn $ "User "++show (userId chatCmd)++" disconnected"
                        )
  where originalUser =
            maybe
            (error "execChatCommand: cannot associate message with any user")
            id
            (find (\x -> userId x == userId chatCmd) users)
        who xs =
            foldl
            (\s user -> s ++ show (userId user) ++ "\t" ++ (name user) ++ "\n")
            "ID\tNAME\n"
            xs
        newName = maybe "<incognito>" id (getRenameText chatCmd)

-- | Delete a user from the userlist when they disconnect.
handleQuit :: ( ChatCommand c, User c
              , User u
              , Foldable f, Monoid (f u), Applicative f) =>
              f u -> c -> f u
handleQuit users cmd
    | isQuit cmd =
        foldl
        (\accu user -> if userId user /= userId cmd
                       then accu <> pure user
                       else accu)
        mempty
        users
    | otherwise = users

handleRename :: ( ChatCommand c, User c
                , User u, Named u
                , Functor f ) =>
                c -> f u -> f u
handleRename cmd users
    | isRename cmd = (\user -> if userId user == userId cmd
                               then rename newName user
                               else user) <$> users
    | otherwise = users
    where newName = maybe "<incognito>" id (getRenameText cmd)

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
        transmit :: String -> IO Int
        transmit = sendString target
        go s = if null s
               then return ()
               else do bytesSent <- transmit s
                       go (drop bytesSent s)

-- | Poll a listening socket for new connections and trigger an event
-- handler when a connection occurs.
runSocketHandler :: (EventHandler h) =>
                    Socket -> (Socket -> IO b) -> h b -> IO ThreadId
runSocketHandler sock f handler = do
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
  _ <- runSocketHandler sock return connectEvents
  network <- compile $ do
               newConnections <- getEvents connectEvents
               userReadyEvent <- getEvents userReady
               newMessages <- getEvents messageEvents
               let -- Behavior
                   users = accumB [] ( unions [ (:) <$> newUser
                                              , flip handleQuit <$> quittingUser
                                              , handleRename <$> userRename
                                              ]
                                     )
                   -- Events
                   newUser = getId ((\sct uid -> Client (uid,"<incognito>",sct)) <$> newConnections)
                   userRename = filterE isRename newMessages
                   quittingUser = filterE isQuit newMessages
                   commandErrors = filterE isError newMessages
                   -- Functions
                   greet user = do
                         putStrLn $ "User connected, user id is "++show (userId user)
                         sendMessage user $ TextMessage (greeting user)
                         fire userReady user
                   greeting user =
                       unlines [ "Hello"
                               , "Your id is "++show (userId user)
                               , "Happy Chatting"
                               ]
                   getMessage user = do
                         msg <- readLine user
                         return (SentMessage (userId user, msg))
               reactimate $ greet <$> newUser
               reactimate $ (\user -> do
                               _ <- withCallbackDo
                                    user
                                    getMessage
                                    userReady
                                    messageEvents
                               return ()
                            ) <$> userReadyEvent
               reactimate $ execChatCommand <$> users <@> newMessages
               reactimate $ (\targets leavingUser ->
                                 mapM_
                                 (flip sendMessage $ TextMessage $
                                       show (userId leavingUser) ++" has left the chat\n")
                                 (filter (\targ -> userId targ /= userId leavingUser) targets)
                            ) <$> users <@> quittingUser
               reactimate $ (\curUsers message ->
                                 case find (\user -> userId user == userId message) curUsers of
                                   Just x -> sendMessage
                                             x
                                             ( TextMessage ("***Syntax error\n"++helpText))
                                   Nothing -> return ()
                            ) <$>
                            users <@>
                            commandErrors
  actuate network
  _ <- forever $ threadDelay (10*10^(6 :: Int))
  return ()
    where port = 4141
