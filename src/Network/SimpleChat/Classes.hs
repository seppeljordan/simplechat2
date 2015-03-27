-- | This module contains abstractions over the important parts and
-- bits that are used to implement the chat program.
--
-- We use so many type classes to enable the code to develop easily by
-- not restricting ourselves to one specific implementation of our
-- data types.
module Network.SimpleChat.Classes
    ( User (..)
    , Message (..)
    , Named (..)
    , NetworkTarget (..)
    , EventHandler (..)
    , EventSource (..)
    , ChatCommand (..)
    , isBroadcast
    , isRename
    , Forceable (..)
    )
where

import Data.Maybe
import Network.Socket
import qualified Network.TLS as TLS
import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L

-- | This class modells data that is associated with a user.
class User a where
    -- | Return the assigned user id
    userId :: a -> Integer

-- | Modell a structure that has a name.              
class Named a where
    -- | query the name of an structure
    name :: a -> String
    -- | rename a structure
    rename :: String -> a -> a

-- | Modells a structure that holds a message string
class Message a where
    messageText :: a -> String

-- | This class modells data that is associated with a socket.  You
-- can use this for example to modell a user that is connected on a
-- specific socket.
class NetworkTarget a where
    -- | Close the connection to a target
    disconnect :: a -> IO ()
    -- | read characters from a target
    readLine :: a -> IO String
    -- | send a string to the target.  The function should return how
    -- many characters were really send.
    sendString :: a -> String -> IO Int

instance NetworkTarget Socket where
    disconnect s = close s
    readLine sock =
        go ""
        where go str = do
                answer <- recv sock 1
                if answer == "\n"
                then return (str ++ answer)
                else go (str++answer)
    sendString = send

instance NetworkTarget TLS.Context where
    disconnect cont = do
      TLS.bye cont
      TLS.contextClose cont
    readLine cont =
        S.unpack <$> TLS.recvData cont
    sendString cont msg = do
        TLS.sendData cont (L.pack msg)
        return (length msg)

-- | Modell a structure that can register callback functions.                 
class EventHandler a where
    newEventHandler :: IO (a b)
    -- | Runs all registered IO actions
    fire :: a b -> b -> IO ()
    -- | This function registers an IO action that is executed when
    -- the handler is fired.  The returned function should unregister
    -- the formerly registered function
    register :: a b -> (b -> IO ()) -> IO (IO ())

-- | Modells a data structure that can be turned into a event source.
class EventSource a where
    getEvents :: (Frameworks t) =>
                 a b -> Moment t (Event t b)

-- | This class modells structures that can be interpreted as a chat
-- command.
class ChatCommand a where
    getBroadcastMessage :: a -> Maybe String
    getDirectionalMessage :: a -> Maybe (Integer, String)
    getRenameText :: a -> Maybe String
    isQuit :: a -> Bool
    isHelp :: a -> Bool
    isError :: a -> Bool
    isWho :: a -> Bool

-- | Check if a command can be interpreted as a broadcast action.
isBroadcast :: (ChatCommand c) =>
                      c -> Bool
isBroadcast cmd = isJust (getBroadcastMessage cmd)

-- | Check if a command can be interpreted as a renaming action.
isRename :: (ChatCommand c) =>
            c -> Bool
isRename cmd = isJust (getRenameText cmd)

-- | This class modells data that can be force to evaluate
-- (strictness).
class Forceable a where
    -- | Force should evaluate the object 'deeply'.
    force :: a -> ()

instance Forceable a => Forceable [a] where
    force [] = ()
    force (x:xs) = force x `seq` force xs
