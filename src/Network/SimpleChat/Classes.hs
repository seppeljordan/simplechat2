-- | This module contains abstractions over the important parts and
-- bits that are used to implement the chat program.
--
-- We use so many type classes to enable the code to develop easily by
-- not restricting ourselves to one specific implementation of our
-- data types.
module Network.SimpleChat.Classes
    ( -- * Non-IO
      User (..)
    , Message (..)
    , Named (..)
    , Forceable (..)
    , RenderChar (..)
    , (<|), (|>)
    , ChatCommand (..)
    , isBroadcast
    , isRename
    -- * IO
    , CreationTime (..)
    , NetworkTarget (..)
    , EventHandler (..)
    , EventSource (..)
    , InputSource (..)
    )
where

import Data.Maybe
import Network.Socket
import qualified Network.TLS as TLS
import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Sequence as Seq
import System.IO
import Data.Foldable
import Data.Monoid
import Data.Time

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
    readString :: a -> IO String
    -- | send a string to the target.  The function should return how
    -- many characters were really send.
    sendString :: a -> String -> IO Int

instance NetworkTarget Socket where
    disconnect s = close s
    readString sock =
        recv sock 256
    sendString = send

instance NetworkTarget TLS.Context where
    disconnect cont = do
      TLS.bye cont
      TLS.contextClose cont
    readString cont =
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

-- | Modells a structure that can be displayed in a list of string.
class RenderChar a where
    renderChar :: a -> [String]

-- | Modells a source of character input.
class InputSource a where
    inputChar :: a -> IO Char
    -- | The predefined implementation of `inputLine` will ask for one
    -- character at a time and check if it got a new line character.
    -- If you need a more efficient implementation (you probably do)
    -- you should implement it yourself.
    inputLine :: a -> IO String
    inputLine h =
        go Seq.empty
        where go xs = do x <- inputChar h
                         if x == '\n'
                         then return $ toList xs
                         else go (xs Seq.|> x)

instance InputSource Handle where
    inputChar = hGetChar
    inputLine = hGetLine

instance InputSource Socket where
    inputChar s = head <$> (recv s 1)

-- | Add a singleton to the right side of a monoid
(|>) :: (Applicative d, Monoid (d a)) =>
        d a -> a -> d a
(|>) deque single = deque <> pure single

-- | Add a singleton to the left side of a monoid
(<|) :: (Applicative d, Monoid (d a)) =>
        a -> d a -> d a
(<|) single deque = pure single <> deque

-- | Modell an object that carries a creation time.
class CreationTime a where
    creationTime :: a -> UTCTime
