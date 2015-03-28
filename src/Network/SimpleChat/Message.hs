module Network.SimpleChat.Message where

import Control.Monad hiding
    ( mapM_ )
import Data.Maybe
import Network.SimpleChat.Classes
import Prelude hiding
    ( mapM_
    , foldl
    )
import Text.Parsec

newtype TextMessage = TextMessage String

instance ChatCommand TextMessage where
    getBroadcastMessage (TextMessage s) = getBroadcastMessage $ parseCmd s
    getDirectionalMessage (TextMessage s) = getDirectionalMessage $ parseCmd s
    getRenameText (TextMessage s) = getRenameText $ parseCmd s
    isHelp (TextMessage s) = isHelp $ parseCmd s
    isError (TextMessage s) = isError $ parseCmd s
    isQuit (TextMessage s) = isQuit $ parseCmd s
    isWho (TextMessage s) = isWho $ parseCmd s

instance Message TextMessage where
    messageText (TextMessage s) = s

data Cmd = CmdQuit
         | CmdHelp
         | CmdBroadcast String
         | CmdWho
         | CmdRename String
         | CmdError

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

-- | Get a string converted to a command of type `Cmd`.
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
