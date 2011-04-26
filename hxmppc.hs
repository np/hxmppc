{-# LANGUAGE OverloadedStrings #-}
import Network.Protocol.XMPP (Message(..), MessageType(..), ReceivedStanza(..),
                              parseJID, getStanza, putStanza)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Network (PortID(..))
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr)
import Control.Monad
import Control.Monad.IO.Class
import Data.Functor
import EasyXMPP

usage :: MonadIO m => String -> m b
usage msg = liftIO $ hPutStrLn stderr msg >> exitFailure

main :: IO ()
main = do
  (user:host:pass:cmd:args) <- map T.pack <$> getArgs
  let Just botJID = parseJID user

  res <- runPersistentClient (botJID, pass) (T.unpack host, PortNumber 5222) $ do
    case cmd of
      "tell" -> do
        case args of
          []  -> usage "`tell' expects a destination user and a message"
          [_] -> usage "`tell' expects a message as well"
          (to:msg) -> do
             toJID <- maybe (usage "`tell' bad format for destination user") return $ parseJID to
             putStanza $ mkMsg toJID (T.unwords msg)
             _ <- getStanza
             liftIO exitSuccess
      "wait" -> do
        case args of
          [] ->
            forever $ do
              message <- getStanza
              case message of
                ReceivedMessage msg | messageType msg /= MessageError ->
                  liftIO (T.putStrLn (extractMsg msg) >> exitSuccess)
                _ -> return ()
          _ -> usage "`wait' expects no arguments"
      _ -> usage $ "no such command " ++ T.unpack cmd
  case res of
    Left err -> usage $ "XMPP error:" ++ show err
    Right _  -> return ()
