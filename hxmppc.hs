{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeOperators #-}
import Network.Protocol.XMPP (Message(..), MessageType(..), ReceivedStanza(..),
                              parseJID, getStanza, putStanza)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network (PortID(..), PortNumber)
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO (stderr)
import Control.Monad
import Control.Monad.IO.Class
import Data.Functor
import EasyXMPP
import Data.Record.Label
-- import Control.Arrow
import System.Console.GetOpt

data Settings = Settings { _user :: T.Text
                         , _pass :: T.Text
                         , _host :: String
                         , _port :: PortNumber
                         , _help :: Bool
                         }
$(mkLabels [''Settings])
type Flag = Settings -> Settings

defaultSettings :: Settings
defaultSettings = Settings { _user = ""
                           , _pass = ""
                           , _host = ""
                           , _port = 5222
                           , _help = False
                           }

options :: [OptDescr Flag]
options =
  [ Option "u" ["user"] (ReqArg (setL user . T.pack)             "USER") "Username (example: foo@jabber.org)"
  , Option "p" ["pass"] (ReqArg (setL pass . T.pack)             "PASS") "Password"
  , Option "h" ["host"] (ReqArg (setL host)                      "HOST") "Hostname (example: jabber.org)"
  , Option "P" ["port"] (ReqArg (setL port . fromInteger . read) "PORT") "Port"
  , Option "?" ["help"] (NoArg (setL help True))                         "Show this help message"
  ]

usage :: MonadIO m => T.Text -> m b
usage msg = liftIO $ T.hPutStrLn stderr msg' >> exitFailure
  where msg' = T.unlines [T.pack $ usageInfo "Usage: hxmppc [<option>*] <command>" options
                         ,"command ::= tell <destination-user> <message>*"
                         ,"          | wait"
                         ,""
                         ,msg
                         ]

main :: IO ()
main = do
  (flags, nonopts, errs) <- getOpt Permute options <$> getArgs
  let opts = foldr ($) defaultSettings flags
  when (not . null $ errs) $ usage (T.concat . map T.pack $ errs)
  when (getL help opts) $ usage ""
  when (T.null $ getL user opts) $ usage "Missing username"
  when (T.null $ getL pass opts) $ usage "Missing password"
  when (null   $ getL host opts) $ usage "Missing hostname"
  let Just botJID = parseJID (getL user opts)

  res <- runPersistentClient (botJID, getL pass opts) (getL host opts, PortNumber (getL port opts)) $ do
    case map T.pack nonopts of
      "tell" : args -> do
        case args of
          []  -> usage "`tell' expects a destination user and a message"
          [_] -> usage "`tell' expects a message as well"
          (to:msg) -> do
             toJID <- maybe (usage "`tell' bad format for destination user") return $ parseJID to
             putStanza $ mkMsg toJID (T.unwords msg)
             _ <- getStanza
             liftIO exitSuccess
      "wait" : args -> do
        case args of
          [] ->
            forever $ do
              message <- getStanza
              case message of
                ReceivedMessage msg | messageType msg /= MessageError ->
                  let text = maybe "" ((<>": "). T.pack . show) (messageFrom msg) <> extractMsg msg in
                  liftIO (T.putStrLn text >> exitSuccess)
                _ -> return ()
          _ -> usage "`wait' expects no arguments"
      [] -> usage $ "no command"
      cmd : _ -> usage $ "no such command " <> cmd
  case res of
    Left err -> usage $ "XMPP error:" <> T.pack (show err)
    Right _  -> return ()
