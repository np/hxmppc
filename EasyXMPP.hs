{-# LANGUAGE OverloadedStrings #-}
module EasyXMPP (
  runPersistentClient,
  mkMsg,
  extractMsg,

  -- more details
  pBody,
  contentText,
  extractBody,
  extractContent,
  p
  ) where

import Network.Protocol.XMPP hiding (Node)
import Data.Text (Text)
import qualified Data.Text as T
import Data.XML.Types (Element(..), Node(..), Content(..), Name(..))
import Network (PortID(..), HostName)
import Data.Maybe (catMaybes)
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Data.Functor
import Data.Monoid

pBody :: [Node] -> Element
pBody = Element (Name "body" (Just "jabber:client") Nothing) []

contentText :: Text -> Node
contentText = NodeContent . ContentText

extractBody :: [Element] -> [Node]
extractBody [Element (Name "body" _ _) _ ns] = ns
extractBody _ = []

extractContent :: Node -> Maybe Text
extractContent (NodeContent (ContentText txt)) = Just txt
extractContent _ = Nothing

p :: Text -> Element
p = pBody . return . contentText

mkMsg :: JID -> Text -> Message
mkMsg toJID txt =
  Message { messageType     = MessageNormal
          , messageTo       = Just toJID
          , messageFrom     = Nothing -- done by the server
          , messageID       = Nothing
          , messageLang     = Just "en"
          , messagePayloads = [p txt]
          }

extractMsg :: Message -> Text
extractMsg = T.concat . catMaybes . map extractContent . extractBody . messagePayloads

-- Send a "ping" occasionally, to prevent server timeouts from
-- closing the connection.
sendPings :: Integer -> Session -> IO ()
sendPings seconds s = forever send where
  send = do
    -- Ignore errors
    _ <- runXMPP s $ putStanza ping
    threadDelay $ fromInteger $ 1000000 * seconds
  ping = (emptyIQ IQGet)
           { iqPayload = Just (Element pingName [] []) }

pingName :: Name
pingName = Name "ping" (Just "urn:xmpp:ping") Nothing

runPersistentClient :: (JID, Text) -> (HostName, PortID) -> XMPP a -> IO (Either Error a)
runPersistentClient (myJID, pass) (hostname, port) act =
  runClient server myJID username pass $ do
    -- Some servers will close the XMPP connection after some period
    -- of inactivity. For this example, we'll simply send a "ping" every
    -- 60 seconds
    _ <- getSession >>= liftIO . forkIO . sendPings 60

    _ <- bindJID myJID
    act

  where
    server = Server (JID Nothing (jidDomain myJID) Nothing) hostname port
    username = case strNode <$> jidNode myJID of
      Just x -> x
      Nothing -> error $ "JID must include a username"

