{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Slack where

import Import
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as LB (ByteString, pack)
import Data.Aeson
import qualified Yesod.Core.Json as YJ
import Data.Maybe (fromJust)

data EventType = UrlVerification {challenge::Text} |
                 Message {user::Text, user_message::Text} |
                 EventTypeError deriving (Show)

base_url :: String
base_url = "https://slack.com/api/"

postSlackR :: Handler YJ.Value
postSlackR = do
        curbody <- requireCheckJsonBody :: Handler Value
        let eventType = getEventType curbody
        case eventType of
            UrlVerification c -> do
                returnJson $ "{'challenge':'"++ c ++"'}"
            Message u um      -> do
                let eum = u ++ " said: " ++ um
                _ <- liftIO $ sendMessage (unpack eum)
                return $ "{}"
            _                 -> do
                $(logInfo) "Not handled event"
                return $ "{}"

{-
SLACK
|-}
getSlackAuthToken :: ByteString
getSlackAuthToken = "Bearer " ++ (Data.Text.Encoding.encodeUtf8 $ slackAuthToken compileTimeAppSettings)

getEventType :: Value -> EventType
getEventType (Object l) = fromJust $
                          isUrlVerification l <|>
                          isMessage l <|>
                          (Just EventTypeError)
getEventType _ = EventTypeError

isUrlVerification :: HashMap Text Value -> Maybe EventType
isUrlVerification l = do
    _ <- lookup "type" l
    (String c) <- lookup "challenge" l
    return $ UrlVerification c

isMessage :: HashMap Text Value -> Maybe EventType
isMessage l = do
    (Object event) <- lookup "event" l
    case (lookup "bot_id" event) of -- Quick and dirty way to check if is bot's message
        Nothing -> do
                   (String user') <- lookup "user" event
                   (String user_message') <- lookup "text" event
                   return $ Message user' user_message'
        _       -> Nothing


sendMessage :: String -> IO Object
sendMessage m = do
    initialRequest <- parseRequest (base_url ++ "/chat.postMessage")
    let authToken = getSlackAuthToken
        request =  initialRequest { method = "POST",
                                    requestHeaders = [("Authorization", authToken), ("content-type","application/json")],
                                    requestBody = RequestBodyLBS echo_message
                                  }
    manager <- Network.HTTP.Client.newManager tlsManagerSettings
    response <- Network.HTTP.Client.httpLbs request manager
--     print.show $ responseStatus response
--     mapM_ print $ responseHeaders response
--     LB.putStrLn $ responseBody response
    case ((decode $ responseBody response) :: Maybe Object) of -- Quick and dirty way to parse response
            Just v -> return v
            _      -> error "error on posting message to slack"
    where echo_message :: LB.ByteString
          echo_message = LB.pack $ "{\"channel\":\"#testing\",\"text\":\"" ++ m ++ "\"}"
