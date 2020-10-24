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

data EventType = UrlVerification {challenge::Text} | EventTypeError deriving (Show)

base_url :: String
base_url = "https://slack.com/api/"

sample_body_json :: LB.ByteString
sample_body_json = LB.pack "{\"channel\":\"#testing\",\"text\":\"A API message from HSlackBot\"}"

postSlackR :: Handler YJ.Value
postSlackR = do
        curbody <- requireCheckJsonBody :: Handler Value
        let eventType = getEventType curbody
        case eventType of
            UrlVerification c -> do
                returnJson $ "{'challenge':'"++ c ++"'}"
            _                 -> do
                response <- liftIO $ sendMessage
                returnJson response

{-
SLACK
|-}
getSlackAuthToken :: ByteString
getSlackAuthToken = "Bearer " ++ (Data.Text.Encoding.encodeUtf8 $ slackAuthToken compileTimeAppSettings)

getEventType :: Value -> EventType
getEventType (Object l) = fromJust $ isUrlVerification l <|> (Just EventTypeError)
getEventType _ = EventTypeError

isUrlVerification :: HashMap Text Value -> Maybe EventType
isUrlVerification l = do
    _ <- lookup "type" l
    (String c) <- lookup "challenge" l
    return $ UrlVerification c

sendMessage :: IO Object
sendMessage = do
    initialRequest <- parseRequest (base_url ++ "/chat.postMessage")
    let authToken = getSlackAuthToken
        request =  initialRequest { method = "POST",
                                    requestHeaders = [("Authorization", authToken), ("content-type","application/json")],
                                    requestBody = RequestBodyLBS sample_body_json
                                  }
    manager <- Network.HTTP.Client.newManager tlsManagerSettings
    response <- Network.HTTP.Client.httpLbs request manager
--     print.show $ responseStatus response
--     mapM_ print $ responseHeaders response
--     LB.putStrLn $ responseBody response
    case ((decode $ responseBody response) :: Maybe Object) of -- Quick and dirty way to parse response
            Just v -> return v
            _      -> error "error on posting message to slack"
