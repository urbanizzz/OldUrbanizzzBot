{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Main where

import            System.IO
import            GHC.Generics
import            Network.HTTP.Simple
import            Network.HTTP.Client.Internal
import qualified  Data.ByteString             as BS
import qualified  Data.ByteString.Lazy        as B
import qualified  Data.ByteString.Lazy.Char8  as B8
import qualified  Data.ByteString.Lazy.UTF8   as BU
import            Data.Aeson
import qualified  Data.Aeson.Encode.Pretty    as Pr
import            Control.Monad               (mzero)
import            Control.Applicative         ((<$>), (<*>))

-- config
data Config = Config {
  about           :: String,
  repeatNumber    :: Integer,
  repeatQuestion  :: String,
  offset          :: Integer }
  deriving (Generic, ToJSON, FromJSON)
instance Show Config where
  show (Config ab rep repq off) = "About=" ++ ab ++
                                  "\nNumber of repetitions=" ++ show rep ++
                                  "\nQuestion for repetitions=" ++ repq ++
                                  "\nOffset=" ++ show off

type TCommand = String
data TKey = TKey {keytext :: String} deriving (Show, Generic)
instance ToJSON TKey where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = dropKey }
data TKeyboard = TKeyboard { keyboard :: [[TKey]], one_time_keyboard :: Bool } deriving (Show, Generic, ToJSON)

dropKey :: String -> String
dropKey "keytext" = "text"
dropKey s = s

k1 = TKey "key 1"
k2 = TKey "key 2"
k3 = TKey "key 3"
k4 = TKey "key 4"
k5 = TKey "key 5"

kb = TKeyboard [[k1, k2], [k4]] True

-- single message
data Message = Message  {update_id :: Integer, chat_id :: Integer, text :: String} deriving Show
instance FromJSON Message where
  parseJSON (Object msg)  = Message
                            <$>
                            -- extract update_id
                            (msg .: "update_id")
                            <*>
                            -- extracting chat_id (message -> chat -> id)
                            (msg .: "message" >>= (.: "chat") >>= (.: "id"))
                            <*>
                            -- extracting text of a message (message -> text)
                            (msg .: "message" >>= (.: "text"))
  parseJSON _             = mzero
instance ToJSON Message where
  toJSON (Message _ chat_id txt) = object [ "chat_id" .= chat_id, "text" .= txt ]

-- list of messages
data Messages = Messages [Message]
instance FromJSON Messages where
  parseJSON (Object msgs) = Messages <$> msgs .: "result"

data ReplyMessage = ReplyMessage {replyText :: String} deriving Show
instance FromJSON ReplyMessage where
  parseJSON (Object msg) = ReplyMessage <$> (msg .: "result" >>= (.: "text"))

botURL = "https://api.telegram.org/bot949284451:AAGK8fCgIhv2KLcmT8Mz_bf-3hAl0Ccp7pA/"
fileCfg = "config.json"
defaultCfg  = Config {
                about = "UrbanizzzBot",
                repeatNumber = 1,
                repeatQuestion = "Enter the number of repetitions",
                offset = 0 }
emptyMsg =  Message {
              update_id = 0,
              chat_id = 0,
              text = "" }
-- chat_id = 845633894


readCfg :: IO Config
readCfg = do
  rawJSON <- BS.readFile fileCfg
  let result = eitherDecodeStrict rawJSON :: Either String Config
  return $ case result of
    Left s -> error $ "error parsing JSON of config: " ++ s -- defaultCfg
    Right js -> js

--todo bracketsOnError
writeCfg :: Config -> IO ()
writeCfg cfg = withFile fileCfg WriteMode (\handle -> do
  B8.hPutStr handle $ Pr.encodePretty cfg)

fetchJSON :: IO B.ByteString
fetchJSON = do
  cfg <- readCfg
  url <- parseRequest $ botURL ++ "getUpdates?offset=" ++ (show . offset $ cfg) ++ "&timeout=60"
  let request = url {
    proxy           = Just (Proxy {proxyHost = "127.0.0.1", proxyPort = 9041}),
    responseTimeout = ResponseTimeoutNone }
  res <- httpLBS request
  return (getResponseBody res)

-- getting last message
lastMsg :: IO Message
lastMsg = do
  rawJSON <- fetchJSON
  let result = decode rawJSON :: Maybe Messages
  return $ case result of
    Nothing -> emptyMsg
    Just (Messages js) -> if null js then emptyMsg else last $ js

-- sending message
{-sendMsg :: Message -> IO ()
sendMsg msg = do
  url <- parseRequest $ botURL ++ "sendMessage?chat_id=" ++ (show . chat_id $ msg) ++ "&text=" ++ (text msg)
  let request = setRequestProxy (Just (Proxy "127.0.0.1" 9041)) $ url
  httpLBS request
  return ()-}
sendMsg :: Value -> IO B.ByteString
sendMsg requestObject = do
  initialRequest <- parseRequest $ botURL ++ "sendMessage"
  let request = initialRequest
        { method = "POST",
          requestBody = RequestBodyLBS $ encode requestObject,
          requestHeaders =
            [ ("Content-Type", "application/json; charset=utf-8") ],
          proxy = Just $ Proxy "127.0.0.1" 9041,
          responseTimeout = ResponseTimeoutNone
        }
  res <- httpLBS request
  return (getResponseBody res)

repeatMsg :: Int -> Message -> IO ()
repeatMsg 0 _ = return ()
repeatMsg n msg = do
  sendMsg . toJSON $ msg
  repeatMsg (n-1) msg

showRepeat :: Message -> IO ()
showRepeat msg = do
  let val = object [ "chat_id" .= chat_id msg, "text" .= text msg, "reply_markup" .= kb ]
  rawJSON <- sendMsg val
  let result = decode rawJSON :: Maybe ReplyMessage
  putStrLn $ case result of
    Nothing -> show $ ReplyMessage "Empty"
    Just msg -> show msg
  return ()

showHelp :: Message -> IO ()
showHelp msg = do
  sendMsg . toJSON $ msg
  return ()

main :: IO ()
main = do
  print "Waiting for updates"
  msg <- lastMsg
  cfg <- readCfg
  writeCfg $ cfg {offset = 1 + update_id msg}
  case text msg of
    "/help"   -> showHelp msg {text = about cfg}
    "/repeat" -> showRepeat msg {text = repeatQuestion cfg}
    otherwise -> repeatMsg (fromIntegral . repeatNumber $ cfg) msg
  main
