{-# LANGUAGE OverloadedStrings #-}

module Main where

import            System.IO
import            Network.HTTP.Simple
import            Network.HTTP.Client.Internal
import qualified  Data.ByteString             as BS
import qualified  Data.ByteString.Lazy        as B
import qualified  Data.ByteString.Lazy.Char8  as B8
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
instance FromJSON Config where
  parseJSON (Object cfg)  = Config
                            <$>
                            cfg .: "about"
                            <*>
                            cfg .: "repeat"
                            <*>
                            cfg .: "repeat_question"
                            <*>
                            cfg .: "offset"
  parseJSON _             = mzero
instance ToJSON Config where
  toJSON (Config ab rep repq off) = object [
                                    "about"           .= ab,
                                    "repeat"          .= rep,
                                    "repeat_question" .= repq,
                                    "offset"          .= off
                                    ]
instance Show Config where
  show (Config ab rep repq off) = "About=" ++ ab ++
                                  "\nNumber of repetitions=" ++ show rep ++
                                  "\nQuestion for repetitions=" ++ repq ++
                                  "\nOffset=" ++ show off

-- single message
data Message = Message  {update_id :: Integer, chat_id :: Integer, text :: String}
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

-- list of messages
data Messages = Messages [Message]
instance FromJSON Messages where
  parseJSON (Object msgs) = Messages <$> msgs .: "result"

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



readCfg :: IO Config
readCfg = do
  rawJSON <- BS.readFile fileCfg
  -- rawJSON <- withFile fileCfg ReadMode (\handle -> B.hGetContents handle)
  let result = decodeStrict rawJSON :: Maybe Config
  return $ case result of
    Nothing -> defaultCfg
    Just js -> js

--todo bracketsOnError
writeCfg :: Config -> IO ()
writeCfg cfg = withFile fileCfg WriteMode (\handle -> do
  B8.hPutStr handle $ Pr.encodePretty cfg)
{-
  bracketOnError
    (openTemplFile "." "temp")
    (\(tempName, tempHandle) -> do
      hClose templHandle
      removeFile tempName)
    (\(tempName, tempHandle) -> do
      B.hPutStr tempHandle cfg
      hClose tempHandle
      renameFile tempName fileCfg)-}

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
sendMsg :: Message -> IO ()
sendMsg msg = do
  url <- parseRequest $ botURL ++ "sendMessage?chat_id=" ++ (show . chat_id $ msg) ++ "&text=" ++ (text msg)
  let request = setRequestProxy (Just (Proxy "127.0.0.1" 9041)) $ url
  httpLBS request
  return ()

repeatMsg :: Int -> Message -> IO ()
repeatMsg 0 _ = return ()
repeatMsg n msg = do
  sendMsg msg
  repeatMsg (n-1) msg

showRepeat :: Message -> IO ()
showRepeat msg = do
  key <- BS.readFile "/home/urban/haskell/keyboard.json"
  --let key = "&reply_markup={\"keyboard\":[[{\"text\":\"1\"},{\"text\":\"2\"},{\"text\":\"3\"},{\"text\":\"4\"},{\"text\":\"5\"}]]}"
  url <- parseRequest $ botURL ++ "sendMessage?chat_id=" ++ (show . chat_id $ msg) ++ "&text=Enter number of repetitions" ++ "&reply_markup=" -- ++ key
  let request = setRequestProxy (Just (Proxy "127.0.0.1" 9041)) $ url
  httpLBS request
  return ()


data Key = Key {keytext :: String}
instance ToJSON Key where
  toJSON (Key key) = object ["text" .= key]

data KeyRow = KeyRow [Key]
-- instance ToJSON KeyRow where
--   toJSON (KeyRow key) =
-- data Keyboard = Keyboard [KeyRow]
-- instance ToJSON Keyboard where

k1 = Key "key 1"
k2 = Key "key 2"
k3 = Key "key 3"
k4 = Key "key 4"
k5 = Key "key 5"

krow1 = KeyRow [k1, k2, k3]

main :: IO ()
main = do
  B8.putStrLn $ Pr.encodePretty <$> krow1



main' = do
  print "Waiting for updates"
  msg <- lastMsg
  cfg <- readCfg
  writeCfg $ cfg {offset = 1 + update_id msg}
  case text msg of
    "/help"   -> sendMsg msg {text = about cfg}
    "/repeat" -> showRepeat msg
    otherwise -> repeatMsg (fromIntegral . repeatNumber $ cfg) msg
  main
