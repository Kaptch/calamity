module Calamity.Voice.Types where

import           Data.Aeson
import           Data.Generics.Labels             ()
import           Data.Text.Lazy

import           GHC.Generics

-- TODO
data ReceivedVoiceDiscordMessage
  = Ready -- 2
  | SessionDescription -- 4
  | SpeakingReq -- 5
  | HeartBeatAck -- 6
  | Hello Int -- 8
  | Resumed -- 9
  | ClientDisconnect -- 13
  deriving ( Show, Generic )

-- TODO
instance FromJSON ReceivedVoiceDiscordMessage where
  parseJSON = withObject "ReceivedVoiceDiscordMessage" $ \v -> do
    op :: Int <- v .: "op"
    case op of
      2  -> pure Ready
      4  -> pure SessionDescription
      5  -> pure SpeakingReq
      6  -> pure HeartBeatAck
      8  -> Hello <$> do
        d <- v .: "d"
        d .: "heartbeat_interval"
      9  -> pure Resumed
      13 -> pure ClientDisconnect
      _  -> fail $ "invalid opcode: " <> show op

data SentVoiceDiscordMessage
  = Identify IdentifyData -- 0
  | SelectProtocol SelectProtocolData -- 1
  | HeartBeat (Maybe Int) -- 3
  | Speaking SpeakingData -- 5
  | Resume ResumeData -- 7
  deriving ( Show, Generic )

data IdentifyData = IdentifyData
  {
    serverID :: Text
  , userID :: Text
  , sessionID
  , token :: Text
  }
  deriving ( Show, Generic, ToJSON )

data SelectProtocolData = SelectProtocolData
  {
    protocol :: Text
  , address :: Text
  , port :: Int
  , mode :: Text
  }
  deriving ( Show, Generic, ToJSON )

data SpeakingData = SpeakingData
  {
    speaking :: Int
  , delay :: Int
  }
  deriving ( Show, Generic, ToJSON )

data ResumeData = ResumeData
  {
    token :: Text
  , serverID :: Text
  , sessingID :: Text
  }
  deriving ( Show, Generic, ToJSON )

instance ToJSON SentVoiceDiscordMessage where
  toJSON (Identify data') = object ["op" .= (0 :: Int), "d" .= data']
  toJSON (SelectProtocol data') = object ["op" .= (1 :: Int), "d" .= data']
  toJSON (HeartBeat data') = object ["op" .= (3 :: Int), "d" .= data']
  toJSON (Speaking data') = object ["op" .= (5 :: Int), "d" .= data']
  toJSON (Resume data') = object ["op" .= (7 :: Int), "d" .= data']
  toEncoding (Identify data') = pairs ("op" .= (0 :: Int) <> "d" .= data')
  toEncoding (SelectProtocol data') = pairs ("op" .= (1 :: Int) <> "d" .= data')
  toEncoding (HeartBeat data') = pairs ("op" .= (3 :: Int) <> "d" .= data')
  toEncoding (Speaking data') = pairs ("op" .= (5 :: Int) <> "d" .= data')
  toEncoding (Resume data') = pairs ("op" .= (7 :: Int) <> "d" .= data')
