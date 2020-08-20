module Calamity.Voice.Types where

import           Calamity.Types.Model.Guild
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake

import           Data.Aeson
import           Data.Generics.Labels             ()
import           Data.Text.Lazy

import           GHC.Generics

data ReceivedVoiceDiscordMessage
  = Ready ReadyData
  | SessionDescription SessionDescriptionData
  | SpeakingReq SpeakingData
  | HeartBeatAck Int
  | Hello Int
  | Resumed
  | ClientDisconnect
  deriving ( Show, Generic )

data ReadyData = ReadyData
  {
    ssrc :: Int
  , ip :: Text
  , port :: Int
  , modes :: [Mode]
  }
  deriving ( Show, Generic, FromJSON )

data Mode = XSALSA20_POLY1305
  | XSALSA20_POLY1305_SUFFIX
  | XSALSA20_POLY1305_LITE
  deriving ( Show, Generic, ToJSON, FromJSON )

data SessionDescriptionData = SessionDescriptionData
  {
    mode :: Mode
  , secret_key :: [Int]
  }
  deriving ( Show, Generic, FromJSON )

data SpeakingData = SpeakingData
  {
    speaking :: Int
  , delay :: Int
  , ssrc :: Int
  }
  deriving ( Show, Generic, ToJSON, FromJSON )

instance FromJSON ReceivedVoiceDiscordMessage where
  parseJSON = withObject "ReceivedVoiceDiscordMessage" $ \v -> do
    op :: Int <- v .: "op"
    case op of
      2  -> Ready <$> do
        d <- v .: "d"
        parseJSON d
      4  -> SessionDescription <$> do
        d <- v .: "d"
        parseJSON d
      5  -> SpeakingReq <$> do
        d <- v .: "d"
        parseJSON d
      6  -> HeartBeatAck <$> v .: "d"
      8  -> Hello <$> do
        d <- v .: "d"
        d .: "heartbeat_interval"
      9  -> pure Resumed
      13 -> pure ClientDisconnect
      _  -> fail $ "invalid opcode: " <> show op

data SentVoiceDiscordMessage
  = Identify IdentifyData
  | SelectProtocol SelectProtocolData
  | Speaking SpeakingData
  | HeartBeat (Maybe Int)
  | Resume ResumeData
  deriving ( Show, Generic )

data IdentifyData = IdentifyData
  {
    serverID :: Snowflake Guild
  , userID :: Snowflake User
  , sessionID
  , token :: Text
  }
  deriving ( Show, Generic, ToJSON )

data SelectProtocolData = SelectProtocolData
  {
    protocol :: Text
  , address :: Text
  , port :: Int
  , mode :: Mode
  }
  deriving ( Show, Generic, ToJSON )

data ResumeData = ResumeData
  {
    token :: Text
  , serverID :: Snowflake Guild
  , sessionID :: Text
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
