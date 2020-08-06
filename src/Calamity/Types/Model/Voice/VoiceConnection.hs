module Calamity.Types.Model.Voice.VoiceConnection where

import           Calamity.Types.Model.Channel
import           Calamity.Types.Model.Guild
import           Calamity.Types.Snowflake

import           Network.WebSockets           (Connection)

import           Data.Text.Lazy

import           GHC.Generics

data VoiceConnection = VoiceConnection
  { guildID   :: Snowflake Guild
  , channelID :: Snowflake Channel
  , mute      :: Bool
  , deaf      :: Bool
  , sessionID :: Text
  , token     :: Text
  , endpoint  :: Text
  , mainWS    :: Connection
  }
  deriving ( Generic )
