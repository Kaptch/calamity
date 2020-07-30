module Calamity.Types.Model.Voice.VoiceState
    ( VoiceState(..) ) where

import           Calamity.Internal.AesonThings
import           Calamity.Types.Model.Channel.Guild.Voice
import {-# SOURCE #-} Calamity.Types.Model.Guild.Guild
import {-# SOURCE #-} Calamity.Types.Model.Guild.Member
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake

import           Data.Aeson
import           Data.Text.Lazy                           ( Text )

import           Debug.Trace
import           Data.Aeson.Types
import qualified Data.Text.Internal as IT

import           GHC.Generics

import           TextShow
import qualified TextShow.Generic                         as TSG

data VoiceState = VoiceState
  { guildID    :: Maybe (Snowflake Guild)
  , channelID  :: Maybe (Snowflake VoiceChannel)
  , userID     :: Snowflake User
  , member     :: Maybe Member
  , sessionID  :: Text
  , deaf       :: Bool
  , mute       :: Bool
  , selfDeaf   :: Bool
  , selfMute   :: Bool
  , selfStream :: Maybe Bool
  , selfVideo  :: Bool
  , suppress   :: Bool
  }
  deriving ( Show, Eq, Generic )
  deriving ( TextShow ) via TSG.FromGeneric VoiceState
  deriving ( ToJSON ) via CalamityJSON VoiceState

instance FromJSON VoiceState where
  parseJSON = withObject "VoiceState" $ \v -> do
    g_id <- v .:? "guild_id"
    c_id <- ((.:?) :: Object -> IT.Text -> Parser (Maybe (Snowflake VoiceChannel))) v "channel_id"
    trace (show v) $ pure ()
    trace (show c_id) $ pure ()
    let member = case g_id of
          Nothing -> pure Nothing
          Just g_id' -> do
            member' <- v .:? "member"
            traverse (\m -> parseJSON $ Object (m <> "guild_id" .= g_id')) member'
    VoiceState g_id
      <$> v .:? "channel_id"
      <*> v .: "user_id"
      <*> member
      <*> v .: "session_id"
      <*> v .: "deaf"
      <*> v .: "mute"
      <*> v .: "self_deaf"
      <*> v .: "self_mute"
      <*> v .:? "self_stream"
      <*> v .: "self_video"
      <*> v .: "suppress"
