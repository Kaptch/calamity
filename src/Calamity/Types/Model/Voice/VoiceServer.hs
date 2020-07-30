module Calamity.Types.Model.Voice.VoiceServer
    ( VoiceServer(..) ) where

import           Calamity.Internal.AesonThings
import           Calamity.Types.Model.Guild.Guild
import           Calamity.Types.Snowflake

import           Data.Aeson
import           Data.Text.Lazy                           ( Text )

import           GHC.Generics

import           TextShow
import qualified TextShow.Generic                         as TSG

data VoiceServer = VoiceServer
  { token      :: Text
  , guildID    :: Snowflake Guild
  , endpoint   :: Text
  }
  deriving ( Show, Eq, Generic )
  deriving ( TextShow ) via TSG.FromGeneric VoiceServer
  deriving ( FromJSON, ToJSON ) via CalamityJSON VoiceServer
