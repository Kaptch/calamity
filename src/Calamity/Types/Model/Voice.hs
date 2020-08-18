-- | Voice data types
module Calamity.Types.Model.Voice
    ( module Calamity.Types.Model.Voice.VoiceState
    , module Calamity.Types.Model.Voice.VoiceRegion
    , module Calamity.Types.Model.Voice.VoiceServer
    , VoiceInitData(..) ) where

import           Calamity.Types.Model.Voice.VoiceRegion
import           Calamity.Types.Model.Voice.VoiceServer
import           Calamity.Types.Model.Voice.VoiceState

import           GHC.Generics

data VoiceInitData = VoiceInitData
  { voiceServer :: Maybe VoiceServer
  , voiceState  :: Maybe VoiceState
  }
  deriving ( Generic )
