{-# LANGUAGE TemplateHaskell #-}

module Calamity.Voice.Client where

import           Calamity.Internal.RunIntoIO
import           Calamity.Types.Model.Channel.Guild.Voice
import           Calamity.Types.Model.Guild
import           Calamity.Types.Model.User
import           Calamity.Types.Snowflake
import           Calamity.Voice.Types
import           Calamity.Types.LogEff
import           Calamity.Internal.Utils

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.Chan.Unagi as UC
import           Control.Concurrent.STM.TBMQueue
import           Control.Exception.Safe
import           Control.Lens
import           Control.Monad
import           Control.Monad.STM

import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import           Data.IORef
import           Data.Maybe
import           Data.Text.Lazy

import           Fmt

import           GHC.Generics

import           Network.Socket (PortNumber(..))
import           Network.WebSockets

import           Polysemy
import qualified Polysemy                      as P
import qualified Polysemy.Reader               as P
import qualified Polysemy.Async                as P
import qualified Polysemy.AtomicState          as P
import qualified Polysemy.Error                as P
import qualified Polysemy.Resource             as P

import           Prelude                       hiding (error, lookup)

import           Wuss

import TextShow.Generic
import TextShow (TextShow)

data VoiceConnectionControl
  = VoiceConnectionRestart
  | VoiceConnectionShutDown
  deriving ( Show, Generic )
  deriving ( TextShow ) via FromGeneric VoiceConnectionControl

data VoiceMsg
  = Discord ReceivedVoiceDiscordMessage
  | Control VoiceConnectionControl
  deriving ( Show, Generic )

data VoiceConnectionState = VoiceConnectionState
  { channelID  :: Snowflake VoiceChannel
  , mute       :: Bool
  , deaf       :: Bool
  
  , sessionID  :: Text
  , token      :: Text
  , endpoint   :: Text
  
  , hbThread   :: Maybe (Async (Maybe ()))
  , hbResponse :: Bool
  , seqNum     :: Maybe Int
  , mainWs     :: Maybe Connection
  }
  deriving ( Generic )

data VoiceConnection = VoiceConnection
  { guildID    :: Snowflake Guild
  , userID     :: Snowflake User
  , cmdOut     :: OutChan VoiceConnectionControl
  }
  deriving ( Generic )

type VoiceC r = (P.Members '[ LogEff, P.Reader VoiceConnection
                            , P.AtomicState VoiceConnectionState
                            , P.Embed IO, P.Final IO, P.Async ] r)

newVoiceConnectionState :: Snowflake VoiceChannel
  -> Bool
  -> Bool
  -> Text
  -> Text
  -> Text
  -> VoiceConnectionState
newVoiceConnectionState chID mute deaf sessionID token endpoint =
  VoiceConnectionState chID mute deaf sessionID token endpoint
    Nothing False Nothing Nothing

newVoiceConnection :: (P.Members '[ LogEff
                                  , P.Embed IO
                                  , P.Final IO
                                  , P.Async ] r)
  => Snowflake Guild
  -> Snowflake User
  -> Snowflake VoiceChannel
  -> Bool
  -> Bool
  -> Text
  -> Text
  -> Text
  -> Sem r (UC.InChan VoiceConnectionControl, Async (Maybe ()))
newVoiceConnection gID uID cID mute deaf sessionID token endpoint = do
  (cmdIn, cmdOut) <- P.embed UC.newChan
  initSt <- P.embed . newIORef $
    newVoiceConnectionState cID mute deaf sessionID token endpoint
  let d = VoiceConnection gID uID cmdOut
  thread <- P.async $
    P.runReader d $
    P.runAtomicStateIORef initSt (void outerloop)
  pure (cmdIn, thread)

runWebsocket :: Members '[P.Final IO, P.Embed IO] r
  => Text
  -> Text
  -> PortNumber
  -> (Connection -> P.Sem r a)
  -> P.Sem r (Maybe a)
runWebsocket host path port ma = do
  inner <- bindSemToIO ma
  embed $ runSecureClient (unpack host) port (unpack path) inner

sendToWs :: VoiceC r => SentVoiceDiscordMessage
  -> Sem r ()
sendToWs data' = do
  st <- P.atomicGet
  let encodedData = encode data'
  debug $ "Sending " +|| data' ||+ " encoded to " +|| encodedData ||+ " to voice server"
  case (st ^. #mainWs) of
    Nothing -> do
      debug "Attempted to send to a closed ws"
    Just conn -> do
      P.embed . sendTextData conn $ encodedData

-- TODO: handle port number
outerloop :: VoiceC r => Sem r (Either VoiceConnectionControl ())
outerloop = P.runError . forever $ do
  st <- P.atomicGet
  let host = st ^. #endpoint
  let host' = fromMaybe host $ stripPrefix "wss://" host
  let host'' = Data.Text.Lazy.dropWhileEnd (== ':') host'
  debug $ "Starting new voice connection to " +|| host' ||+ " " +|| host'' ||+""
  innerLoopVal <- runWebsocket host'' "" 80 innerloop
  case innerLoopVal of
    Just VoiceConnectionRestart -> do
      debug "Restarting voice client"
    Just VoiceConnectionShutDown -> do
      debug "Shutting down voice client"
      P.throw VoiceConnectionShutDown
    Nothing -> do
      debug "Something bad happened, restarting voice client"

innerloop :: (VoiceC r, P.Member (P.Error VoiceConnectionControl) r)
  => Connection
  -> Sem r VoiceConnectionControl
innerloop ws = do
  debug "Entering inner loop of a voice connection"
  
  P.atomicModify (#mainWs ?~ ws)
  
  seqNum <- P.atomicGets (^. #seqNum)
  gID <- P.asks (^. #guildID)
  uID <- P.asks (^. #userID)
  sessionID <- P.atomicGets (^. #sessionID)
  token <- P.atomicGets (^. #token)
  
  case seqNum of
    Just n -> do
      debug $
        "Resuming voice connection (sessionID: " +| sessionID
        |+ ", seq: " +| n
        |+ ")"
      sendToWs (Resume $ ResumeData token gID sessionID)
    Nothing -> do
      debug "Idenifying voice connection"
      sendToWs (Identify $ IdentifyData gID uID sessionID token)

  d <- P.ask
  result <- P.resourceToIOFinal $ P.bracket (P.embed $ newTBMQueueIO 1)
    (P.embed . atomically . closeTBMQueue)
    (\q -> do
        _controlThread <- P.async . P.embed $ controlStream d q
        _discordThread <- P.async . P.embed $ discordStream ws q
        forever $ do
          msg <- P.embed . atomically $ readTBMQueue q
          debug (pack $ show msg)
          case msg of
            Nothing -> pure ()
            Just msg -> handleMsg msg
    )

  debug "Exiting inner loop of a voice connection"

  P.atomicModify (#mainWs .~ Nothing)
  stopHb
  pure result

handleMsg :: (VoiceC r, P.Member (P.Error VoiceConnectionControl) r)
  => VoiceMsg
  -> Sem r ()
handleMsg (Discord msg) = case msg of
  Ready data' ->
    debug . pack . show $ data'
  SessionDescription data' ->
    debug . pack . show $ data'
  SpeakingReq data' ->
    debug . pack . show $ data'
  HeartBeatAck _ -> do
    debug "Received heartbeat voice client ack"
    P.atomicModify (#hbResponse .~ True)
  Hello i -> do
    debug $ "Received hello with " +| i |+ " ms"
    startHb i
  Resumed ->
    debug "Resumed"
  ClientDisconnect ->
    debug "Client disconnected"
handleMsg (Control msg) = case msg of
  VoiceConnectionRestart ->
    P.throw VoiceConnectionRestart
  VoiceConnectionShutDown ->
    P.throw VoiceConnectionShutDown

controlStream :: VoiceConnection -> TBMQueue VoiceMsg -> IO ()
controlStream conn outqueue = do
  v <- UC.readChan (conn ^. #cmdOut)
  r <- atomically $ do
    b <- tryWriteTBMQueue outqueue (Control v)
    case b of
      Nothing -> pure False
      Just True -> pure True
      Just False -> retry
  when r (controlStream conn outqueue)

discordStream :: Connection -> TBMQueue VoiceMsg -> IO ()
discordStream conn outqueue = do
  msg :: Either SomeException BS.ByteString <-
    catchAny (Right <$> receiveData conn) (\ex -> pure $ Left ex)
  case msg of
    Left _ -> do
      atomically $ writeTBMQueue outqueue (Control VoiceConnectionRestart)
    Right msg -> do
      let decoded = eitherDecode msg
      case decoded of
        Left _ -> do
          pure ()
        Right msg -> do
          r <- atomically $ do
            b <- tryWriteTBMQueue outqueue (Discord msg)
            case b of
              Nothing -> pure False
              Just True -> pure True
              Just False -> retry
          when r (discordStream conn outqueue)

stopHb :: VoiceC r => Sem r ()
stopHb = do
  st <- P.atomicGet
  case (st ^. #hbThread) of
    Nothing ->
      pure ()
    Just thread -> do
      P.embed (void $ cancel thread)
      P.atomicPut $ st & #hbThread .~ Nothing
      pure ()

sendHb :: VoiceC r => Sem r ()
sendHb = do
  st <- P.atomicGet
  let sn = st ^. #seqNum
  debug $ "Sending heartbeat (seq: " +|| sn ||+ ")"
  sendToWs $ HeartBeat sn
  P.atomicModify (#hbResponse .~ False)

hbLoop :: VoiceC r => Int -> Sem r ()
hbLoop interval = void . P.runError . forever $ do
  sendHb
  P.embed . threadDelay $ interval * 1000
  unlessM (P.atomicGets (^. #hbResponse)) $ do
    debug "No heartbeat response, restarting voice connection"
    mainWs <- P.note () =<< P.atomicGets (^. #mainWs)
    P.embed $ sendCloseCode mainWs 4000 ("No heartbeat in time" :: Text)
    P.throw ()

startHb :: VoiceC r => Int -> Sem r ()
startHb interval = do
  stopHb
  thread <- P.async $ hbLoop interval
  P.atomicModify (#hbThread ?~ thread)
