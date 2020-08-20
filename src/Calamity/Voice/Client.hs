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
import           Data.IORef
import           Data.Maybe
import           Data.Text.Lazy
import           Data.Void

import           DiPolysemy                      hiding ( debug, error, info )

import           Fmt

import           GHC.Generics

import           Network.Socket (PortNumber)
import           Network.WebSockets

import           Polysemy
import qualified Polysemy                      as P
import qualified Polysemy.Reader               as P
import qualified Polysemy.Async                as P
import qualified Polysemy.AtomicState          as P
import qualified Polysemy.Error                as P
import qualified Polysemy.Resource             as P

import           Prelude                       hiding (error, lookup)
import           TextShow.Generic
import           TextShow                      (TextShow, showtl)
import           System.Random                 (random, getStdRandom)

import           Wuss

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
  , nonce      :: Maybe Int
  , initial    :: Bool
  , hbThread   :: Maybe (Async (Maybe ()))
  , hbResponse :: Bool
  , mainWs     :: Maybe Connection
  -- TODO: refactor
  , ssrc       :: Maybe Int
  , ip         :: Maybe Text
  , port       :: Maybe Int
  , modes      :: Maybe [Mode]
  , mode       :: Maybe Mode
  , secretKey  :: Maybe [Int]
  }
  deriving ( Generic )

data VoiceConnection = VoiceConnection
  { guildID    :: Snowflake Guild
  , userID     :: Snowflake User
  , cmdOut     :: OutChan VoiceConnectionControl
  }
  deriving ( Generic )

type VoiceC r = (P.Members '[ LogEff
                            , P.Embed IO
                            , P.Final IO
                            , P.Async
                            , P.Reader VoiceConnection
                            , P.AtomicState VoiceConnectionState ] r)

newVoiceConnectionState :: Snowflake VoiceChannel
  -> Bool
  -> Bool
  -> Text
  -> Text
  -> Text
  -> VoiceConnectionState
newVoiceConnectionState chID mute deaf sessionID token endpoint =
  VoiceConnectionState chID mute deaf sessionID token endpoint
    Nothing True Nothing False Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing

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
  let runVoiceConnection =
        P.runReader d $
        P.runAtomicStateIORef initSt (void outerloop)
  let action = push "calamity-voice-connection" $
        attr "guild_id" (showtl gID) $
        runVoiceConnection
  thread <- P.async action
  pure (cmdIn, thread)

runWebsocket :: Members '[ P.Embed IO
                         , P.Final IO ] r
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

outerloop :: VoiceC r => Sem r (Either VoiceConnectionControl ())
outerloop = P.runError . forever $ do
  st <- P.atomicGet
  let host = st ^. #endpoint
  let host' = fromMaybe host $ stripPrefix "wss://" host
  let host'' = fromMaybe host' $ stripSuffix ":80" host'
  innerLoopVal <- runWebsocket host'' "/?v=4" 443 innerloop
  case innerLoopVal of
    Just VoiceConnectionRestart -> do
      debug "Restarting voice client"
    Just VoiceConnectionShutDown -> do
      debug "Shutting down voice client"
      P.throw VoiceConnectionShutDown
    Nothing -> do
      debug "Something bad happened, restarting voice client"

innerloop :: VoiceC r
  => Connection
  -> Sem r VoiceConnectionControl
innerloop ws = do
  debug "Entering inner loop of a voice connection"
  
  P.atomicModify (#mainWs ?~ ws)
  
  gID <- P.asks (^. #guildID)
  uID <- P.asks (^. #userID)
  sessionID <- P.atomicGets (^. #sessionID)
  token <- P.atomicGets (^. #token)
  initial <- P.atomicGets (^. #initial)
  
  case initial of
    False -> do
      debug $
        "Resuming voice connection (sessionID: " +|| sessionID ||+ ")"
      sendToWs (Resume $ ResumeData token gID sessionID)
    True -> do
      debug "Idenifying voice connection"
      P.atomicModify (#initial .~ False)
      sendToWs (Identify $ IdentifyData gID uID sessionID token)

  d <- P.ask
  
  result <- P.resourceToIOFinal $ P.bracket (P.embed $ newTBMQueueIO 1)
    (P.embed . atomically . closeTBMQueue)
    (\q -> do
        _controlThread <- P.async $ controlStream d q
        _discordThread <- P.async $ discordStream ws q
        (fromEitherVoid <$>) . P.raise . P.runError . forever $ do
          msg <- P.embed . atomically $ readTBMQueue q
          debug (pack $ show msg)
          case msg of
            Nothing -> pure ()
            Just msg -> handleMsg msg
    )

  P.atomicModify (#mainWs .~ Nothing)
  stopHb
  pure result

fromEitherVoid :: Either a Void -> a
fromEitherVoid (Left a) = a
fromEitherVoid (Right a) = absurd a

handleWSException :: SomeException -> IO (Either (VoiceConnectionControl, Maybe Text) a)
handleWSException e = case fromException e of
  Just (CloseRequest code _)
    | code `elem` [1000, 4004, 4009, 4011, 4014] -> do
      pure $ Left (VoiceConnectionShutDown, Nothing)
    | code `elem` [4006] -> do
      pure $ Left (VoiceConnectionRestart, Nothing)
  e -> do
    pure $ Left (VoiceConnectionRestart, Just . pack . show $ e)

handleMsg :: (VoiceC r, P.Member (P.Error VoiceConnectionControl) r)
  => VoiceMsg
  -> Sem r ()
handleMsg (Discord msg) = case msg of
  Ready data' -> do
    P.atomicModify (#ssrc ?~ (data' ^. #ssrc))
    P.atomicModify (#ip ?~ (data' ^. #ip))
    P.atomicModify (#port ?~ (data' ^. #port))
    P.atomicModify (#modes ?~ (data' ^. #modes))
  SessionDescription data' -> do
    P.atomicModify (#mode ?~ (data' ^. #mode))
    P.atomicModify (#secretKey ?~ (data' ^. #secret_key))
  SpeakingReq data' -> do
    debug . pack . show $ data'
  HeartBeatAck receivedNonce -> do
    debug $ "Received heartbeat voice client ack with nonce: " +|| receivedNonce ||+ ""
    P.atomicModify (#hbResponse .~ True)
    savedNonce <- P.atomicGets (^. #nonce)
    case savedNonce of
      Nothing -> do
        P.throw VoiceConnectionRestart
      Just savedNonce -> do
        when (savedNonce == receivedNonce) $ do
          debug "Nonce conflict"
          P.throw VoiceConnectionRestart
  Hello i -> do
    debug $ "Received hello (interval: " +|| i ||+ ")"
    startHb i
  Resumed -> do
    debug "Resumed"
    -- TODO: handle resumed
  ClientDisconnect -> do
    debug "Client disconnected"
    -- TODO: handle client disconnect
handleMsg (Control msg) = case msg of
  VoiceConnectionRestart -> do
    P.throw VoiceConnectionRestart
  VoiceConnectionShutDown -> do
    P.throw VoiceConnectionShutDown

controlStream :: VoiceC r
  => VoiceConnection -> TBMQueue VoiceMsg -> Sem r ()
controlStream conn outqueue = do
  v <- P.embed $ UC.readChan (conn ^. #cmdOut)
  r <- P.embed . atomically $ do
    b <- tryWriteTBMQueue outqueue (Control v)
    case b of
      Nothing -> do
        pure False
      Just True -> do
        pure True
      Just False -> do
        retry
  when r (controlStream conn outqueue)

discordStream :: VoiceC r
  => Connection -> TBMQueue VoiceMsg -> Sem r ()
discordStream conn outqueue = do
  msg <- P.embed $ catchAny (Right <$> receiveData conn) handleWSException
  debug $ "Received a message: " +|| msg ||+ ""
  case msg of
    Left (c, _) -> do
      P.embed . atomically $ writeTBMQueue outqueue (Control c)
    Right msg -> do
      let decoded = eitherDecode msg
      case decoded of
        Left _ -> do
          pure ()
        Right msg -> do
          r <- P.embed . atomically $ do
            b <- tryWriteTBMQueue outqueue (Discord msg)
            case b of
              Nothing -> do
                pure False
              Just True -> do
                pure True
              Just False -> do
                retry
          when r (discordStream conn outqueue)

stopHb :: VoiceC r => Sem r ()
stopHb = do
  st <- P.atomicGet
  case (st ^. #hbThread) of
    Nothing -> do
      pure ()
    Just thread -> do
      P.embed (void $ cancel thread)
      P.atomicPut $ st & #hbThread .~ Nothing
      pure ()

sendHb :: VoiceC r => Sem r ()
sendHb = do
  nonce <- P.embed $ getStdRandom random
  P.atomicModify (#nonce ?~ nonce)
  sendToWs $ HeartBeat (Just nonce)
  P.atomicModify (#hbResponse .~ False)

hbLoop :: VoiceC r => Int -> Sem r ()
hbLoop interval = void . P.runError . forever $ do
  sendHb
  P.embed . threadDelay $ interval * 1000
  unlessM (P.atomicGets (^. #hbResponse)) $ do
    debug "No heartbeat ACK, restarting voice connection"
    mainWs <- P.note () =<< P.atomicGets (^. #mainWs)
    P.embed $ sendCloseCode mainWs 4000 ("No heartbeat in time" :: Text)
    P.throw ()

startHb :: VoiceC r => Int -> Sem r ()
startHb interval = do
  stopHb
  thread <- P.async $ hbLoop interval
  P.atomicModify (#hbThread ?~ thread)
