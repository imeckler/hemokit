module Hemokit.Pipes where

import Control.Monad

import Hemokit
import Pipes
import qualified Pipes.Prelude as P
import           Hemokit.Internal.Utils (untilNothing)

rawSource :: (MonadIO m) => EmotivDevice -> Proxy x' x () EmotivRawData m ()
rawSource dev = void $ untilNothing (liftIO (readEmotivRaw dev)) yield

parsePackets :: (MonadIO m) => EmotivDevice -> Pipe EmotivRawData (EmotivState, EmotivPacket) m r
parsePackets dev = P.mapM (liftIO . updateEmotivState dev)

emotivStates :: (MonadIO m) => EmotivDevice -> Proxy a' a () EmotivState m ()
emotivStates dev = void (rawSource dev >-> parsePackets dev >-> P.map fst)

emotivPackets :: (MonadIO m) => EmotivDevice -> Proxy a' a () EmotivPacket m ()
emotivPackets dev = void (rawSource dev >-> parsePackets dev >-> P.map snd)

