{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Output.AMQP where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue
       (TBQueue, newTBQueueIO, writeTBQueue)
import Control.Concurrent (forkIO)
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Data.Binary.Builder            (toLazyByteString)
import           Data.ByteString.Lazy           (ByteString)
import           Data.ProtoLens                 (buildMessage, def)
import           Internal                       (Msg, Source, Token)
import           Lens.Family2                   ((&), (.~))

import           Proto.EventLog                 (Severity)

import           Proto.InnerLogRep              (LogRep, msg, severity, source,
                                                 timestamp, token)


data AMQPMessage
    = AMQPMessage ByteString
    | AMQPBatchTimer
    | AMQPStop

type BatchQueue = TBQueue AMQPMessage


initAMQP :: (BatchQueue -> IO ())
         -> IO (Token -> Double -> Severity -> Source -> Msg -> IO ())
initAMQP writer = do
         queue <- newTBQueueIO tBQueueLen
         _ <- forkIO (writer queue)
         return (publish queue)

publish
    :: MonadIO m
    => BatchQueue -> Token -> Double -> Severity -> Source -> Msg -> m ()
publish tbqueue tkn timemp svrty src message =
    liftIO
        (atomically
             (writeTBQueue
                  tbqueue
                  (AMQPMessage
                       (toLazyByteString
                            (buildMessage
                                 (encodeLogRep tkn timemp svrty src message))))))

encodeLogRep :: Token -> Double -> Severity -> Source -> Msg -> LogRep
encodeLogRep tkn timemp svrty src message =
    (((((def & severity .~ svrty) & timestamp .~ timemp) & msg .~ message) &
      source .~
      src) &
     token .~
     tkn)

tBQueueLen :: Int
tBQueueLen = 1000

