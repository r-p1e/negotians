{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Output.AMQP where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue
       (TBQueue, newTBQueueIO, writeTBQueue)
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (MonadIO(liftIO))
import           Data.Binary.Builder            (toLazyByteString)
import           Data.ByteString.Lazy           (ByteString)
import           Data.ProtoLens                 (buildMessage)
import Proto.CommonLogRep (LogEntry)


data AMQPMessage
    = AMQPMessage ByteString
    | AMQPBatchTimer
    | AMQPStop

type BatchQueue = TBQueue AMQPMessage


initAMQP :: (BatchQueue -> IO ())
         -> IO (LogEntry -> IO ())
initAMQP writer = do
         queue <- newTBQueueIO tBQueueLen
         _ <- forkIO (writer queue)
         return (publish queue)

publish
    :: MonadIO m
    => BatchQueue -> LogEntry -> m ()
publish tbqueue message =
    liftIO
        (atomically
             (writeTBQueue
                  tbqueue
                  (AMQPMessage (toLazyByteString (buildMessage message)))))

tBQueueLen :: Int
tBQueueLen = 1000

