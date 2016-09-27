{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Output.AMQP (initAMQP) where

import           Control.Concurrent             (forkIO)
import           Control.Concurrent.STM         (atomically)
import           Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO,
                                                 readTBQueue, writeTBQueue)
import           Control.Exception              (SomeException (..), catch)
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Retry                  (defaultLogMsg,
                                                 exponentialBackoff, logRetries,
                                                 recovering)
import           Data.Binary.Builder            (toLazyByteString)
import           Data.ByteString.Lazy           (ByteString)
import           Data.ProtoLens                 (buildMessage, def)
import           Internal                       (Msg, Source, Token)
import           Lens.Family2                   ((&), (.~))
import           Network.AMQP                   (AMQPException (ChannelClosedException, ConnectionClosedException),
                                                 DeliveryMode (Persistent),
                                                 ExchangeOpts (exchangeName, exchangeType),
                                                 Message (msgBody, msgDeliveryMode))
import qualified Network.AMQP                   as AMQP
import           Proto.EventLog                 (Severity)
import           Proto.InnerLogRep              (LogRep, msg, severity, source,
                                                 timestamp, token)
import           System.IO                      (hPrint, stderr)


data RMQMessage = RMQMessage ByteString | RMQStop

initAMQP
    :: IO (Token -> Double -> Severity -> Source -> Msg -> IO ())
initAMQP = do
         queue <- newTBQueueIO tBQueueLen
         _ <- forkIO (rmqWriter queue)
         return (publish queue)

publish
    :: MonadIO m
    => TBQueue RMQMessage -> Token -> Double -> Severity -> Source -> Msg -> m ()
publish tbqueue tkn timemp svrty src message =
    liftIO
        (atomically
             (writeTBQueue
                  tbqueue
                  (RMQMessage
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

rmqWriter :: TBQueue RMQMessage -> IO ()
rmqWriter queue = do
    conn <-
        recovering
            (exponentialBackoff 50)
            [ logRetries
                  (\(ConnectionClosedException _) ->
                        return False)
                  (\b e rs ->
                        hPrint stderr $ defaultLogMsg b e rs)]
            (\_ ->
                  AMQP.openConnection
                      "rabbitmq.service.consul"
                      "/"
                      "guest"
                      "guest")
    chan <- AMQP.openChannel conn
    AMQP.declareExchange
        chan
        AMQP.newExchange
        { exchangeName = "logs"
        , exchangeType = "fanout"
        }
    loop conn chan
  where
    loop :: AMQP.Connection -> AMQP.Channel -> IO ()
    loop connection channel = do
        value <- atomically (readTBQueue queue)
        case value of
            RMQMessage msg -> publishLog channel msg >> loop connection channel
            RMQStop -> AMQP.closeConnection connection
    publishLog :: AMQP.Channel -> ByteString -> IO ()
    publishLog channel msg =
        catch
            (recovering
                 (exponentialBackoff 50)
                 [ logRetries
                       (\(ConnectionClosedException _) ->
                             return False)
                       (\b e rs ->
                             hPrint stderr $ defaultLogMsg b e rs)
                 , logRetries
                       (\(ChannelClosedException _) ->
                             return False)
                       (\b e rs ->
                             hPrint stderr $ defaultLogMsg b e rs)
                 , logRetries
                       (\(SomeException _) ->
                             return True)
                       (\b e rs ->
                             hPrint stderr $ defaultLogMsg b e rs)]
                 (\_ ->
                       AMQP.publishMsg
                           channel
                           "logs"
                           ""
                           (AMQP.newMsg
                            { msgBody = msg
                            , msgDeliveryMode = Just Persistent
                            }) >>
                       return ()))
            (\(e :: AMQPException) ->
                  case e of
                      ConnectionClosedException _ -> rmqWriter queue
                      ChannelClosedException _ -> rmqWriter queue)
