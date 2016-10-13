{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Output.RMQ where


import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (readTBQueue)
import Control.Exception (SomeException(..), catch)
import Data.Text (Text)
import Output.AMQP (BatchQueue, AMQPMessage(..))
import Network.AMQP
       (AMQPException(ChannelClosedException, ConnectionClosedException),
        DeliveryMode(Persistent), ExchangeOpts(exchangeName, exchangeType),
        Message(msgBody, msgDeliveryMode))
import qualified Network.AMQP as AMQP
import Control.Retry
       (defaultLogMsg, exponentialBackoff, logRetries, recovering)
import Data.ByteString.Lazy (ByteString)

import System.IO (hPrint, stderr)


rmqWriter :: BatchQueue -> Text -> Text -> IO ()
rmqWriter queue username pwd = do
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
                      username
                      pwd)
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
            AMQPMessage msg ->
                publishLog channel msg >> loop connection channel
            AMQPStop -> AMQP.closeConnection connection
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
                      ConnectionClosedException _ ->
                          rmqWriter queue username pwd
                      ChannelClosedException _ -> rmqWriter queue username pwd)
