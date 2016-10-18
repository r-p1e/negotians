{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Output.GooglePubSub (initGPS) where

import           Control.Concurrent.STM         (atomically)
import           Control.Concurrent.STM.TBQueue (readTBQueue)
import           Control.Lens                   ((&), (.~), (<&>))
import           Control.Monad.Catch            (MonadCatch (catch))
import           Control.Retry                  (defaultLogMsg,
                                                 exponentialBackoff, logRetries,
                                                 recovering)
import           Data.ByteString.Lazy           (toStrict)
import           Data.Text                      (Text)
import Network.Google
       (Error(..), HasEnv(..), LogLevel(..), newEnv, newLogger, runGoogle,
        runResourceT, send)
import Network.Google.PubSub
       (pmData, prMessages, projectsTopicsCreate, projectsTopicsGet,
        projectsTopicsPublish, pubSubScope, publishRequest, pubsubMessage,
        tName, topic)
import Output.AMQP (AMQPMessage(..), BatchQueue, initAMQP)
import Proto.CommonLogRep (LogEntry)
import System.IO (stdout)


initGPS :: IO (LogEntry -> IO ())
initGPS = initAMQP gpsWriter

gpsWriter :: BatchQueue -> IO ()
gpsWriter queue = do
    lgr <- newLogger Debug stdout
    env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ pubSubScope)
    _ <-
        runResourceT . runGoogle env $
        catch
            (send (projectsTopicsGet topicName))
            (\(e :: Error) ->
                  send
                      (projectsTopicsCreate
                           (topic & tName .~ Just topicName)
                           topicName))
    loop env 0 []
  where
    loop env number msgs = do
        value <- atomically (readTBQueue queue)
        case value of
            AMQPMessage msg ->
                if number >= batchSize
                    then publish env msgs >> loop env 0 []
                    else loop
                             env
                             (number + 1)
                             ((pubsubMessage & pmData .~ Just (toStrict msg)) :
                              msgs)
    publish _ [] = return ()
    publish env msgs =
        catch
            (recovering
                 (exponentialBackoff 15)
                 [ logRetries
                       (\(TransportError _) ->
                             return False)
                       (\b e rs ->
                             print $ defaultLogMsg b e rs)
                 , logRetries
                       (\(SerializeError _) ->
                             return True)
                       (\b e rs ->
                             print $ defaultLogMsg b e rs)
                 , logRetries
                       (\(ServiceError _) ->
                             return False)
                       (\b e rs ->
                             print $ defaultLogMsg b e rs)]
                 (\_ -> do
                      _ <-
                          runResourceT
                              (runGoogle
                                   env
                                   (send
                                        (projectsTopicsPublish
                                             (publishRequest & prMessages .~
                                              msgs)
                                             topicName)))
                      return ()))
            (\(e :: Error) ->
                  return ())


batchSize :: Int
batchSize = 128

topicName :: Text
topicName = "logs"
