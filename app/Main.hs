{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid ((<>))
import GHC.Generics (Generic)
import Data.Text (Text)
import App (app)
import Data.Default (def)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip (gzip)
import Options.Generic (ParseRecord, getRecord, type(<?>)(..))
import System.IO (stdout)
import Network.Google (newLogger, LogLevel(Debug), newEnv, (!))
import Network.Google.Env (HasEnv(..))
import Network.Google.Logging
       (loggingWriteScope, monitoredResource, mrLabels,
        monitoredResourceLabels, mrType)
import Network.Google.PubSub (pubSubScope)
import Network.Wai.Logger.GoogleLogging (loggerMiddleware)
import Control.Lens ((<&>), (.~), (&))
import Control.Monad.Log
       (defaultBatchingOptions, BatchingOptions(..))
import Control.Monad.Log.Handler
       (withGoogleLoggingHandler, withGooglePubSubHandler)


import qualified Data.HashMap.Strict as HashMap


data Args = Args
    { projectid :: Text <?> "Project ID, used in log name"
    , instanceid :: Text <?> "Instance ID"
    , zone :: Text <?> "zone where instance located"
    , topic :: Text <?> "PubSub topic name"
    } deriving (Generic)

instance ParseRecord Args


main :: IO ()
main = do
    args <-
        getRecord
            "negotians - web service for collection logs from agents and send them to Google PubSub"
    lgr <- newLogger Debug stdout
    env <- newEnv <&> (envScopes .~ ( loggingWriteScope ! pubSubScope)) . (envLogger .~ lgr)
    withGooglePubSubHandler
        (defaultBatchingOptions
         { flushMaxDelay = 3 * 1000000
         , flushMaxQueueSize = 128
         })
        env
        (unHelpful (topic args)) $
        \pubsub ->
             withGoogleLoggingHandler
                 (defaultBatchingOptions
                  { flushMaxDelay = 10 * 1000000
                  , flushMaxQueueSize = 128
                  })
                 env
                 (Just
                      ("projects/" <> (unHelpful (projectid args)) <>
                       ("/logs/negotians")))
                 (Just
                      ((monitoredResource & mrLabels .~
                        (Just
                             (monitoredResourceLabels
                                  (HashMap.fromList
                                       ([ ( "instanceId"
                                          , (unHelpful (instanceid args)))
                                        , ("zone", (unHelpful (zone args)))]))))) &
                       mrType .~
                       (Just "gce_instance")))
                 Nothing $
             \wailogger ->
                  run 3000 $ loggerMiddleware wailogger $ gzip def $
                  app pubsub
