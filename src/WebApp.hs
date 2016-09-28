{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module WebApp
    ( WebAppT
    , runWebAppT
    , WebAppErr(..)
    ) where

import           Control.Exception      (Exception)
import           Control.Monad.Except   (ExceptT, MonadError, runExceptT)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, ReaderT (..), runReaderT)
import           Control.Monad.Trans    (MonadTrans (..))
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Lazy   as BSL
import           Katip                  (Katip (..), KatipContext (..),
                                         KatipContextT, LogEnv, LogItem,
                                         Namespace, runKatipContextT)
import           Network.HTTP.Types     (RequestHeaders)

newtype WebAppT e r m a = WebAppT
    { unWebAppT :: ReaderT r (ExceptT e (KatipContextT m)) a
    } deriving (Functor,Applicative,Monad,MonadReader r,MonadIO, MonadError e)

instance MonadTrans (WebAppT e r) where
 lift = WebAppT . lift . lift . lift

instance Katip m => Katip (WebAppT e r m) where
  getLogEnv = lift getLogEnv

instance (Katip m, KatipContext m) => KatipContext (WebAppT e r m) where
    getKatipContext = lift getKatipContext
    getKatipNamespace = lift getKatipNamespace


runWebAppT
    :: (LogItem c, Katip m, KatipContext m, MonadIO m)
    => r -> LogEnv -> c -> Namespace -> WebAppT e r m a -> m (Either e a)
runWebAppT cfg logenv ctx namesapace app =
    runKatipContextT
        logenv
        ctx
        namesapace
        (runExceptT (runReaderT (unWebAppT app) cfg))


-- | When used WAI errors data type
data WebAppErr = WebAppErr
    { errHTTPCode     :: Int
    , errReasonPhrase :: ByteString
    , errBody         :: BSL.ByteString
    , errHeaders      :: RequestHeaders
    } deriving (Show)

instance Exception WebAppErr
