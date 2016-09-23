{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Internal where

import           Data.Aeson                (ToJSON, encode)
import           Data.CaseInsensitive      (original)
import           Data.Monoid               ((<>))
import           Data.String.Conv          (StringConv (..))
import           Data.Text                 (Text)
import           GHC.Generics              (Generic)
import           Network.HTTP.Types.Header (RequestHeaders)

import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text


data LogMsg = LogMsg
    { who           :: String
    , circumstances :: Text
    , what          :: Text
    } deriving (Generic)

instance ToJSON LogMsg

instance StringConv LogMsg Text where
         strConv l = strConv l . encode

describeHeader :: RequestHeaders -> Text
describeHeader =
    Text.intercalate "; " .
    map
        (\(a,b) ->
              Text.decodeUtf8 (original a) <> ":" <> Text.decodeUtf8 b)
