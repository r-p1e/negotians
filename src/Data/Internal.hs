{-# LANGUAGE DeriveGeneric #-}

module Data.Internal where

import Data.ByteString (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)

data FRVTResponse = FRVTResponse
    { body :: Text
    } deriving (Generic)

instance ToJSON FRVTResponse

type Token = ByteString
