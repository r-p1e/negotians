
module Event where

data Event = Event { crc :: Int, entities :: [Entity]}

data Entity = Entity { time :: UTCTime, msg :: }
