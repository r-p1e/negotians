
module Output
    ( initOutput
    ) where

import Data.Text (Text)
import           Internal       (Msg, Source, Token)
import           Output.AMQP    (initAMQP)
import           Proto.EventLog (Severity)


initOutput
    :: Text -> Text -> IO (Token -> Double -> Severity -> Source -> Msg -> IO ())
initOutput = initAMQP
