
module Output
    ( initOutput
    ) where

import           Internal       (Msg, Source, Token)
import           Output.AMQP    (initAMQP)
import           Proto.EventLog (Severity)


initOutput
    :: IO (Token -> Double -> Severity -> Source -> Msg -> IO ())
initOutput = initAMQP
