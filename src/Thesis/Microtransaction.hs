module Thesis.Microtransaction where

import Control.DeepSeq (NFData, force)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Timeout (delay, timeout)
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.Time.Units (Microsecond, TimeUnit, fromMicroseconds, subTime)

posixToTimeUnit :: TimeUnit t => POSIXTime -> t
posixToTimeUnit s = fromMicroseconds . round $ s * 1000000

cleanupTime :: Microsecond
cleanupTime = 100

time :: (MonadIO m, NFData a, TimeUnit t) => m a -> m (t, a)
time f = do
  startTime <- force <$> liftIO getPOSIXTime
  value <- force <$> f
  endTime <- force <$> liftIO getPOSIXTime
  let elapsedTime = posixToTimeUnit $ endTime - startTime
  return (elapsedTime, value)

timeoutMicrotransaction :: (TimeUnit t, NFData a, MonadIO m, MonadCatch m)
                        => t -> a -> m a -> m (Microsecond, a)
timeoutMicrotransaction baseTime defaultAnswer transaction =
  let targetTime = subTime baseTime cleanupTime :: Microsecond
  in time $ do
    answer <- timeout targetTime transaction
    return $ fromMaybe defaultAnswer answer

runMicrotransaction :: (TimeUnit t, NFData a, MonadIO m, MonadCatch m)
                    => t -> a -> m a -> m a
runMicrotransaction targetTime defaultAnswer transaction = do
  (elapsedTime, answer) <- timeoutMicrotransaction targetTime defaultAnswer transaction
  let sleepTime = subTime targetTime elapsedTime :: Microsecond
  delay sleepTime
  return answer
