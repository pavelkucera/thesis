module Thesis.Microtransaction where

import Control.DeepSeq (NFData, force, deepseq)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Timeout (runTimeoutT, withTimeoutCatch)
import Control.Timeout (delay)
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.Time.Units (Microsecond, TimeUnit, fromMicroseconds, subTime)

-- | Converts 'POSIXTime' to a specified 'TimeUnit'.
posixToTimeUnit :: TimeUnit t => POSIXTime -> t
posixToTimeUnit s = fromMicroseconds . round $ s * 1000000

-- | Magic constant specifying cleanup time.
cleanupTime :: Microsecond
cleanupTime = 100

-- | Performs and times the given action. The result value of the action is 'force'd
-- to ensure that the thunk evaluation is included in the given time.
time :: (MonadIO m, NFData a, TimeUnit t) => m a -> m (t, a)
time f = do
  startTime <- force <$> liftIO getPOSIXTime
  value <- force <$> f
  endTime <- force <$> liftIO getPOSIXTime
  let elapsedTime = posixToTimeUnit $ endTime - startTime
  return (elapsedTime, value)

-- | Runs given action with the given timeout. Returns given default answer if the
-- action times out.
timeoutMicrotransaction :: (TimeUnit t, NFData a, MonadIO m, MonadMask m)
                        => t                  -- ^ Timeout
                        -> a                  -- ^ Default answer
                        -> m a                -- ^ Microtransaction
                        -> m (Microsecond, a) -- ^ Elapsed time and result
timeoutMicrotransaction baseTime defaultAnswer transaction =
  let targetTime = subTime baseTime cleanupTime :: Microsecond
      action = defaultAnswer `deepseq` withTimeoutCatch transaction
  in time $
    if targetTime >= 1 then do
      answer <- runTimeoutT action targetTime
      return $ fromMaybe defaultAnswer answer
    else
      return defaultAnswer

-- | Runs action for a specifed amount of time. If the action finishes before the
-- time limit, the program awaits until the target time is reached. If the action
-- is to finish after the time limit, it is killed and a default answer is returned.
runMicrotransaction :: (TimeUnit t, NFData a, MonadIO m, MonadMask m)
                    => t   -- ^ Desired target time
                    -> a   -- ^ Default answer
                    -> m a -- ^ Action
                    -> m a -- ^ Result of the action or the default answer
runMicrotransaction targetTime defaultAnswer transaction = do
  (elapsedTime, answer) <- timeoutMicrotransaction targetTime defaultAnswer transaction
  let sleepTime = subTime targetTime elapsedTime :: Microsecond
  delay sleepTime
  return answer
