{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Caldron.Job
  ( JobId
  , JobStatus (..)
  , JobQueue
  , newJobQueue
  , submitJob
  , getJobStatus
  , startWorker
  ) where

import Protolude

import Control.Concurrent.STM      (TBQueue, TVar, modifyTVar',
                                    newTBQueueIO, newTVarIO, readTBQueue,
                                    readTVarIO, writeTBQueue)
import qualified Data.Map.Strict   as Map
import Data.UUID                   (toText)
import Data.UUID.V4                (nextRandom)
import Text.URI                    (URI)

type JobId = Text

data JobStatus
  = Pending
  | Processing
  | Complete URI
  | Failed Text

data JobQueue = JobQueue
  { jqQueue    :: TBQueue (JobId, IO (Either Text URI))
  , jqStatuses :: TVar (Map JobId JobStatus)
  }

newJobQueue :: IO JobQueue
newJobQueue = JobQueue <$> newTBQueueIO 100 <*> newTVarIO Map.empty

submitJob :: JobQueue -> IO (Either Text URI) -> IO JobId
submitJob jq action = do
  jobId <- toText <$> nextRandom
  atomically $ do
    modifyTVar' (jqStatuses jq) (Map.insert jobId Pending)
    writeTBQueue (jqQueue jq) (jobId, action)
  pure jobId

getJobStatus :: JobQueue -> JobId -> IO (Maybe JobStatus)
getJobStatus jq jobId = Map.lookup jobId <$> readTVarIO (jqStatuses jq)

-- | Start a background worker thread that processes jobs from the queue.
-- Exceptions thrown by a job action are caught and recorded as failures.
startWorker :: JobQueue -> IO ()
startWorker jq = void $ forkIO $ forever $ do
  (jobId, action) <- atomically $ readTBQueue (jqQueue jq)
  atomically $ modifyTVar' (jqStatuses jq) (Map.insert jobId Processing)
  result <- try action :: IO (Either SomeException (Either Text URI))
  let newStatus = case result of
        Left ex          -> Failed (show ex)
        Right (Left err) -> Failed err
        Right (Right uri) -> Complete uri
  atomically $ modifyTVar' (jqStatuses jq) (Map.insert jobId newStatus)
