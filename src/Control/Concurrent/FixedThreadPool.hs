{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Concurrent.FixedThreadPool
  ( ThreadPool
  , Action
  , newThreadPool
  , stopThreadPool
  , killThreadPool
  , waitThreadPool
  , withThreadPool
  , runAction
  , runActionGroup
  , mapAction
  , mapAction_
  , waitAction
  ) where

------------------------------------------------------------------------------
import Control.Concurrent (ThreadId, forkIOWithUnmask, killThread)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMChan (TBMChan)
import qualified Control.Concurrent.STM.TBMChan as TBMChan
import Control.Exception
import Control.Monad (forever, void, when)
import Data.IORef
import Data.Maybe (isJust)
import Data.Vector (Vector)
import qualified Data.Vector as V
------------------------------------------------------------------------------

data Action = Action {
    _action :: IO ()
  , _actionDone :: Maybe SomeException -> IO ()
  , _wait :: IO (Maybe SomeException)
  }

type Chan = TBMChan Action

data ThreadPool = ThreadPool {
    _threads :: {-# UNPACK #-} !(Vector ThreadId)
  , _joins :: {-# UNPACK #-} !(Vector (IO ()))
  , _chan :: {-# UNPACK #-} !Chan
  }


------------------------------------------------------------------------------
newThreadPool :: Int -> IO ThreadPool
newThreadPool n = do
    chan <- atomically $ TBMChan.newTBMChan n
    mvars <- V.replicateM n newEmptyMVar
    threads <- V.mapM (\m -> forkIOWithUnmask $ \u -> worker chan m u) mvars
    let joins = V.map takeMVar mvars
    return $! ThreadPool threads joins chan

  where
    eatExceptions = handle $ \(e :: SomeException) -> void $ evaluate e
    performAction (Action action done _) =
        (action >> done Nothing) `catch` (eatExceptions . done . Just)
    worker chan mv restore = (`finally` putMVar mv ()) $ eatExceptions $
                             restore $ forever $ do
        m <- atomically $ TBMChan.readTBMChan chan
        case m of
          Nothing -> throwIO ThreadKilled
          (Just act) -> performAction act


-- | Runs an IO action asynchronously. Returns an 'Action' you can wait on with
-- 'waitAction'.
runAction :: ThreadPool -> IO () -> IO Action
runAction (ThreadPool _ _ chan) action = do
    mv <- newEmptyMVar
    let actData = Action action (putMVar mv) (readMVar mv)
    atomically $ TBMChan.writeTBMChan chan actData
    return actData


waitAction :: Action -> IO (Maybe SomeException)
waitAction (Action _ _ wait) = wait


-- | Run a group of actions and wait for them all to finish
data Group = Group {
    _sema :: MVar Int
  , _groupErr :: IORef (Maybe SomeException)
  , _groupDone :: MVar (Maybe SomeException)
  }


newGroup :: Int -> IO Group
newGroup k = Group <$> newMVar k <*> newIORef Nothing <*> newEmptyMVar


doneGroup :: Group -> Maybe SomeException -> IO ()
doneGroup (Group sema gerr done) mberr = mask_ $ do
    b <- modifyMVar sema dec
    when (isJust mberr) handleErr
    when b $ void (readIORef gerr >>= tryPutMVar done)
  where
    handleErr = writeIORef gerr mberr
    dec !k = let !k' = k - 1
             in return (k', k' == 0)


waitGroup :: Group -> IO (Maybe SomeException)
waitGroup (Group _ _ done) = readMVar done


-- | Runs a group of IO actions and waits for them to finish. If any of the IO
-- actions throws an exception, 'runActionGroup' will return it. If more than
-- one action throws an exception, 'runActionGroup' will arbitrarily return one
-- of the exceptions. The 'runActionGroup' function will wait until all of the
-- actions have run, even if an exception is thrown.
runActionGroup :: Traversable t
               => ThreadPool
               -> t (IO ())
               -> IO (Maybe SomeException)
runActionGroup (ThreadPool _ _ chan) actions =
    if len == 0 then return Nothing else go
  where
    len = length actions

    go = do
        g <- newGroup len
        let actData = fmap (toAction g) actions
        mapM_ (atomically . TBMChan.writeTBMChan chan) actData
        waitGroup g

    toAction g a = Action a (doneGroup g) (waitGroup g)


stopThreadPool :: ThreadPool -> IO ()
stopThreadPool (ThreadPool _ _ chan) =
    atomically $ TBMChan.closeTBMChan chan


killThreadPool :: ThreadPool -> IO ()
killThreadPool tp@(ThreadPool threads _ _) = do
    stopThreadPool tp
    V.mapM_ killThread threads


waitThreadPool :: ThreadPool -> IO ()
waitThreadPool (ThreadPool _ waits _) = V.sequence_ waits


withThreadPool :: Int -> (ThreadPool -> IO a) -> IO a
withThreadPool k userFunc = bracket create destroy userFunc
  where
    create = newThreadPool k
    destroy tp = stopThreadPool tp >> waitThreadPool tp


mapAction :: Traversable t
          => ThreadPool
          -> (a -> IO b)
          -> t a
          -> IO (t (Either SomeException b))
mapAction tp userFunc xs = do
    vals <- mapM zipMV xs
    let mvs = fmap snd vals
    let actions = fmap toAction vals
    mapM_ (runAction tp) actions
    mapM takeMVar mvs

  where
    zipMV x = do
        mv <- newEmptyMVar
        return (x, mv)
    toAction (x, mv) = try (userFunc x) >>= putMVar mv


mapAction_ :: Traversable t
           => ThreadPool
           -> (a -> IO b)
           -> t a
           -> IO ()
mapAction_ tp userFunc xs0 = do
    e <- sequence_ <$> mapAction tp (void . userFunc) xs0
    either throwIO return e
