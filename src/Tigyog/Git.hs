module Tigyog.Git (getCommitMessage) where

import Git hiding (CommitOid, Commit)
import Git.Libgit2
import Data.Text
import Control.Monad.Reader
import Control.Monad.Logger
import Data.Tagged

-- (Applicative m, Failure GitException m, MonadBaseControl IO m, MonadIO m, MonadLogger m)
--   => MonadGit LgRepo (ReaderT LgRepo m)
--
-- choose m = NoLoggingT IO
--
-- Applicative (NoLoggingT IO) ?
--   yes:
--     Monad m => Applicative (NoLoggingT m)
--     Monad IO
--     Applicative (NoLoggingT IO)
--
-- Failure GitException (NoLoggingT IO) ?
--   yes:
--     (MonadTrans t, Failure e m, Monad (t m)) => Failure e (t m)
--     choose t = NoLoggingT
--     choose e = GitException
--     choose m = IO
--       MonadTrans NoLoggingT ? yes
--       Failure GitException IO ? yes:
--         Exception e => Failure e IO
--         choose e = GitException
--           Exception GitException ? yes
--       Monad (NoLoggingT IO) ? yes
--         Monad m => Monad (NoLoggingT m)
--         choose m = IO
--
-- MonadBaseControl IO (NoLoggingT IO) ? yes
--   MonadBaseControl b m => MonadBaseControl b (NoLoggingT m)
--   choose b = IO
--   choose m = IO
--   MonadBaseControl IO IO ? yes
--
-- MonadIO (NoLoggingT IO) ? yes
--   MonadIO m => MonadIO (NoLoggingT m)
--   choose m = IO
--   MonadIO IO ? yes
--
-- MonadLogger (NoLoggingT IO) ? yes
--   MonadIO m => MonadLogger (NoLoggingT m)
--   choose m = IO
--   MonadIO IO ? yes
--
-- then we have:
--   MonadGit LgRepo (ReaderT LgRepo (NoLoggingT IO))
-- i.e. in MonadGit r m
--   r = LgRepo
--   m = ReaderT LgRepo (NoLoggingT IO)
--
-- everything works in the (ReaderT LgRepo) monad, e.g.

-- parseOid :: Text -> m (Oid r)
parseOid' :: Text -> ReaderT LgRepo (NoLoggingT IO) (Oid LgRepo)
parseOid' = parseOid

-- So we have to be able to construct an LgRepo somehow.
-- Doesn't look like meant to be constructed directly ("ForeignPtr C'git_repository"?)
--
-- lgFactory :: MonadIO m => RepositoryFactory (ReaderT LgRepo (NoLoggingT m)) m LgRepo
-- choose m = IO
lgFactory' :: RepositoryFactory (ReaderT LgRepo (NoLoggingT IO)) IO LgRepo
lgFactory' = lgFactory

-- Great. What can we do with a RepositoryFactory?
-- data RepositoryFactory n m r
--   openRepository :: RepositoryOptions -> m r
openRepository' :: RepositoryFactory (ReaderT LgRepo (NoLoggingT IO)) IO LgRepo -> RepositoryOptions -> IO LgRepo
openRepository' = openRepository

openRepository'' :: RepositoryOptions -> IO LgRepo
openRepository'' = openRepository' lgFactory'

-- Looks good. Now we need a RepositoryOptions. How about:
-- defaultRepositoryOptions :: RepositoryOptions

repositoryOptions :: RepositoryOptions
repositoryOptions = defaultRepositoryOptions { repoPath = "." }

repository :: IO LgRepo
repository = openRepository'' repositoryOptions

-- So we can get an LgRepo in IO.
-- From there, we can use the Reader monad.

-- remember:
-- parseOid' :: Text -> ReaderT LgRepo (NoLoggingT IO) (Oid LgRepo)
--
-- newtype ReaderT r m a
--   runReaderT :: r -> m a
-- runReaderT :: ReaderT r m a -> r -> m a
-- runReaderT :: ReaderT LgRepo (NoLoggingT IO) (Oid LgRepo) -> LgRepo -> NoLoggingT IO (Oid LgRepo)

parseOid'' :: Text -> NoLoggingT IO (Oid LgRepo)
parseOid'' t = do
  repo <- lift repository
  runReaderT (parseOid' t) repo

-- newtype NoLoggingT m a
--   runNoLoggingT :: m a
-- runNoLoggingT :: NoLoggingT m a -> m a
-- runNoLoggingT :: NoLoggingT IO (Oid LgRepo) -> IO (Oid LgRepo)
parseOid''' :: Text -> IO (Oid LgRepo)
parseOid''' = runNoLoggingT . parseOid''

-- Now what can we do with our Oid thingy?
-- How about:
-- existsObject :: Oid r -> m Bool
existsObject' :: Oid LgRepo -> ReaderT LgRepo (NoLoggingT IO) Bool
existsObject' = existsObject

existsObject'' :: Oid LgRepo -> NoLoggingT IO Bool
existsObject'' o = do
  repo <- lift repository
  runReaderT (existsObject' o) repo

existsObject''' :: Oid LgRepo -> IO Bool
existsObject''' = runNoLoggingT . existsObject''

-- Now tie it together ...
existsOid :: Text -> IO Bool
existsOid t = parseOid''' t >>= existsObject'''

-- lookupCommit :: CommitOid r -> m (Commit r)
lookupCommit' :: CommitOid -> ReaderT LgRepo (NoLoggingT IO) Commit
lookupCommit' = lookupCommit

lookupCommit'' :: CommitOid -> NoLoggingT IO Commit
lookupCommit'' c = do
  repo <- lift repository
  runReaderT (lookupCommit' c) repo

lookupCommit''' :: CommitOid -> IO Commit
lookupCommit''' = runNoLoggingT . lookupCommit''

-- type CommitOid = CommitOid LgRepo
-- type CommitOid r = Tagged (Commit r) (Oid r)
-- Tagged :: (Oid r) -> Tagged (Commit r) (Oid r)
-- Tagged :: Oid LgRepo -> Tagged Commit Oid LgRepo
-- Tagged :: Oid LgRepo -> CommitOid LgRepo
-- Tagged :: Oid LgRepo -> CommitOid
toCommit :: Oid LgRepo -> CommitOid
toCommit = Tagged

-- parseOid''' :: Text -> IO (Oid LgRepo)
parseCommitHash :: Text -> IO CommitOid
parseCommitHash t = fmap toCommit $ parseOid''' t

getCommit :: Text -> IO Commit
getCommit t = parseCommitHash t >>= lookupCommit'''

getCommitMessage :: Text -> IO Text
getCommitMessage t = fmap commitLog $ getCommit t
