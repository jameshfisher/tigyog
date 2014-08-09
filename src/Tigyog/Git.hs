module Tigyog.Git (getCommitMessage, getBlobContents) where

import Git hiding (CommitOid, Commit, BlobOid)
import Git.Libgit2
import Data.Text
import Control.Monad.Reader
import Control.Monad.Logger
import Data.Tagged

openRepository'' :: RepositoryOptions -> IO LgRepo
openRepository'' = openRepository lgFactory

repositoryOptions :: RepositoryOptions
repositoryOptions = defaultRepositoryOptions { repoPath = "." }

repository :: IO LgRepo
repository = openRepository'' repositoryOptions

runStack :: ReaderT LgRepo (NoLoggingT IO) t -> IO t
runStack s = runNoLoggingT $ do
  repo <- lift repository
  runReaderT s repo

parseOid' :: Text -> IO (Oid LgRepo)
parseOid' = runStack . parseOid

lookupCommit' :: CommitOid -> IO Commit
lookupCommit' = runStack . lookupCommit

toCommit :: Oid LgRepo -> CommitOid
toCommit = Tagged

toBlobOid :: Oid LgRepo -> BlobOid
toBlobOid = Tagged

parseCommitHash :: Text -> IO CommitOid
parseCommitHash t = fmap toCommit $ parseOid' t

getCommit :: Text -> IO Commit
getCommit t = parseCommitHash t >>= lookupCommit'

getCommitMessage :: Text -> IO Text
getCommitMessage t = fmap commitLog $ getCommit t

-- lookupBlob :: BlobOid r -> m (Blob r m)
-- lookupBlob :: BlobOid r -> m (Blob r m)

-- catBlobUtf8 :: MonadGit r m => BlobOid r -> m Text
-- catBlobUtf8' :: BlobOid LgRepo -> (ReaderT LgRepo NoLoggingT IO) Text
catBlobUtf8' :: BlobOid -> IO Text
catBlobUtf8' = runStack . catBlobUtf8

getBlobContents :: Text -> IO Text
getBlobContents h = do
  oid <- parseOid' h
  catBlobUtf8' $ toBlobOid oid
