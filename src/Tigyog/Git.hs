{-# LANGUAGE OverloadedStrings #-}
module Tigyog.Git (getCommitMessage, getBlobContents, getMasterFileContents) where

import Git hiding (CommitOid, Commit, BlobOid, Tree, TreeOid, TreeEntry, RefTarget)
import Git.Libgit2
import Git.Libgit2.Types
import Data.Text
import Data.Text.Encoding (encodeUtf8)
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

lookupTree' :: TreeOid -> IO Tree
lookupTree' = runStack . lookupTree

treeEntry' :: Tree -> TreeFilePath -> IO (Maybe TreeEntry)
treeEntry' t tfp = runStack $ treeEntry t tfp

-- path looks like "foo/bar"; no leading slash
commitAndPathToBlobOid :: CommitOid -> Text -> IO (Maybe BlobOid)
commitAndPathToBlobOid oid path = do
  commit <- lookupCommit' oid
  let tOid = commitTree commit
  tree <- lookupTree' tOid
  te <- treeEntry' tree (encodeUtf8 path)
  case te of
    Just (BlobEntry bOid _) -> return $ Just bOid
    Just _                  -> return Nothing
    Nothing                 -> return Nothing

getFileContents :: CommitOid -> Text -> IO (Maybe Text)
getFileContents oid path = do
  bOid <- commitAndPathToBlobOid oid path
  case bOid of
    Just b -> do
      t <- catBlobUtf8' b
      return $ Just t
    Nothing -> return Nothing

-- MonadGit LgRepo (ReaderT LgRepo (NoLogging IO))
-- lookupReference :: RefName -> m (Maybe (RefTarget r))
lookupReference' :: Text -> IO (Maybe RefTarget)
lookupReference' = runStack . lookupReference

lookupReference'' :: Text -> IO (Maybe (Oid LgRepo))
lookupReference'' t = do
  r <- lookupReference' t
  case r of
    Just (RefObj oid) -> return $ Just oid
    Just (RefSymbolic rn) -> do
      print rn
      return Nothing
    Nothing -> return Nothing

getMasterFileContents :: Text -> IO (Maybe Text)
getMasterFileContents path = do
  mOid <- lookupReference'' "refs/heads/master"
  case mOid of
    Nothing -> return Nothing
    Just oid -> getFileContents (toCommit oid) path
