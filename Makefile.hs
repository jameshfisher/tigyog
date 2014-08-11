module Main where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

import PackageInfo (version)

main :: IO ()
main = shakeArgs shakeOptions $ do
  want ["dist/build/tigyog/tigyog", "fpm/tigyog.deb", "fpm/tigyog.rpm"]

  "dist/build/tigyog/tigyog" *> \f -> do
    command_ [] "cabal" ["clean"]
    command_ [] "cabal" ["configure"]
    command_ [] "cabal" ["build"]

  "fpm" *> \f -> do
    cmd "mkdir -p" [f]

  "fpm/tigyog" *> \f -> do
    need ["fpm", "dist/build/tigyog/tigyog"]
    cmd "cp dist/build/tigyog/tigyog" [f]

  "fpm/tigyog.deb" *> \f -> do
    need ["fpm/tigyog"]
    v <- liftIO version
    cmd (Cwd "fpm") "fpm" [
      "--name",        "tigyog",
      "--description", "Git project management",
      "--version",     v,
      "--package",     "tigyog.deb",
      "-t",            "deb",
      "-s",            "dir",
      "--depends",     "libssl-dev",
      "--depends",     "libicu-dev",
      "--maintainer",  "jameshfisher@gmail.com",
      "--vendor",      "jameshfisher@gmail.com",
      "--url",         "http://tigyog.org/",
      "--license",     "GPL3",
      "--prefix",      "/opt/tigyog",
      "tigyog"
      ] :: Action ()

  "fpm/tigyog.rpm" *> \f -> do
    need ["fpm/tigyog"]
    v <- liftIO version
    cmd (Cwd "fpm") "fpm" [
      "--name",        "tigyog",
      "--description", "Git project management",
      "--version",     v,
      "--package",     "tigyog.rpm",
      "-t",            "rpm",
      "-s",            "dir",
      "--depends",     "libssl-dev",
      "--depends",     "libicu-dev",
      "--maintainer",  "jameshfisher@gmail.com",
      "--vendor",      "jameshfisher@gmail.com",
      "--url",         "http://tigyog.org/",
      "--license",     "GPL3",
      "--prefix",      "/opt/tigyog",
      "tigyog"
      ] :: Action ()
