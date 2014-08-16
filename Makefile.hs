module Main where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

import PackageInfo (TigyogInfo(..), tigyogInfo)

main :: IO ()
main = shakeArgs shakeOptions $ do
  want ["fpm/tigyog.deb", "fpm/tigyog.rpm"]

  "server/dist/build/tigyog/tigyog" *> \f -> do
    command_ [Cwd "server"] "cabal" ["clean"]
    command_ [Cwd "server"] "cabal" ["configure"]
    command_ [Cwd "server"] "cabal" ["build"]

  "fpm" *> \f -> do
    cmd "mkdir -p" [f]

  "fpm/tigyog" *> \f -> do
    need ["fpm", "server/dist/build/tigyog/tigyog"]
    cmd "cp server/dist/build/tigyog/tigyog" [f]

  let
    fpm pkgType = do
      ("fpm/tigyog." ++ pkgType) *> \f -> do
        need ["fpm/tigyog"]
        info <- liftIO tigyogInfo
        cmd (Cwd "fpm") "fpm" [
          "--name",        tigyogName info,
          "--description", tigyogDescription info,
          "--version",     tigyogVersion info,
          "--maintainer",  tigyogMaintainer info,
          "--vendor",      tigyogMaintainer info,
          "--url",         tigyogHomepage info,
          "--license",     tigyogLicense info,
          "--package",     "tigyog." ++ pkgType,
          "-t",            pkgType,
          "-s",            "dir",
          "--depends",     "libssl-dev",
          "--depends",     "libicu-dev",
          "--prefix",      "/opt/tigyog",
          "--force",
          "tigyog"
          ] :: Action ()

  fpm "deb"
  fpm "rpm"
