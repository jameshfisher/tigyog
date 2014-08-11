module Main where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

import PackageInfo (TigyogInfo(..), tigyogInfo)

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
          "tigyog"
          ] :: Action ()

  fpm "deb"
  fpm "rpm"
