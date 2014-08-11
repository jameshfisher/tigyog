module PackageInfo (TigyogInfo(..), tigyogInfo) where

import Data.List (intercalate)
import Distribution.Version (versionBranch)
import Distribution.License (License)
import Distribution.Package (pkgName, PackageName(PackageName), pkgVersion)
import Distribution.PackageDescription (PackageDescription(..), packageDescription, package)
import Distribution.PackageDescription.Parse (parsePackageDescription, ParseResult (ParseOk))
import Distribution.Text (display)

data TigyogInfo = TigyogInfo {
  tigyogName :: String,
  tigyogVersion :: String,
  tigyogDescription :: String,
  tigyogHomepage :: String,
  tigyogMaintainer :: String,
  tigyogLicense :: String
}

pkg :: IO PackageDescription
pkg = do
  contents <- readFile "tigyog.cabal"
  let
    (ParseOk [] genericDescription) = parsePackageDescription contents
    p = packageDescription genericDescription
  return p

tigyogInfo :: IO TigyogInfo
tigyogInfo = do
  p <- pkg
  let
    identifier = package p
    PackageName name = pkgName identifier
    version = pkgVersion identifier
    branch = versionBranch version
  return $ TigyogInfo {
    tigyogName        = name,
    tigyogVersion     = intercalate "." (map show branch),
    tigyogDescription = description p,
    tigyogHomepage    = homepage p,
    tigyogMaintainer  = maintainer p,
    tigyogLicense     = display $ license p
  }
