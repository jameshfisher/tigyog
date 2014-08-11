module PackageInfo (version) where

import Data.List (intercalate)
import Distribution.Version (versionBranch)
import Distribution.Package (pkgVersion)
import Distribution.PackageDescription (packageDescription, package)
import Distribution.PackageDescription.Parse (parsePackageDescription, ParseResult (ParseOk))

version :: IO String
version = do
  contents <- readFile "tigyog.cabal"
  let
    (ParseOk [] genericDescription) = parsePackageDescription contents
    description = packageDescription genericDescription
    identifier = package description
    version = pkgVersion identifier
    branch = versionBranch version
  return $ intercalate "." (map show branch)
