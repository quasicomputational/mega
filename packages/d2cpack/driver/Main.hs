module Main
  ( main
  )
  where

import qualified Data.Text.IO as STIO
import DhallToCabal ( dhallToCabal )
import Distribution.PackageDescription ( packageDescription, package )
import Distribution.PackageDescription.PrettyPrint ( writeGenericPackageDescription )
import Distribution.Types.PackageId ( pkgName )
import Distribution.Types.PackageName ( unPackageName )

--TODO: support all bells and whistles of union(hpack, dhall-to-cabal). Definitely missing:
-- - a generated header warning against editing
-- - directories (plural) passed on the command line
-- - file hashing and refusing to over-write if hand-edited, plus a --force option
-- - responding to --help and --version
main :: IO ()
main = do
  pkgDescr <- dhallToCabal "package.dhall" =<< STIO.readFile "package.dhall"
  -- TODO: is this really the simplest way to get the name??
  let name = unPackageName $ pkgName $ package $ packageDescription pkgDescr
      dest = addExtension name "cabal"
  writeGenericPackageDescription dest pkgDescr
