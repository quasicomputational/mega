module Main
  ( main
  )
  where

import Control.Exception
  ( tryJust
  )
import qualified Data.Text as ST
import qualified Data.Text.IO as STIO
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.IO as LTIO
import qualified Hpack
import qualified Q4C12.ProjectFile as PF
import Q4C12.XML
  ( displayError
  , displayWarnings
  , parseXML'
  )
import qualified Q4C12.XMLDesc.Parse as XMLDesc.Parse
import System.Process
  ( callProcess
  )

import Q4C12.Refreeze
  ( RefreezeConfig
  , projects
  , refreezeSchema
  )

main :: IO ()
main = do
  (parseRes, warns) <- parseXML' <$> STIO.readFile "refreeze.xml"
  LTIO.putStr $ LTB.toLazyText $ displayWarnings warns
  case parseRes of
    Left err -> do
      LTIO.putStrLn $ LTB.toLazyText $ displayError err
      exitFailure
    Right el ->
      case XMLDesc.Parse.parse refreezeSchema el of
        Nothing -> do
          STIO.putStrLn "Extraction failed."
          exitFailure
        Just refreezeConfig ->
          refreeze refreezeConfig

refreeze :: RefreezeConfig -> IO ()
refreeze config = for_ ( view projects config ) $ \ projectFile -> do
  projectConfig <- do
    res <- PF.parse <$> STIO.readFile projectFile
    case res of
      Left err -> do
        STIO.putStrLn $ "While processing " <> ST.pack projectFile <> ":"
        STIO.putStr $ PF.renderError err
        exitFailure
      Right projectConfig ->
        pure projectConfig

  -- Go over the packages and run hpack on them if needed.
  -- TODO: do this once per package, even if the package is in multiple projects
  for_ ( view PF.packages projectConfig ) $ \ package -> do
    -- TODO: no glob expansion
    let
      packageYaml = ST.unpack package </> "package.yaml"
    packageYamlExists <- doesFileExist packageYaml
    when packageYamlExists $ do
      Hpack.hpack Hpack.Verbose $
        Hpack.setTarget packageYaml Hpack.defaultOptions

  -- TODO: this is a hack working around cabal-install 2.2's bug in how it doesn't properly rebuild when the project file is changed, which is going to be fixed in 2.4 (at which point we'd also have new-clean, but that's moot!)
  removeDirectoryRecursive "dist-newstyle"
  void $ tryJust
    ( guard . isDoesNotExistError )
    ( removeFile ( addExtension projectFile "freeze" ) )
  callProcess "cabal"
    [ "new-update", "--project-file", projectFile ]
  callProcess "cabal"
    [ "new-freeze", "--project-file", projectFile ]
