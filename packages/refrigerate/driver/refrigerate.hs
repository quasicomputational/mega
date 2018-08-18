module Main
  ( main
  )
  where

import qualified Data.ByteString as BS
import qualified Data.Text as ST
import qualified Data.Text.IO as STIO
import Distribution.PackageDescription.PrettyPrint
  ( showGenericPackageDescription
  )
import qualified Options.Applicative as OA
import qualified Q4C12.ProjectFile as PF
import qualified System.IO

import Q4C12.Refrigerate
  ( refrigerate
  )

data RefrigerateOptions = RefrigerateOptions
  { _refrigerateOptionsProjectFiles :: [FilePath]
  }

refrigerateOptions :: OA.Parser RefrigerateOptions
refrigerateOptions = RefrigerateOptions
  <$> many
        ( OA.strOption
        $ OA.long "project-file"
       <> OA.short 'f'
       <> OA.metavar "FILE"
       <> OA.help "The name of a project file with an associated freeze file to read constraints from. For example, specifying --project-file=foo.bar will lead to refrigerate looking for foo.bar.freeze."
        )

opts :: OA.ParserInfo RefrigerateOptions
opts = OA.info (OA.helper <*> refrigerateOptions)
   $ OA.fullDesc
  <> OA.progDesc "Add bounds to a Cabal file from several freeze files."

main :: IO ()
main = do
  RefrigerateOptions projectFiles <- OA.execParser opts
  let freezeFiles = flip addExtension "freeze" <$> projectFiles
  configs <- for freezeFiles $ \ freezeFile -> do
    res <- PF.parse <$> STIO.readFile freezeFile
    case res of
      Left err -> do
        STIO.putStrLn $ "Error while reading " <> ST.pack freezeFile <> ":"
        STIO.putStr $ PF.renderError err
        exitFailure
      Right config ->
        pure config
  res <- refrigerate configs <$> BS.getContents
  case res of
    Left err -> do
      STIO.putStr err
      exitFailure
    Right gpd ->
      System.IO.putStr $ showGenericPackageDescription gpd
