module Q4C12.Refreeze
  ( RefreezeConfig
  , defaultRefreezeConfig
  , projects
  , refreezeSchema
  , main
  )
  where

import qualified Data.Text.IO as STIO
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.IO as LTIO
import Q4C12.XML
  ( displayError
  , displayWarnings
  , parseXML'
  , uname
  )
import Q4C12.XMLDesc
  ( Desc
  , El
  , consolidate
  , elementMixed
  , flowEvenPreWSDropComments
  , flowWSEDropComments
  , oddTxNoPos
  , rfmap
  , rmany
  )
import qualified Q4C12.XMLDesc.Parse as XMLDesc.Parse
import System.Process
  ( callProcess
  )

data RefreezeConfig = RefreezeConfig
  { _refreezeProjects :: [ FilePath ]
  }

defaultRefreezeConfig :: RefreezeConfig
defaultRefreezeConfig = RefreezeConfig []

projects :: Lens' RefreezeConfig [ FilePath ]
projects f ( RefreezeConfig ps ) = RefreezeConfig <$> f ps

refreezeSchema
  :: (Desc tag)
  => El tag RefreezeConfig
refreezeSchema
  = rfmap (iso f g) $ elementMixed ( uname "refreeze" ) $ flowEvenPreWSDropComments
  $ rmany
  $ flowWSEDropComments
  $ rfmap ( iso LT.unpack LT.pack )
  $ elementMixed ( uname "build" )
  $ rfmap consolidate
  $ oddTxNoPos
  where
  f = RefreezeConfig
  g ( RefreezeConfig ps ) = ps

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
refreeze ( RefreezeConfig ps ) = for_ ps $ \ projectFile -> do
  -- TODO: this is a hack working around cabal-install 2.2's bug in how it doesn't properly rebuild when the project file is changed, which is going to be fixed in 2.4 (at which point we'd also have new-clean, but that's moot!)
  removeDirectoryRecursive "dist-newstyle"
  callProcess "cabal"
    [ "new-update", "--project-file", projectFile ]
  callProcess "cabal"
    [ "new-freeze", "--project-file", projectFile ]
