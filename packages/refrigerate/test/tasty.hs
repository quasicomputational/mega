module Main
  ( main
  )
  where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.DList as DList
import qualified Data.Text as ST
import qualified Data.Text.Encoding as STEnc
import qualified Data.Text.IO as STIO
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTEnc
import Distribution.PackageDescription.PrettyPrint
  ( showGenericPackageDescription
  )
import qualified Q4C12.ProjectFile as PF
import Test.Tasty
  ( TestTree
  , defaultMain
  , testGroup
  )
import Test.Tasty.Golden
  ( goldenVsString
  )

import Q4C12.Refrigerate
  ( refrigerate
  )

data Tests = Tests
  { _successTests :: DList FilePath
  , _failureTests :: DList FilePath
  }

instance Semigroup Tests where
  Tests a b <> Tests a' b' = Tests (a <> a') (b <> b')

instance Monoid Tests where
  mempty = Tests mempty mempty
  mappend = (<>)

gatherTests :: IO Tests
gatherTests = do
  dirs <- listDirectory "test/golden"
  fmap fold $ for dirs $ \ dirLocal -> do
    let dir = "test/golden" </> dirLocal
    isSuccess <- doesFileExist ( dir </> "out" )
    isFailure <- doesFileExist ( dir </> "err" )
    pure $ Tests
      ( if isSuccess then DList.singleton dir else mempty )
      ( if isFailure then DList.singleton dir else mempty )

-- Returns the (possibly non-existent) project file name, not the freeze itself (i.e., FILE, not FILE.freeze).
gatherFreezes :: FilePath -> IO [FilePath]
gatherFreezes dir =
  fmap (dir </>) . mapMaybe ( stripExtension "freeze" ) <$> listDirectory dir

main :: IO ()
main = do
  Tests successTests failureTests <- gatherTests
  defaultMain $ testGroup "refrigeration testing"
    [ testGroup "should work" $ successTest <$> toList successTests
    , testGroup "should fail" $ failureTest <$> toList failureTests
    ]

successTest :: FilePath -> TestTree
successTest dir =
  goldenVsString dir ( dir </> "out" ) $ do
    projectFiles <- gatherFreezes dir
    let freezeFiles = flip addExtension "freeze" <$> projectFiles
    configs <- for freezeFiles $ \ freezeFile -> do
      res <- PF.parse <$> STIO.readFile freezeFile
      case res of
        Left err ->
          fail $ ST.unpack $
            "Error while reading " <> ST.pack freezeFile <> ":\n" <> PF.renderError err
        Right config ->
          pure config
    cabalString <- BS.readFile ( dir </> "test.cabal" )
    let res = refrigerate configs cabalString
    case res of
      Left err ->
        fail $ ST.unpack $ "refrigerate failed: " <> err
      Right gpd ->
        pure $ LTEnc.encodeUtf8 $ LT.pack $ showGenericPackageDescription gpd

failureTest :: FilePath -> TestTree
failureTest dir =
  goldenVsString dir ( dir </> "err" ) $ do
    projectFiles <- gatherFreezes dir
    let freezeFiles = flip addExtension "freeze" <$> projectFiles
    configs <- for freezeFiles $ \ freezeFile -> do
      res <- PF.parse <$> STIO.readFile freezeFile
      case res of
        Left err ->
          fail $ ST.unpack $
            "Error while reading " <> ST.pack freezeFile <> ":\n" <> PF.renderError err
        Right config ->
          pure config
    cabalString <- BS.readFile ( dir </> "test.cabal" )
    let res = refrigerate configs cabalString
    case res of
      Left err ->
        pure $ LBS.fromStrict $ STEnc.encodeUtf8 err
      Right _gpd ->
        fail "refrigerate failed to fail."
