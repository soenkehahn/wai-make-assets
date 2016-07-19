{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Wai.MakeAssetsSpec where

import           Control.Exception
import           Control.Lens
import           Data.ByteString.Lazy (isPrefixOf)
import           Data.List (intercalate)
import           Language.Haskell.TH
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wreq as Wreq
import           System.Directory
import           System.IO
import           System.IO.Silently
import           Test.Hspec hiding (runIO)
import           Test.Mockery.Directory

import           Network.Wai.Application.Static.Missing
import           Network.Wai.MakeAssets
import           Utils

serveDef :: IO Application
serveDef = serveAssets def

spec :: Spec
spec = do
  around_ silence $ do
    describe "serveAssets" $ do
      it "returns static files" $ do
        inTempDirectory $ do
          createDirectoryIfMissing True "client"
          createDirectoryIfMissing True "assets"
          writeFile "assets/foo" "bar"
          writeFile "client/Makefile" "all:\n\ttrue"
          testWithApplication serveDef $ \ port -> do
            let url = "http://localhost:" ++ show port ++ "/foo"
            response <- get url
            response ^. responseBody `shouldBe` "bar"

      it "runs 'make' in 'client/' before answering requests" $ do
        inTempDirectory $ do
          createDirectoryIfMissing True "client"
          createDirectoryIfMissing True "assets"
          writeFile "client/Makefile" "all:\n\techo bar > ../assets/foo"
          testWithApplication serveDef $ \ port -> do
            let url = "http://localhost:" ++ show port ++ "/foo"
            response <- get url
            response ^. responseBody `shouldBe` "bar\n"

      it "allows to configure the name of the 'client/' directory" $ do
        inTempDirectory $ do
          createDirectoryIfMissing True "custom"
          createDirectoryIfMissing True "assets"
          writeFile "custom/Makefile" "all:\n\techo bar > ../assets/foo"
          let options = def{ clientDir = "custom" }
          testWithApplication (serveAssets options) $ \ port -> do
            let url = "http://localhost:" ++ show port ++ "/foo"
            response <- get url
            response ^. responseBody `shouldBe` "bar\n"

      it "returns the error messages in case 'make' fails" $ do
        inTempDirectory $ do
          createDirectoryIfMissing True "client"
          createDirectoryIfMissing True "assets"
          writeFile "client/Makefile" "all:\n\t>&2 echo error message ; false"
          testWithApplication serveDef $ \ port -> do
            let url = "http://localhost:" ++ show port ++ "/foo"
            response <- getWith acceptErrors url
            let body = response ^. responseBody
            body `shouldSatisfy` ("make error:\nerror message\n" `isPrefixOf`)

      context "complains about missing files or directories" $ do
        it "missing client/" $ do
          inTempDirectory $ do
            createDirectoryIfMissing True "assets"
            let expected = intercalate "\n" $
                  "missing directory: 'client/'" :
                  "Please create 'client/'." :
                  "(You should put sources for assets in there.)" :
                  []
            testWithApplication serveDef (\ _ -> return ())
              `shouldThrow` errorCall expected

        it "missing client/Makefile" $ do
          inTempDirectory $ do
            createDirectoryIfMissing True "client"
            createDirectoryIfMissing True "assets"
            let expected = intercalate "\n" $
                  "missing file: 'client/Makefile'" :
                  "Please create 'client/Makefile'." :
                  "(Which will be invoked to build the assets. It should put compiled assets into 'assets/'.)" :
                  []
            testWithApplication serveDef (\ _ -> return ())
              `shouldThrow` errorCall expected

        it "missing assets/" $ do
          inTempDirectory $ do
            touch "client/Makefile"
            let expected = intercalate "\n" $
                  "missing directory: 'assets/'" :
                  "Please create 'assets/'." :
                  "(All files in 'assets/' will be served.)" :
                  []
            catch (testWithApplication serveDef (\ _ -> return ())) $
              \ (ErrorCall message) -> do
                message `shouldBe` expected

    describe "serveAssetsEmbedded" $ do
      it "allows to serve files when being run from a different directory" $ do
        let app = $(runIO $ inTempDirectory $ do
              touch "client/Makefile"
              writeFile "client/Makefile" "all:\n\techo fooBody > ../assets/foo"
              touch "assets/.keep"
              testInIO $ serveAssetsEmbedded $ def)
        inTempDirectory $ do
          testWithApplication app $ \ port -> do
            let url = "http://localhost:" ++ show port ++ "/foo"
            response <- get url
            response ^. responseBody `shouldBe` "fooBody\n"

      it "outputs a message that it runs the Makefile" $ do
        inTempDirectory $ do
          touch "client/Makefile"
          writeFile "client/Makefile" "all:\n\techo fooBody > ../assets/foo"
          touch "assets/.keep"
          output <- hCapture_ [stderr] $ testInIO $ serveAssetsEmbedded $ def
          output `shouldContain` "running client/Makefile..."

    describe "serveFilesEmbedded" $ do
      let testFile :: Application -> String -> IO ()
          testFile app path = do
            testWithApplication (return app) $ \ port -> do
              let url = "http://localhost:" ++ show port ++ path
              response <- get url
              response ^. responseBody `shouldBe` "fileContent"

      it "serves files from a directory" $ do
        let app :: Application
            app = $(runIO $ inTempDirectory $ do
              touch "files/foo"
              writeFile "files/foo" "fileContent"
              testInIO $ serveFilesEmbedded "files")
        testFile app "/foo"

      it "works if given directory with trailing slash" $ do
        let app :: Application
            app = $(do
              runIO $ inTempDirectory $ do
                touch "files/foo"
                writeFile "files/foo" "fileContent"
                testInIO $ serveFilesEmbedded "files/")
        testFile app "/foo"

      it "works for nested directories" $ do
        let app :: Application
            app = $(do
              runIO $ inTempDirectory $ do
                touch "files/foo/bar"
                writeFile "files/foo/bar" "fileContent"
                testInIO $ serveFilesEmbedded "files")
        testFile app "/foo/bar"

      it "throws a nicer error when the directory doesn't exist" $ do
        inTempDirectory $ do
          runQ (serveFilesEmbedded "foo") `shouldThrow`
            errorCall "directory not found: foo"

      it "guesses the mime type" $ do
        let app :: Application
            app = $(runIO $ inTempDirectory $ do
              touch "files/foo.html"
              testInIO $ serveFilesEmbedded "files")
        testWithApplication (return app) $ \ port -> do
          let url = "http://localhost:" ++ show port ++ "/foo.html"
          response <- get url
          response ^. responseHeader "content-type" `shouldBe`
            "text/html"

      it "serves index.html on /" $ do
        let app :: Application
            app = $(do
              runIO $ inTempDirectory $ do
                touch "files/index.html"
                writeFile "files/index.html" "fileContent"
                testInIO $ serveFilesEmbedded "files")
        testFile app "/"

      it "adds client/* to dependent files" $ do
        inTempDirectory $ do
          touch "files/foo"
          touch "files/bar"
          dependentFiles <- collectDependentFiles $ do
            _ <- serveFilesEmbedded "files"
            return ()
          dependentFiles `shouldBe` ["files/foo", "files/bar"]

acceptErrors :: Wreq.Options
acceptErrors = defaults &
  checkStatus .~ Just (\ _ _ _ -> Nothing)
