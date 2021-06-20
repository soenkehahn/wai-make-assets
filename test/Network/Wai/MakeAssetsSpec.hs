{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.MakeAssetsSpec where

import           Control.Exception
import           Control.Lens
import           Data.ByteString.Lazy (isPrefixOf)
import           Data.List (intercalate)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wreq as Wreq
import           System.Directory
import           System.IO.Silently
import           Test.Hspec
import           Test.Mockery.Directory

import           Network.Wai.MakeAssets

serveDef :: IO Application
serveDef = serveAssets def

spec :: Spec
spec = do
  around_ silence $ do
    describe "serverClient" $ do
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

acceptErrors :: Wreq.Options
acceptErrors = defaults &
  checkResponse .~ Just (\ _ _ -> return ())
