{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.MakeAssetsSpec where

import           Control.Lens
import           Data.ByteString.Lazy (isPrefixOf)
import           Network.Wai.Handler.Warp
import           Network.Wreq
import           System.Directory
import           System.IO.Silently
import           Test.Hspec
import           Test.Mockery.Directory

import           Network.Wai.MakeAssets

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
          testWithApplication serveAssets $ \ port -> do
            let url = "http://localhost:" ++ show port ++ "/foo"
            response <- get url
            response ^. responseBody `shouldBe` "bar"

      it "runs 'make' in 'client/' before answering requests" $ do
        inTempDirectory $ do
          createDirectoryIfMissing True "client"
          createDirectoryIfMissing True "assets"
          writeFile "client/Makefile" "all:\n\techo bar > ../assets/foo"
          testWithApplication serveAssets $ \ port -> do
            let url = "http://localhost:" ++ show port ++ "/foo"
            response <- get url
            response ^. responseBody `shouldBe` "bar\n"

      it "returns the error messages in case 'make' fails" $ do
        inTempDirectory $ do
          createDirectoryIfMissing True "client"
          createDirectoryIfMissing True "assets"
          writeFile "client/Makefile" "all:\n\t>&2 echo error message ; false"
          testWithApplication serveAssets $ \ port -> do
            let url = "http://localhost:" ++ show port ++ "/foo"
            response <- getWith acceptErrors url
            let body = response ^. responseBody
            body `shouldSatisfy` ("make error:\nerror message\n" `isPrefixOf`)

acceptErrors :: Options
acceptErrors = defaults &
  checkStatus .~ Just (\ _ _ _ -> Nothing)
