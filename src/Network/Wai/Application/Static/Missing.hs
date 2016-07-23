{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Wai.Application.Static.Missing (
  serveFilesEmbedded,
) where

import           Control.Arrow
import           Control.Exception
import           Control.Monad
import           Crypto.Hash.MD5 (hashlazy)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable
import           Data.String.Conversions
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Network.Mime
import           Network.Wai.Application.Static
import           System.Directory
import           System.Directory.Tree
import           System.FilePath
import           WaiAppStatic.Storage.Embedded
import           WaiAppStatic.Types

serveFilesEmbedded :: FilePath -> Q Exp
serveFilesEmbedded directory = do
  runIO $ checkExists directory
  withAnchor <- runIO $ zipPaths <$> readDirectoryWith LBS.readFile directory
  forM_ (fmap fst withAnchor) addDependentFile
  let tree = fmap (first (makeRelative directory)) withAnchor
  [| let settingsEmbedded = $(mkSettings (return $ fmap (uncurry toEmbeddable) $ toList tree))
         settingsIndices = settingsEmbedded{
           ssIndices = [unsafeToPiece $ T.pack "index.html"],
           ssAddTrailingSlash = True
         }
     in staticApp settingsIndices |]

hash :: LBS.ByteString -> Text
hash = T.take 8 . T.decodeUtf8 . B64.encode .hashlazy

toEmbeddable :: String -> LBS.ByteString -> EmbeddableEntry
toEmbeddable location contents = EmbeddableEntry{
  eLocation = cs location,
  eContent = Left (hash contents, contents),
  eMimeType = defaultMimeLookup (cs location)
}

checkExists :: FilePath -> IO ()
checkExists dir = do
  exists <- doesDirectoryExist dir
  when (not exists) $
    throwIO $ ErrorCall $ "directory not found: " ++ dir
