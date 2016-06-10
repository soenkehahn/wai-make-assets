{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

import           Data.Maybe
import           Network.Wai.Handler.Warp
import           System.IO
import           WithCli

import qualified Network.Wai.MakeAssets as MA
import           Network.Wai.MakeAssets hiding (clientDir)

data Args
  = Args {
    port :: Maybe Int,
    clientDir :: Maybe FilePath
  }
  deriving (Generic)

instance HasArguments Args

main :: IO ()
main = withCliModified [AddShortOption "port" 'p'] $
  \ args -> do
    let appPort = fromMaybe 8000 (port args)
        settings =
          setPort appPort $
          setBeforeMainLoop (hPutStrLn stderr
            ("listening to " ++ show appPort ++ "...")) $
          defaultSettings
        options = maybe def (\ cd -> def{ MA.clientDir = cd }) (clientDir args)
    runSettings settings =<< serveAssets options
