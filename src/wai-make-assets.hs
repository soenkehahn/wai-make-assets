{-# LANGUAGE DeriveGeneric #-}

import           Network.Wai.Handler.Warp
import           System.IO
import           WithCli

import           Network.Wai.MakeAssets

data Args
  = Args {
    port :: Int
  }
  deriving (Generic)

instance HasArguments Args

main :: IO ()
main = withCliModified [AddShortOption "port" 'p'] $ \ (Args port) -> do
  let settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr
          ("listening to " ++ show port ++ "...")) $
        defaultSettings
  runSettings settings =<< serveAssets
