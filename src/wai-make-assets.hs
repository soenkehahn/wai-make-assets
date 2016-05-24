
import           Network.Wai.Handler.Warp
import           System.IO

import           Network.Wai.MakeAssets

main :: IO ()
main = do
  let port = 8000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr
          ("listening to " ++ show port ++ "...")) $
        defaultSettings
  runSettings settings =<< serveAssets
