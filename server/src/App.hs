{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module App where

import           Control.Concurrent
import           Data.Monoid
import           Data.String.Conversions
import           Development.Shake hiding (action)
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           System.Exit
import           System.IO

run :: IO ()
run = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop
          (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< app

app :: IO Application
app = serveClient

serveClient :: IO Application
serveClient = do
  let fileApp = staticApp $ defaultFileServerSettings "assets/"
  mvar <- newMVar ()
  return $ \ request respond -> do
    (Exit exitCode, Stderr errs) <- synchronize mvar $
      cmd (Cwd "client") "make"
    case exitCode of
      ExitSuccess -> fileApp request respond
      ExitFailure _ -> respond $ responseLBS internalServerError500 [] $
        cs "make error:\n" <> errs

synchronize :: MVar () -> IO a -> IO a
synchronize mvar action = modifyMVar mvar $ \ () -> ((), ) <$> action
