{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Network.Wai.MakeAssets (
  serveAssets,
  serveAssetsEmbedded,
  Options(..),
  def,
) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Default
import           Data.List (intercalate)
import           Data.Monoid
import           Data.String.Conversions
import           Development.Shake (cmd, Exit(..), Stderr(..), CmdOption(..))
import           Language.Haskell.TH
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Application.Static
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO

import           Network.Wai.Application.Static.Missing

data Options
  = Options {
    clientDir :: FilePath
  }

instance Default Options where
  def = Options {
    clientDir = "client"
  }

serveAssets :: Options -> IO Application
serveAssets options = do
  startupChecks options
  let fileApp = staticApp $ defaultFileServerSettings "assets/"
  mvar <- newMVar ()
  return $ \ request respond -> do
    makeResult <- synchronize mvar $ callMake options
    case makeResult of
      Right () -> fileApp request respond
      Left errs -> respond $ responseLBS internalServerError500 [] $
        cs "make error:\n" <> errs

callMake :: Options -> IO (Either LBS ())
callMake options = do
  hPutStrLn stderr $ "running " ++ clientDir options </> "Makefile..."
  (Exit exitCode, Stderr errs) <- cmd (Cwd (clientDir options)) "make"
  return $ case exitCode of
    ExitSuccess -> Right ()
    ExitFailure _ -> Left errs

-- | Returns an 'Exp' of type Application
serveAssetsEmbedded :: Options -> Q Exp
serveAssetsEmbedded options = do
  runIO $ startupChecks options
  makeResult <- runIO $ callMake options
  case makeResult of
    Right () -> serveFilesEmbedded "assets"
    Left err -> runIO $ throwIO $ ErrorCall $ cs err

synchronize :: MVar () -> IO a -> IO a
synchronize mvar action = modifyMVar mvar $ \ () -> ((), ) <$> action

startupChecks :: Options -> IO ()
startupChecks options = do
  checkExists Dir (clientDir options) $
    "You should put sources for assets in there."
  checkExists File (clientDir options </> "Makefile") $ unwords $
    "Which will be invoked to build the assets." :
    "It should put compiled assets into 'assets/'." :
    []
  checkExists Dir "assets" $
    "All files in 'assets/' will be served."

data FileType
  = File
  | Dir

checkExists :: FileType -> FilePath -> String -> IO ()
checkExists typ path hint = do
  exists <- (isFile doesFileExist doesDirectoryExist) path
  when (not exists) $ do
    throwIO $ ErrorCall $ intercalate "\n" $
      ("missing " ++ isFile "file" "directory" ++ ": '" ++ showPath path ++ "'") :
      ("Please create '" ++ showPath path ++ "'.") :
      ("(" ++ hint ++ ")") :
      []
  where
    isFile :: a -> a -> a
    isFile a b = case typ of
      File -> a
      Dir -> b

    showPath :: FilePath -> String
    showPath = case typ of
      File -> id
      Dir -> (++ "/")
