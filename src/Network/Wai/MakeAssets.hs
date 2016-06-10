{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Network.Wai.MakeAssets (serveAssets, Options(..), def) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Default
import           Data.List (intercalate)
import           Data.Monoid
import           Data.String.Conversions
import           Development.Shake (cmd, Exit(..), Stderr(..), CmdOption(..))
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Application.Static
import           System.Directory
import           System.Exit
import           System.FilePath

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
    (Exit exitCode, Stderr errs) <- synchronize mvar $
      cmd (Cwd (clientDir options)) "make"
    case exitCode of
      ExitSuccess -> fileApp request respond
      ExitFailure _ -> respond $ responseLBS internalServerError500 [] $
        cs "make error:\n" <> errs

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
