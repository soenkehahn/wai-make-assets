{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Network.Wai.MakeAssets (serveAssets) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.List (intercalate)
import           Data.Monoid
import           Data.String.Conversions
import           Development.Shake (cmd, Exit(..), Stderr(..), CmdOption(..))
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Application.Static
import           System.Directory
import           System.Exit

serveAssets :: IO Application
serveAssets = do
  startupChecks
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

startupChecks :: IO ()
startupChecks = do
  checkExists Dir "client/" $
    "You should put sources for assets in there."
  checkExists File "client/Makefile" $ unwords $
    "Which will be invoked to build the assets." :
    "It should put compiled assets into 'assets/'." :
    []
  checkExists Dir "assets/" $
    "All files in 'assets/' will be served."

data FileType
  = File
  | Dir

checkExists :: FileType -> FilePath -> String -> IO ()
checkExists typ path hint = do
  exists <- (isFile doesFileExist doesDirectoryExist) path
  when (not exists) $ do
    throwIO $ ErrorCall $ intercalate "\n" $
      ("missing " ++ isFile "file" "directory" ++ ": '" ++ path ++ "'") :
      ("Please create '" ++ path ++ "'.") :
      ("(" ++ hint ++ ")") :
      []
  where
    isFile :: a -> a -> a
    isFile a b = case typ of
      File -> a
      Dir -> b
