{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Network.Wai.MakeAssets (
  serveAssets,
  Options(..),

  -- * re-exports
  Default(..),
) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Default
import           Data.List (intercalate)
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

-- | 'serveAssets' will create a wai 'Application' that serves files from the
-- "assets" directory.
--
-- The workflow that 'serveAssets' allows is similar to working on files (for
-- web-sites) that don't need compilation or generation, e.g. html, css, php or
-- javascript. You edit the file in an editor, save it, switch to a browser and
-- hit reload. 'serveAssets' makes sure your browser will be sent up-to-date
-- files.
--
-- To accomplish this, 'serveAssets' assumes that there's a "Makefile" in the
-- directory pointed to by 'clientDir' (default: "client"). This "Makefile" is
-- supposed to put compilation results into the "assets" directory. On __every__
-- request, 'serveAssets' will execute that "Makefile" and only start serving
-- files once the "Makefile" is done. ('serveAssets' makes sure not to run your
-- "Makefile" concurrently.)
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
