{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Utils (testInIO, collectDependentFiles) where

import           Control.Monad.Writer
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

testInIO :: Q a -> IO a
testInIO = unWrap . runQ

newtype Wrap a = Wrap {unWrap :: IO a}
  deriving (Functor, Applicative, Monad)

instance Quasi Wrap where
  qRunIO = Wrap . qRunIO
  qAddDependentFile = \ _ -> return ()

collectDependentFiles :: Q () -> IO [FilePath]
collectDependentFiles = execWriterT . runQ

instance Quasi (WriterT [FilePath] IO) where
  qRunIO = Control.Monad.Writer.lift
  qAddDependentFile = tell . pure
