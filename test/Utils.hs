{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Utils (testInIO) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

testInIO :: Q a -> IO a
testInIO = unWrap . runQ

newtype Wrap a = Wrap {unWrap :: IO a}
  deriving (Functor, Applicative, Monad)

instance Quasi Wrap where
  qRunIO = Wrap . qRunIO
  qAddDependentFile = \ _ -> return ()
