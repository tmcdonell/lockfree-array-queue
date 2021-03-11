{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Delay
-- Copyright   : [2021] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Delay where

import Foreign.Ptr
import Foreign.ForeignPtr


newtype Delay = Delay (ForeignPtr ())

delay_init :: Int -> IO Delay
delay_init tid = do
  state <- mallocForeignPtrBytes 32
  withForeignPtr state $ \p -> delay_init_c p tid
  return (Delay state)

delay_exec :: Delay -> IO ()
delay_exec (Delay state) =
  withForeignPtr state delay_exec_c

foreign import ccall unsafe "delay_init" delay_init_c :: Ptr () -> Int -> IO ()
foreign import ccall unsafe "delay_exec" delay_exec_c :: Ptr () -> IO ()

