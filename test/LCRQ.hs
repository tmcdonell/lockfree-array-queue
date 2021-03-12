{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : LCRQ
-- Copyright   : [2021] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LCRQ where

import Foreign.C.Types
import GHC.ForeignPtr
import GHC.Ptr
import GHC.Stable

data LinkedQueue a = LinkedQueue {-# UNPACK #-} !(ForeignPtr ())
data Handle        = Handle {-# UNPACK #-} !(ForeignPtr ())

new :: Int -> IO (LinkedQueue a)
new nprocs = do
  fp <- mallocForeignPtrAlignedBytes _sizeof_queue_t _page_size
  withForeignPtr fp $ \p -> queue_init_c p (fromIntegral nprocs)
  return $! LinkedQueue fp

thread_register :: LinkedQueue a -> Int -> IO Handle
thread_register (LinkedQueue fq) tid = do
  fh <- mallocForeignPtrAlignedBytes _sizeof_handle_t _page_size
  withForeignPtr fq $ \q ->
    withForeignPtr fh $ \h -> queue_register_c q h (fromIntegral tid)
  return $! Handle fh

pushL :: LinkedQueue a -> Handle -> a -> IO ()
pushL (LinkedQueue fq) (Handle fh) val = do
  p <- newStablePtr val
  withForeignPtr fq $ \q ->
    withForeignPtr fh $ \h ->
      enqueue_c q h (castStablePtrToPtr p)

tryPopR :: LinkedQueue a -> Handle -> IO (Maybe a)
tryPopR (LinkedQueue fq) (Handle fh) = do
  p <- withForeignPtr fq $ \q ->
         withForeignPtr fh $ \h ->
           dequeue_c q h
  if p == nullPtr
     then return Nothing
     else do
       let sp = castPtrToStablePtr p
       v <- deRefStablePtr sp
       freeStablePtr sp
       return (Just v)


_sizeof_queue_t :: Int
_sizeof_queue_t = _page_size

_sizeof_handle_t :: Int
_sizeof_handle_t = _page_size

_page_size :: Int
_page_size = 4096

foreign import ccall unsafe "queue_init" queue_init_c :: Ptr () -> CInt -> IO ()
foreign import ccall unsafe "queue_register" queue_register_c :: Ptr () -> Ptr () -> CInt -> IO ()
foreign import ccall unsafe "enqueue" enqueue_c :: Ptr () -> Ptr () -> Ptr () -> IO ()
foreign import ccall unsafe "dequeue" dequeue_c :: Ptr () -> Ptr () -> IO (Ptr ())

