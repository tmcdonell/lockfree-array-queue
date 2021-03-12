{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UnboxedTuples       #-}
-- |
-- Module      : Main
-- Copyright   : [2021] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.Primitive
import Data.List                                                    ( sort )
import Data.Primitive.ByteArray
import Numeric
import System.Clock
import Text.Printf
import Prelude                                                      hiding ( null )

import GHC.Conc
import GHC.Exts

import qualified Data.Concurrent.Queue.MichaelScott                 as MS
import qualified Data.Concurrent.Queue.Array.FAA                    as FAA
import qualified LCRQ                                               as LCRQ


main :: IO ()
main = do
  let nprocs  = numCapabilities
      ops     = 100_000_000
      rounds  = 100             -- for burst
      iters   = 11              -- should be odd

  printf "running with %d thread(s) and %s operations\n\n" numCapabilities (showFFloatSIBase (Just 0) 1000 (fromIntegral ops :: Double) "")

  faa iters nprocs ops

  printf "faa-array-queue\n"
  printf "---------------\n"
  pairwise @(FAA.LinkedQueue RealWorld) iters nprocs ops
  burst @(FAA.LinkedQueue RealWorld) iters nprocs ops rounds

  printf "ms-queue\n"
  printf "--------\n"
  pairwise @MS.LinkedQueue iters nprocs ops
  burst @MS.LinkedQueue iters nprocs ops rounds

  printf "lcrq\n"
  printf "----\n"
  pairwise @LCRQ.LinkedQueue iters nprocs ops
  burst @LCRQ.LinkedQueue iters nprocs ops rounds


class LinkedQueue q where
  type Handle q
  new      :: Int -> IO (q a)
  register :: q a -> Int -> IO (Handle q)
  enqueue  :: q a -> Handle q -> a -> IO ()
  dequeue  :: q a -> Handle q -> IO (Maybe a)

instance LinkedQueue MS.LinkedQueue where
  type Handle MS.LinkedQueue = ()
  {-# INLINE new      #-}
  {-# INLINE register #-}
  {-# INLINE enqueue  #-}
  {-# INLINE dequeue  #-}
  new _        = MS.newQ
  enqueue q _  = MS.pushL q
  dequeue q _  = MS.tryPopR q
  register _ _ = return ()

instance LinkedQueue (FAA.LinkedQueue RealWorld) where
  type Handle (FAA.LinkedQueue RealWorld) = ()
  {-# INLINE new      #-}
  {-# INLINE register #-}
  {-# INLINE enqueue  #-}
  {-# INLINE dequeue  #-}
  new _        = FAA.new
  enqueue q _  = FAA.pushL q
  dequeue q _  = FAA.tryPopR q
  register _ _ = return ()

instance LinkedQueue LCRQ.LinkedQueue where
  type Handle LCRQ.LinkedQueue = LCRQ.Handle
  {-# INLINE new      #-}
  {-# INLINE register #-}
  {-# INLINE enqueue  #-}
  {-# INLINE dequeue  #-}
  new      = LCRQ.new
  register = LCRQ.thread_register
  enqueue  = LCRQ.pushL
  dequeue  = LCRQ.tryPopR


{-# INLINE pairwise #-}
pairwise :: forall q. LinkedQueue q => Int -> Int -> Int -> IO ()
pairwise iters nprocs nops = do
  let lIMIT = nops `quot` nprocs

      bench :: Int -> IO [[Integer]]
      bench !i
        | i > iters = return []
        | otherwise = do
            q  <- new nprocs
            d  <- forkThreads nprocs (lambda q)
            ds <- bench (i+1)
            return (d:ds)

      lambda :: LinkedQueue q => q Int -> Int -> IO Integer
      lambda q tid = do
        h <- register q tid
        let go !i !v
              | i > lIMIT = return ()
              | otherwise = do
                  enqueue q h v

                  mv <- dequeue q h
                  v' <- case mv of
                          Just x  -> return x
                          Nothing -> error (printf "thread %d: failed to dequeue!" tid)

                  go (i+1) v'

        start <- getTime Monotonic
        go 0 (tid+1)
        end <- getTime Monotonic
        return (toNanoSecs (end - start))
  --
  printf "benchmarking pairwise...\n"
  analyse nprocs =<< bench 0
  printf "\n"


{-# INLINE burst #-}
burst :: forall q. LinkedQueue q => Int -> Int -> Int -> Int -> IO ()
burst iters nprocs nops rounds = do
  let lIMIT  = nops `quot` rounds `quot` nprocs

      bench :: Int -> IO ([[Integer]], [[Integer]])
      bench !i
        | i > iters = return ([], [])
        | otherwise = do
            q  <- new nprocs
            let go !j
                  | j > rounds = return (replicate nprocs 0, replicate nprocs 0)
                  | otherwise  = do
                      e'        <- forkThreads nprocs (enq q)
                      d'        <- forkThreads nprocs (deq q)
                      (es',ds') <- go (j+1)
                      return (zipWith (+) e' es', zipWith (+) d' ds')
            (e, d)  <- go 0
            (es,ds) <- bench (i+1)
            return (e:es, d:ds)

      enq :: LinkedQueue q => q Int -> Int -> IO Integer
      enq q tid = do
        h <- register q tid
        let go !i
              | i > lIMIT = return ()
              | otherwise = do
                  enqueue q h tid
                  go (i+1)
        --
        start <- getTime Monotonic
        go 0
        end <- getTime Monotonic
        return (toNanoSecs (end - start))

      deq :: LinkedQueue q => q Int -> Int -> IO Integer
      deq q tid = do
        h <- register q tid
        let go !i
              | i > lIMIT = return ()
              | otherwise = do
                  mv <- dequeue q h
                  _  <- case mv of
                          Just x  -> return x
                          Nothing -> error (printf "thread %d: failed to dequeue!" tid)
                  go (i+1)
        --
        start <- getTime Monotonic
        go 0
        end <- getTime Monotonic
        return (toNanoSecs (end - start))
  --
  printf "benchmarking burst...\n"
  (es, ds) <- bench 0
  printf "enque:\n"
  analyse nprocs es
  printf "dequeue:\n"
  analyse nprocs ds
  printf "\n"


faa :: Int -> Int -> Int -> IO ()
faa iters nprocs nops = do
  let lIMIT = nops `quot` nprocs

      bench :: Int -> IO [[Integer]]
      bench !i
        | i > iters = return []
        | otherwise = do
            mba <- newAlignedPinnedByteArray 256 128
            d   <- forkThreads nprocs (lambda mba)
            ds  <- bench (i+1)
            return (d:ds)

      lambda :: MutableByteArray RealWorld -> Int -> IO Integer
      lambda (MutableByteArray mba#) _ = do
        let go :: Int -> IO ()
            go !i | i > lIMIT = return ()
                  | otherwise = do
                      primitive_ $ \s0# -> case fetchAddIntArray# mba# 16# 1# s0# of { (# s1#, _ #) -> s1# }
                      go (i+1)

        start <- getTime Monotonic
        go 0
        end <- getTime Monotonic
        return (toNanoSecs (end - start))
  --
  printf "benchmarking FAA...\n"
  analyse nprocs =<< bench 0
  printf "\n"


forkThreads :: Int -> (Int -> IO a) -> IO [a]
forkThreads nprocs work = do
  ready <- newEmptyMVar

  let work' tid = do
        var <- newEmptyMVar
        _   <- forkOn tid $ do
          readMVar ready  -- multiple wake-up
          res <- work tid
          putMVar var res
        return var

  results <- mapM work' [0 .. nprocs - 1]
  putMVar ready ()
  mapM takeMVar results

analyse :: Int -> [[Integer]] -> IO ()
analyse nprocs deltas = do
  let
      secs :: Integer -> Double
      secs ns = fromIntegral ns * 1.0E-9

      n       = length deltas
      times   = sort (map (\xs -> (secs (sum xs)) / fromIntegral nprocs) deltas)  -- per thread time
      median  = times !! (n `quot` 2)
      mean    = sum times / fromIntegral n
      --
      det     = map (\x -> (x - mean) ^ (2 :: Int)) times
      mean2   = sum det / fromIntegral n
      stddev  = sqrt mean2
  --
  printf "  mean:   %s\n" (showFFloatSIBase (Just 3) 1000 mean "s")
  printf "  median: %s\n" (showFFloatSIBase (Just 3) 1000 median "s")
  printf "  stddev: %s\n" (showFFloatSIBase (Just 3) 1000 stddev "s")

showFFloatSIBase :: RealFloat a => Maybe Int -> a -> a -> ShowS
showFFloatSIBase prec !base !k
  = showString
  $ case pow of
      4   -> with "T"
      3   -> with "G"
      2   -> with "M"
      1   -> with "k"
      -1  -> with "m"
      -2  -> with "µ"
      -3  -> with "n"
      -4  -> with "p"
      _   -> showGFloat prec k " "      -- no unit or unhandled SI prefix
  where
    !k'         = k / (base ^^ pow)
    !pow        = floor (logBase base k) :: Int
    with unit   = showFFloat prec k' (' ':unit)

