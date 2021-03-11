{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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

import Delay

import Control.Concurrent
import Control.DeepSeq
import Control.Monad
import Data.List                                                    ( sort )
import GHC.Conc
import GHC.Prim
import Gauge.Main
import System.Exit
import Text.Printf
import Prelude                                                      hiding ( null )

import qualified Data.Concurrent.Queue.MichaelScott                 as MS
import qualified Data.Concurrent.Queue.Array.FAA                    as FAA


class LinkedQueue q where
  new     :: IO (q a)
  null    :: q a -> IO Bool
  enqueue :: q a -> a -> IO ()
  dequeue :: q a -> IO (Maybe a)


instance LinkedQueue MS.LinkedQueue where
  {-# INLINE new     #-}
  {-# INLINE null    #-}
  {-# INLINE enqueue #-}
  {-# INLINE dequeue #-}
  new = MS.newQ
  null = MS.nullQ
  enqueue = MS.pushL
  dequeue = MS.tryPopR

instance LinkedQueue (FAA.LinkedQueue RealWorld) where
  {-# INLINE new     #-}
  {-# INLINE null    #-}
  {-# INLINE enqueue #-}
  {-# INLINE dequeue #-}
  new = FAA.new
  null = FAA.null
  enqueue = FAA.pushL
  dequeue = FAA.tryPopR

main :: IO ()
main = do
  let nprocs = numCapabilities
      nops   = 100_000_000

  -- q <- FAA.new
  -- r <- forkThreads nprocs (pairwise nprocs nops q)

  -- x <- null q
  -- printf "queue is empty? %s\n" (show x)

  -- putStr "verifying... "
  -- verify nprocs r
  -- putStrLn "success!"

  -- benchmarking thread creating and verification as well, but this should
  -- be constant each run (and for every implementation)
  --
  defaultMain
    [ bench "msqueue" $ perRunEnv (new @MS.LinkedQueue)              (\q -> verify nprocs =<< forkThreads nprocs (pairwise nprocs nops q))
    , bench "faa"     $ perRunEnv (new @(FAA.LinkedQueue RealWorld)) (\q -> verify nprocs =<< forkThreads nprocs (pairwise nprocs nops q))
    ]


instance NFData (MS.LinkedQueue a) where
  rnf q = q `seq` ()

instance NFData (FAA.LinkedQueue RealWorld a) where
  rnf q = q `seq` ()

{-# SPECIALISE INLINE pairwise :: Int -> Int -> MS.LinkedQueue Int -> Int -> IO Int #-}
{-# SPECIALISE INLINE pairwise :: Int -> Int -> FAA.LinkedQueue RealWorld Int -> Int -> IO Int #-}
pairwise :: LinkedQueue q => Int -> Int -> q Int -> Int -> IO Int
pairwise nprocs nops q tid = do
  s <- delay_init tid

  let lIMIT = nops `quot` nprocs

      go !i !v
        | i > lIMIT = return v
        | otherwise = do
            enqueue q v
            delay_exec s

            mv <- dequeue q
            v' <- case mv of
                    Just x  -> return x
                    Nothing -> error (printf "thread %d: failed to dequeue!" tid)
            delay_exec s

            go (i+1) v'

  go 0 (tid+1)

verify :: Int -> [Int] -> IO ()
verify nprocs result = do
  let result' = sort result
  unless (result' == [1 .. nprocs]) $ do
    printf "ERROR:\n  expected: %s\n  received: %s\n" (show [1..nprocs]) (show result')
    exitFailure

forkThreads :: Int -> (Int -> IO Int) -> IO [Int]
forkThreads nprocs work = do
  mapM takeMVar =<< mapM work' [0 .. nprocs - 1]
  where
    work' :: Int -> IO (MVar Int)
    work' tid = do
      var <- newEmptyMVar
      _   <- forkOn tid (work tid >>= putMVar var)
      return var

