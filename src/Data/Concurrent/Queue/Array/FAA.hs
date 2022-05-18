{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim     #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE StrictData               #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE UnliftedFFITypes         #-}
{-# LANGUAGE ViewPatterns             #-}
-- {-# OPTIONS_GHC -fobject-code  #-}
-- {-# OPTIONS_GHC -ddump-simpl   #-}
-- {-# OPTIONS_GHC -dsuppress-all #-}
-- |
-- Module      : Data.Concurrent.Queue.Array.FAA
-- Copyright   : [2021] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- <http://concurrencyfreaks.blogspot.com/2016/11/faaarrayqueue-mpmc-lock-free-queue-part.html>
--

module Data.Concurrent.Queue.Array.FAA (

  LinkedQueue,
  new, new#,
  null, null#,
  pushL, pushL#,
  tryPopR, tryPopR#,

) where

import Prelude                                                      hiding ( null, head, tail )
import Control.Monad.Primitive
import GHC.Exts


data Node s e = Node
  !(MutableByteArray# s)               -- enqidx deqidx
  !(SmallMutableArray# s e)            -- items
  !(SmallMutableArray# s (Node s e))   -- previous/next node

data LinkedQueue s e = LinkedQueue
  !(SmallMutableArray# s (Node s e))   -- head/tail pointers


-- XXX: A lot of work just to get two unique memory addresses... ._.
pattern Null# :: null
pattern Null# <- (isNull# -> True)
  where Null# = __null

pattern Taken# :: taken
pattern Taken# <- (isTaken# -> True)
  where Taken# = __taken

isNull# :: a -> Bool
isNull# x = isTrue# (reallyUnsafePtrEquality# (unsafeCoerce# x) __null)

isTaken# :: a -> Bool
isTaken# x = isTrue# (reallyUnsafePtrEquality# (unsafeCoerce# x) __taken)

__null :: null
__null  = case newByteArray# 0# realWorld# of { (# _, ba# #) -> unsafeCoerce# ba# }
{-# NOINLINE __null #-}

__taken :: taken
__taken = case newByteArray# 0# realWorld# of { (# _, ba# #) -> unsafeCoerce# ba# }
{-# NOINLINE __taken #-}


-- Contended variables so should be double-cache aligned to avoid false
-- sharing. Assume a 64-bit architecture (pointers are 8-bytes) and a cache
-- line of 64 bytes and that the pre-fetcher fetches two consecutive cache
-- lines at a time (i.e. x86). Thus, we pad each head and tail pointer for
-- example to their own 128-byte region.
--
-- Technically these should be aligned on a 128-byte boundary, rather than
-- 128-bytes apart (including the first contended variable from the object
-- header) but we can't express that with SmallArray#.

newNode# :: e -> State# s -> (# State# s, Node s e #)
newNode# x s0# =
  case newAlignedPinnedByteArray# 384# 128# s0#      of { (# s1#, indices# #) ->
  case newSmallArray# 128# (unsafeCoerce# Null#) s1# of { (# s2#, items# #) -> -- BUFFER_SIZE
  case newSmallArray# 48# (unsafeCoerce# Null#) s2#  of { (# s3#, next# #) ->
  case writeIntArray# indices# 16# 1# s3#            of { s4# -> -- enqidx
  case writeIntArray# indices# 32# 0# s4#            of { s5# -> -- deqidx
  case writeSmallArray# items# 0# x s5#              of { s6# ->
    (# s6#, Node indices# items# next# #) }}}}}}

newEmptyNode# :: State# s -> (# State# s, Node s e #)
newEmptyNode# s0# =
  case newAlignedPinnedByteArray# 384# 128# s0#      of { (# s1#, indices# #) ->
  case newSmallArray# 128# (unsafeCoerce# Null#) s1# of { (# s2#, items# #) -> -- BUFFER_SIZE
  case newSmallArray# 48# (unsafeCoerce# Null#) s2#  of { (# s3#, next# #) ->
  case writeIntArray# indices# 16# 0# s3#            of { s4# -> -- enqidx
  case writeIntArray# indices# 32# 0# s4#            of { s5# -> -- deqidx
    (# s5#, Node indices# items# next# #) }}}}}


-- | Create a new (empty) queue
--
{-# INLINEABLE new #-}
new :: PrimMonad m => m (LinkedQueue (PrimState m) e)
new = primitive new#

{-# INLINEABLE new# #-}
new# :: State# s -> (# State# s, LinkedQueue s e #)
new# s0# =
  case newEmptyNode# s0#                            of { (# s1#, sentinelNode #) ->
  case newSmallArray# 48# (unsafeCoerce# Null#) s1# of { (# s2#, ptrs# #) ->
  case writeSmallArray# ptrs# 16# sentinelNode s2#  of { s3# -> -- headPtr
  case writeSmallArray# ptrs# 32# sentinelNode s3#  of { s4# -> -- tailPtr
    (# s4#, LinkedQueue ptrs# #)
  }}}}


-- | Is the queue currently empty? Only safe to call when the queue is idle
-- (no active producers or consumers).
--
{-# INLINEABLE null #-}
null :: PrimMonad m => LinkedQueue (PrimState m) e -> m Bool
null q = primitive $ \s0# ->
  case null# q s0# of { (# s1#, result# #) ->
    (# s1#, isTrue# result# #)
  }

null# :: LinkedQueue s e -> State# s -> (# State# s, Int# #)
null# (LinkedQueue ptrs#) s0# =
  case readSmallArray# ptrs# 16# s0# of { (# s1#, headPtr #) ->
  case readSmallArray# ptrs# 32# s1# of { (# s2#, tailPtr #) ->
  case isTrue# (reallyUnsafePtrEquality# headPtr tailPtr) of
    False -> (# s2#, 0# #)
    True  -> case headPtr                              of { Node indices# _ _  ->
             case atomicReadIntArray# indices# 16# s2# of { (# s3#, enqidx# #) ->
             case atomicReadIntArray# indices# 32# s3# of { (# s4#, deqidx# #) ->
               (# s4#, enqidx# ==# deqidx# #)
             }}}
  }}


-- | Push an element onto the queue. The queue grows as needed so this
-- always succeeds. Uncontended enqueues require one FAA instruction and
-- one CAS instruction. Lock-free progress guarantee.
--
{-# INLINEABLE pushL #-}
pushL :: PrimMonad m => LinkedQueue (PrimState m) e -> e -> m ()
pushL q x = primitive_ (pushL# q x)

{-# INLINEABLE pushL# #-}
pushL# :: LinkedQueue s e -> e -> State# s -> State# s
pushL# (LinkedQueue ptrs#) x =
  let loop s0# =
        case readSmallArray# ptrs# 32# s0#         of { (# s1#, ltail #) ->
        case ltail                                 of { Node indices# items# next# ->
        case fetchAddIntArray# indices# 16# 1# s1# of { (# s2#, enqidx# #) ->
        case isTrue# (enqidx# ># 127#)             of -- BUFFER_SIZE - 1
          -- This node is full
          True -> case readSmallArray# ptrs# 32# s2#                   of { (# s3#, ltail' #) ->
                  case isTrue# (reallyUnsafePtrEquality# ltail ltail') of
                    False -> loop s3#
                    True  -> case readSmallArray# next# 16# s3# of { (# s4#, lnext #) ->
                             case isNull# lnext                 of
                               -- swing the tail pointer
                               False -> case casSmallArray# ptrs# 32# ltail lnext s4# of { (# s5#, _, _ #) ->
                                          loop s5#
                                        }
                               -- add the new node
                               True  -> case newNode# x s4#                                          of { (# s5#, node #) ->
                                        case casSmallArray# next# 16# (unsafeCoerce# Null#) node s5# of { (# s6#, fail#, _ #) ->
                                        case isTrue# fail#                                           of
                                          True  -> loop s6#
                                          False -> case casSmallArray# ptrs# 32# ltail node s6# of { (# s7#, _, _ #) ->
                                                     s7#
                                                   }
                                        }}
                             }
                  }

          -- Try to enqueue the item
          False -> case casSmallArray# items# enqidx# (unsafeCoerce# Null#) x s2# of { (# s3#, fail#, _ #) ->
                   case isTrue# fail#                                             of
                     True  -> loop s3#
                     False -> s3#
                   }
        }}}
  in
  loop


-- | Pop an element from the queue, if one is available. Uncontended
-- dequeue requires one FAA instruction and one CAS instruction. Lock-free
-- progress guarantee.
--
{-# INLINEABLE tryPopR #-}
tryPopR :: PrimMonad m => LinkedQueue (PrimState m) e -> m (Maybe e)
tryPopR q = primitive $ \s0# ->
  case tryPopR# q s0# of { (# s1#, valid#, item #) ->
  case isTrue# valid# of
    False -> (# s1#, Nothing #)
    True  -> (# s1#, Just item #)
  }

{-# INLINEABLE tryPopR# #-}
tryPopR# :: LinkedQueue s e -> State# s -> (# State# s, Int#, e #)
tryPopR# (LinkedQueue ptrs#) =
  let loop s0# =
        case readSmallArray# ptrs# 16# s0#        of { (# s1#, lhead #) ->
        case lhead                                of { Node indices# items# next# ->
        case atomicReadIntArray# indices# 32# s1# of { (# s2#, deqidx# #) ->
        case atomicReadIntArray# indices# 16# s2# of { (# s3#, enqidx# #) ->
        case readSmallArray# next# 16# s3#        of { (# s4#, lnext #) ->
        case isTrue# (deqidx# >=# enqidx#) && isNull# lnext of
          True  -> (# s4#, 0#, Null# #)
          False -> case fetchAddIntArray# indices# 32# 1# s4# of { (# s5#, idx# #) ->
                   case isTrue# (idx# ># 127#)                of -- BUFFER_SIZE - 1
                     -- This node has been drained, check if there is another one
                     True  -> case readSmallArray# next# 16# s5# of { (# s6#, lnext' #) ->
                              case isNull# lnext'                of
                                True  -> (# s6#, 0#, Null# #)
                                False -> case casSmallArray# ptrs# 16# lhead lnext' s6# of { (# s7#, _, _ #) ->
                                           loop s7#
                                         }
                              }

                     -- Try to dequeue the item
                     False -> case xchgSmallArray# items# idx# (unsafeCoerce# Taken#) s5# of { (# s6#, item #) ->
                              case isNull# item                                           of
                                True  -> loop s6#
                                False -> (# s6#, 1#, item #)
                              }
                   }
      }}}}}
  in
  loop

foreign import prim "xchgSmallArrayzh"
  xchgSmallArray# :: SmallMutableArray# s a -> Int# -> Any a -> State# s -> (# State# s, a #)

