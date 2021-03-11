/*
 * Module      : SFC64
 * Copyright   : [2021] Trevor L. McDonell
 * License     : BSD3
 *
 * Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
 * Stability   : experimental
 * Portability : non-portable (GHC extensions)
 */

#ifndef __SFC64_H__
#define __SFC64_H__

#include <stdint.h>

typedef struct sfc64_data {
  int64_t a;
  int64_t b;
  int64_t c;
  int64_t counter;
} sfc64_t;

static inline int64_t sfc64(sfc64_t * state)
{
  // good sets include {24,11,3},{25,12,3},{},{}
  // older versions used {25,12,3}, which is decent
  enum {BARREL_SHIFT = 24, RSHIFT = 11, LSHIFT = 3};

  int64_t tmp = state->a + state->b + state->counter++;
  state->a = state->b ^ (state->b >> RSHIFT);
  state->b = state->c + (state->c << LSHIFT);
  state->c = ((state->c << BARREL_SHIFT) | (state->c >> (64-BARREL_SHIFT))) + tmp;
  return tmp;
}

static inline void sfc64_seed(sfc64_t * state, int64_t s)
{
  state->a = s;
  state->b = s;
  state->c = s;
  state->counter = 1;

  for (int i = 0; i < 8; i++)
    sfc64(state);
}

#endif

