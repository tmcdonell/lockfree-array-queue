/*
 * Module      : Delay
 * Copyright   : [2021] Trevor L. McDonell
 * License     : BSD3
 *
 * Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
 * Stability   : experimental
 * Portability : non-portable (GHC extensions)
 */

#include "cbits/delay.h"

void delay_init(delay_t * state, int64_t tid)
{
  sfc64_seed(state, tid);
}

void delay_exec(delay_t * state)
{
  int64_t n = sfc64(state);

  int j;
  for (j = 50; j < 50 + n % 100; ++j) {
    __asm__ ("nop");
  }
}

