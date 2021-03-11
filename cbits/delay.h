/*
 * Module      : Delay
 * Copyright   : [2021] Trevor L. McDonell
 * License     : BSD3
 *
 * Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
 * Stability   : experimental
 * Portability : non-portable (GHC extensions)
 */

#ifndef __DELAY_H__
#define __DELAY_H__

#include "cbits/sfc64.h"
#include <stdint.h>

typedef sfc64_t delay_t;

void delay_init(delay_t * state, int64_t id);
void delay_exec(delay_t * state);

#endif

