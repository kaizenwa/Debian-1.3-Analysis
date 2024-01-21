/* irqhigh.h -- set high priority IRQ's */

/*
// Copyright 1996 by Craig Estey -- See the file COPYING for details
*/

#include <linux/types.h>

#ifndef i386
#error irqhigh: this only works/is needed for x86/PC architecture -- sorry
#endif

/* original PC issue configuration */
#define IRQ_HIGH_MST_BAD		0
#define IRQ_HIGH_SLV_BAD		8

/* defaults based on standard configuration */
#define IRQ_HIGH_MST_DFT		3
#define IRQ_HIGH_SLV_DFT		14
