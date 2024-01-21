/* irqprior/load.c -- irqprior module loader */

/*
// Copyright 1996 by Craig Estey -- See the file COPYING for details
*/

#include <linux/types.h>
#include <linux/errno.h>
#include <linux/kernel.h>
#include <linux/version.h>
#include <asm/system.h>

#include <irqhigh.c>

/* overridden by insmod command line (e.g. "priority=3,14") */
u_long priority[] = { IRQ_HIGH_MST_DFT, IRQ_HIGH_SLV_DFT };

/* NOTE: we are version independent but the symbol is required :-( */
char kernel_version[] = "We_Are_Version_Independent";

/* yes_we_used_it -- allow us to compile */
extern inline void
yes_we_used_it(void *x)
{
}

/* init_module -- called by modules package when installing the driver */
/* RETURNS: error code */
int
init_module(void)
{
	int err;
	int idx;
	u_long flags;
	u_char vals[2];

	err = 0;

	yes_we_used_it(irq_high_init);

	/* grab data from insmod command line */
	for (idx = 0;  idx <= 1;  ++idx)
		vals[idx] = priority[idx] & 0x0F;

	printk("irqtune: setting system IRQ priority to %u/%u\n",vals[0],vals[1]);

	save_flags(flags);
	cli();

	/* perform the set */
	irq_high_set(vals[0],vals[1]);

	restore_flags(flags);

	/* insure that things worked */
	if ((vals[0] != irq_high_list[0]) || (vals[1] != irq_high_list[1])) {
		printk("irqprior: ERROR expected %u/%u but got %u/%u\n",
			vals[0],vals[1],irq_high_list[0],irq_high_list[1]);
		err = -EINVAL;
	}

	return err;
}

/* cleanup_module -- called by modules package when removing the driver */
void cleanup_module(void)
{
}
