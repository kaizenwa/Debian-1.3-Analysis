/* irqhigh.c -- set high priority IRQ's */

/*
// Copyright 1996 by Craig Estey -- See the file COPYING for details
*/

#include <irqhigh.h>
#include <asm/io.h>

#if defined(__GNUC__) && (! defined(__OPTIMIZE__))
#error irqhigh: yes, we really need the optimizer enabled
#endif

/* current value */
u_char irq_high_list[2] = { IRQ_HIGH_MST_BAD, IRQ_HIGH_SLV_BAD };

/* irq_high_offset -- compute offset into irq_high_list */
extern inline u_char
irq_high_offset(u_char irq_no)
{

	irq_no >>= 3;

	return irq_no;
}

/* irq_high_get -- get priority of IRQ */
static u_char irq_high_get(u_char irq_no)
{
	u_char hi;
	u_char prior;
	u_char idx;

	idx = irq_high_offset(irq_no);
	hi = irq_high_list[idx];

	prior = (irq_no - hi) & 0x07;
	if (idx > 0)
		prior += irq_high_get(2);

	return prior;
}

/* irq_high_set1 -- set highest priority IRQ device on given 8259 */
static void irq_high_set1(u_char irq_no)
{
	u_char lo;
	u_short port;
	u_char idx;

	/* decide on master vs. slave 8259 */
	idx = irq_high_offset(irq_no);
	if (idx > 0) {
		port = 0xA0;
		irq_high_set1(2);
	}

	/* master */
	else
		port = 0x20;

	irq_high_list[idx] = irq_no;
	irq_no &= 0x07;

	/* set specific priority (of lowest priority device) */
	lo = (irq_no - 1) & 0x07;
	lo |= 0xC0;
	outb(lo,port);
}

/* irq_high_set -- set highest priority IRQ device for both 8259's */
static void irq_high_set(u_char mst_no,u_char slv_no)
{

	/* order is important -- set slave then master */
	irq_high_set1(slv_no);
	irq_high_set1(mst_no);
}

/* irq_high_init -- initialize to default */
static void irq_high_init(void)
{

	/* set standard COM ports highest on master and disk controller on slave */
	irq_high_set(IRQ_HIGH_MST_DFT,IRQ_HIGH_SLV_DFT);
}
