/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/
/*
** Machine registers mr0 - mr36 for the SPARC architecture.
**
** The first NUM_REAL_REGS of these are real machine registers.
** The others are just slots in a global array.
**
** This is a bit tricky on sparcs, because of the sliding register
** windows. There are only seven global registers (g1-g7), and of
** these three (g5-g7) are reserved for use by the operating system,
** and the other four (g1-g4) are all temp registers that get clobbered
** by calls to the C standard library functions.
**
** So it looks like we'll have to use the sliding registers.
** This won't work at all unless we are using gcc's non-local gotos.
*/

#ifndef USE_GCC_NONLOCAL_GOTOS
  #error "On SPARCs, you must use non-local gotos if you want global registers"
#else

#define NUM_REAL_REGS 10

register 	Word	mr0 __asm__("i0");
register	Word	mr1 __asm__("i1");	/* potentially non-clobbered */
register	Word	mr2 __asm__("i2");	/* potentially non-clobbered */
register	Word	mr3 __asm__("i3");	/* potentially non-clobbered */
register	Word	mr4 __asm__("i4");	/* potentially non-clobbered */
register	Word	mr5 __asm__("i5");	/* potentially non-clobbered */
register	Word	mr6 __asm__("l1");
register	Word	mr7 __asm__("l2");
register	Word	mr8 __asm__("l3");
register	Word	mr9 __asm__("l4");

/* we could use l5, l6, and l7 as well, */
/* but for the moment at least I'll leave them for gcc */

#define save_registers()			\
	(					\
		fake_reg[0] = mr0,		\
		fake_reg[1] = mr1,		\
		fake_reg[2] = mr2,		\
		fake_reg[3] = mr3,		\
		fake_reg[4] = mr4,		\
		fake_reg[5] = mr5,		\
		fake_reg[6] = mr6,		\
		fake_reg[7] = mr7,		\
		fake_reg[8] = mr8,		\
		fake_reg[9] = mr9,		\
		(void)0				\
	)

#define restore_registers()			\
	(					\
		mr0 = fake_reg[0],		\
		mr1 = fake_reg[1],		\
		mr2 = fake_reg[2],		\
		mr3 = fake_reg[3],		\
		mr4 = fake_reg[4],		\
		mr5 = fake_reg[5],		\
		mr6 = fake_reg[6],		\
		mr7 = fake_reg[7],		\
		mr8 = fake_reg[8],		\
		mr9 = fake_reg[9],		\
		(void)0				\
	)

/* for save_transient_registers(), we probably don't have to save
  the registers marked above as `potentially non-clobbered', but
  I haven't verified that yet */

#define save_transient_registers() save_registers()
#define restore_transient_registers() restore_registers()

#define	mr10	fake_reg[10]
#define	mr11	fake_reg[11]
#define	mr12	fake_reg[12]
#define	mr13	fake_reg[13]
#define	mr14	fake_reg[14]
#define	mr15	fake_reg[15]
#define	mr16	fake_reg[16]
#define	mr17	fake_reg[17]
#define	mr18	fake_reg[18]
#define	mr19	fake_reg[19]
#define	mr20	fake_reg[20]
#define	mr21	fake_reg[21]
#define	mr22	fake_reg[22]
#define	mr23	fake_reg[23]
#define	mr24	fake_reg[24]
#define	mr25	fake_reg[25]
#define	mr26	fake_reg[26]
#define	mr27	fake_reg[27]
#define	mr28	fake_reg[28]
#define	mr29	fake_reg[29]
#define	mr30	fake_reg[30]
#define	mr31	fake_reg[31]
#define	mr32	fake_reg[32]
#define	mr33	fake_reg[33]
#define	mr34	fake_reg[34]
#define	mr35	fake_reg[35]
#define	mr36	fake_reg[36]

#endif
