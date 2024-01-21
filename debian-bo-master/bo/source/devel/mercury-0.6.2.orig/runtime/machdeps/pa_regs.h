/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef PA_REGS_H
#define PA_REGS_H

/*
** Machine registers mr0 - mr36 for the HP-PA architecture.
**
** The first NUM_REAL_REGS of these are real machine registers.
** The others are just slots in a global array.
**
** At the moment we're only using 8 of the callee-save registers.
*/

#define NUM_REAL_REGS 8

register 	Word	mr0 __asm__("%r6");
register	Word	mr1 __asm__("%r7");
register	Word	mr2 __asm__("%r8");
register	Word	mr3 __asm__("%r9");
register	Word	mr4 __asm__("%r10");
register	Word	mr5 __asm__("%r11");
register	Word	mr6 __asm__("%r12");
register	Word	mr7 __asm__("%r13");

#define save_registers()	(	\
	fake_reg[0] = mr0,			\
	fake_reg[1] = mr1,			\
	fake_reg[2] = mr2,			\
	fake_reg[3] = mr3,			\
	fake_reg[4] = mr4,			\
	fake_reg[5] = mr5,			\
	fake_reg[6] = mr6,			\
	fake_reg[7] = mr7,			\
	(void)0					\
)

#define restore_registers()	(	\
	mr0 = fake_reg[0],			\
	mr1 = fake_reg[1],			\
	mr2 = fake_reg[2],			\
	mr3 = fake_reg[3],			\
	mr4 = fake_reg[4],			\
	mr5 = fake_reg[5],			\
	mr6 = fake_reg[6],			\
	mr7 = fake_reg[7],			\
	(void)0					\
)

#define save_transient_registers()	((void)0)
#define restore_transient_registers()	((void)0)

#define	mr8	fake_reg[8]
#define	mr9	fake_reg[9]
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

#endif /* PA_REGS_H */
