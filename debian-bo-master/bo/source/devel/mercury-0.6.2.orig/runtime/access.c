/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#include	"imp.h"
#include	"access.h"

#if 0
/* this old junk is not used */

void reset(void)
{
	abort();
}

void help(void)
{
	printf("help text\n");
}
#endif	/* old junk */

Word get_reg(int num)
{
	restore_transient_registers();
 	switch (num) {

	case 1:  return r1;
	case 2:  return r2;
	case 3:  return r3;
	case 4:  return r4;
	case 5:  return r5;
	case 6:  return r6;
	case 7:  return r7;
	case 8:  return r8;
	case 9:  return r9;
	case 10: return r10;
	case 11: return r11;
	case 12: return r12;
	case 13: return r13;
	case 14: return r14;
	case 15: return r15;
	case 16: return r16;
	case 17: return r17;
	case 18: return r18;
	case 19: return r19;
	case 20: return r20;
	case 21: return r21;
	case 22: return r22;
	case 23: return r23;
	case 24: return r24;
	case 25: return r25;
	case 26: return r26;
	case 27: return r27;
	case 28: return r28;
	case 29: return r29;
	case 30: return r30;
	case 31: return r31;
	case 32: return r32;

	}

	/* NOTREACHED */
	fprintf(stderr, "register %d out of range in get_reg\n", num);
	abort();
	return 0;
}

Word set_reg(int num, Word val)
{
	restore_transient_registers();
 	switch (num) {

	case 1:  r1  = val; save_transient_registers(); return val;
	case 2:  r2  = val; save_transient_registers(); return val;
	case 3:  r3  = val; save_transient_registers(); return val;
	case 4:  r4  = val; save_transient_registers(); return val;
	case 5:  r5  = val; save_transient_registers(); return val;
	case 6:  r6  = val; save_transient_registers(); return val;
	case 7:  r7  = val; save_transient_registers(); return val;
	case 8:  r8  = val; save_transient_registers(); return val;
	case 9:  r9  = val; save_transient_registers(); return val;
	case 10: r10 = val; save_transient_registers(); return val;
	case 11: r11 = val; save_transient_registers(); return val;
	case 12: r12 = val; save_transient_registers(); return val;
	case 13: r13 = val; save_transient_registers(); return val;
	case 14: r14 = val; save_transient_registers(); return val;
	case 15: r15 = val; save_transient_registers(); return val;
	case 16: r16 = val; save_transient_registers(); return val;
	case 17: r17 = val; save_transient_registers(); return val;
	case 18: r18 = val; save_transient_registers(); return val;
	case 19: r19 = val; save_transient_registers(); return val;
	case 20: r20 = val; save_transient_registers(); return val;
	case 21: r21 = val; save_transient_registers(); return val;
	case 22: r22 = val; save_transient_registers(); return val;
	case 23: r23 = val; save_transient_registers(); return val;
	case 24: r24 = val; save_transient_registers(); return val;
	case 25: r25 = val; save_transient_registers(); return val;
	case 26: r26 = val; save_transient_registers(); return val;
	case 27: r27 = val; save_transient_registers(); return val;
	case 28: r28 = val; save_transient_registers(); return val;
	case 29: r29 = val; save_transient_registers(); return val;
	case 30: r30 = val; save_transient_registers(); return val;
	case 31: r31 = val; save_transient_registers(); return val;
	case 32: r32 = val; save_transient_registers(); return val;

	}

	/* NOTREACHED */
	fprintf(stderr, "register %d out of range in set_reg\n", num);
	abort();
	return 0;
}

#if 0
/* this old junk is not used */

Word get_mem(Word *addr)
{
	if (heapmin <= addr && addr < heapend)
		return *addr;

	if (detstackmin <= addr && addr < detstackend)
		return *addr;

	if (nondstackmin <= addr && addr < nondstackend)
		return *addr;

	/* NOTREACHED */
	fprintf(stderr, "address %p out of range in set_mem\n", (void *) addr);
	abort();
	return 0;
}

Word set_mem(Word *addr, Word val)
{
	if (heapmin <= addr && addr < heapend)
	{
		*addr = val;
		return val;
	}

	if (detstackmin <= addr && addr < detstackend)
	{
		*addr = val;
		return val;
	}

	if (nondstackmin <= addr && addr < nondstackend)
	{
		*addr = val;
		return val;
	}

	/* NOTREACHED */
	fprintf(stderr, "address %p out of range in set_mem\n", (void *) addr);
	abort();
	return 0;
}

Word createn(List *exprs)
{
	reg	List	*p;
	reg	Word	*oldhp;

	oldhp = hp;
	for_list(p, exprs)
	{
		hp += 1;
		hp[-1] = (Word) ldata(p);
	}

	return (Word) oldhp;
}

/*
**	Version of getline that does not truncate lines
**	but gets more space if necessary.
*/

int
getflexline(const char *prompt, FILE *file, char **line, int *line_max)
{
	reg	int	c, i, lim;
	reg	char	*buf;

	printf("%s", prompt);

	i = 0;
	lim = *line_max-2;
	buf = *line;
	while ((c = getc(file)) != EOF && c != '\n')
	{
		if (i >= lim)
		{
			*line_max *= 2;
			*line = resize_many(char, *line, *line_max);
			lim = *line_max-2;
			buf = *line;
		}

		buf[i++] = c;
	}
	
	if (c == '\n' || i > 0)
		buf[i++] = '\n';

	buf[i] = '\0';
	return i;
}

#endif /* old junk */
