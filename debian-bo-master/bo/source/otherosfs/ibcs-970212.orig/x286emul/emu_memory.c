#include <errno.h>
#include <signal.h>
#include "ldt.h"

#include <ibcs/xout.h>

#include "x286emul.h"
#include "syscall.h"

#ifdef DEBUG
#include "debug.h"
#endif


int
emu_stkgro(struct sigcontext_struct *sc)
{
	if ((sc->ebx & 0xffff) < 0x10000 - xext->xe_stksize) {
		errno = ENOMEM;
		return -1;
	}

	return 0;
}


#define BR_ARGSEG	1
#define BR_NEWSEG	2
#define BR_IMPSEG	3
#define BR_FREESEG	4
#define BR_HUGE		64
int
emu_brkctl(struct sigcontext_struct *sc)
{
	struct modify_ldt_ldt_s ldt_info;
	int cmd = (sc->ebx & 0xffff);
	int incr = (int)((sc->ecx & 0xffff) + (sc->esi << 16));
	int seg = ((sc->edi & 0xffff) >> 3);

	sc->ebx =0xff;
	errno = EINVAL;

	if (incr > 0x0000ffff
	|| (cmd == BR_NEWSEG && incr < 0)
	|| (cmd == BR_ARGSEG && incr < 0 && -incr > ldt[last_desc].limit))
		return -1;

	if (cmd == BR_IMPSEG)
		seg = last_desc;

	if (incr >= 0) {
		if (cmd == BR_ARGSEG
		&& ((seg != base_desc && (unsigned long)(ldt[seg].limit + incr) > 0x10000)
		|| (seg == base_desc && (unsigned long)(ldt[seg].limit + incr) > 0x10000 - xext->xe_stksize))) {
			return -1;
		}

		if (cmd == BR_NEWSEG
		|| (cmd == BR_IMPSEG
		&& ldt[seg].limit + incr > 0xffff)) {
			if (!(xexec->x_renv & XE_LDATA)
			&& last_desc == base_desc)
				return -1;
			seg = ++last_desc;
			ldt[seg].base = sbrk(0);
			ldt[seg].limit = 0;
			sbrk(65536);
		}

		sc->ebx = (seg << 3) | 7;
		sc->eax = ldt[seg].limit;
	} else {
		while (-incr > ldt[last_desc].limit) {
			incr += ldt[last_desc].limit;
			last_desc--;
		}

		seg = last_desc;
		sc->ebx = (seg << 3) | 7;
		sc->eax = ldt[seg].limit + incr;
	}

	ldt[seg].limit += incr;

	/* Don't change the size of the near segment. It is always a
	 * full 64k because it has the stack in the top.
	 */
	if (seg != base_desc) {
		ldt_info.entry_number = seg;
		ldt_info.read_exec_only = 0;
		ldt_info.contents = 0;
		ldt_info.seg_not_present = 0;
		ldt_info.seg_32bit = 0;
		ldt_info.limit_in_pages = 0;
		ldt_info.base_addr = ldt[seg].base;
		ldt_info.limit = ldt[seg].limit;
#ifdef DEBUG
		if (__dbf) fprintf(__dbf, "x286emul:   segment %d: base = 0x%08lx, length = 0x%08lx\n",
			ldt_info.entry_number, ldt_info.base_addr, ldt_info.limit);
#endif
		modify_ldt(1, &ldt_info, sizeof(ldt_info));
	}

	return sc->eax;
}
