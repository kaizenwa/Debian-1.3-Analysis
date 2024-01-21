#include <linux/unistd.h>
#include <errno.h>

#include "x286emul.h"

#ifdef DEBUG
#include "debug.h"
#endif

#include "ldt.h"


#ifdef DEBUG
char *desc_type[] = {
	"NULL",
	"286TSS",
	"LDT",
	"286BSY",
	"286CGT",
	"TASKGT",
	"286IGT",
	"286TGT",
	"NULL2",
	"386TSS",
	"NULL3",
	"386BSY",
	"386CGT",
	"NULL4",
	"386IGT",
	"386TGT",
	"MEMRO",
	"MEMROA",
	"MEMRW",
	"MEMRWA",
	"MEMROD",
	"MEMRODA",
	"MEMRWD",
	"MEMRWDA",
	"MEMX",
	"MEMXA",
	"MEMXR",
	"MEMXRA",
	"MEMXC",
	"MEMXAC",
	"MEMXRC"
	"MEMXRAC"
};
#endif


struct ldt_desc ldt[LDT_ENTRIES] = { 0, };
int last_desc = 0, base_desc = 0;


_syscall3(int, modify_ldt, int, func, void *, ptr, unsigned long, bytecount)


void
ldt_init(void)
{
	char buf[LDT_ENTRIES*LDT_ENTRY_SIZE];
	descriptor_t *d = (descriptor_t *)buf;
	unsigned long base_addr, limit;
	int type, dpl, i, count;

	memset(buf, '\0', sizeof(buf));
	if ((count = modify_ldt(0, buf, sizeof(buf))) < 0) {
		fprintf(stderr, "x286emul: can't get ldt! (errno=%d)\n", errno);
		exit(1);
	}
	count = count / LDT_ENTRY_SIZE;

#ifdef DEBUG
	if (__dbf)
		fprintf(__dbf, "SLOT  BASE/SEL    LIM/OFF     TYPE     DPL  ACCESSBITS\n");
#endif
	for (i=0; i < count; i++) {
		ldt[i].base = d[i].sd.lobase1
			| (d[i].sd.lobase2 << 16)
			| (d[i].sd.hibase << 24);
		ldt[i].limit = d[i].sd.lolimit | (d[i].sd.hilimit << 16);
		ldt[i].type = d[i].sd.type;
		ldt[i].dpl = d[i].sd.dpl;

		if ((ldt[i].base > 0) || (ldt[i].limit > 0 )) {
			if (i > last_desc)
				last_desc = i;
#ifdef DEBUG
			if (d[i].ad.type < 16) {
				if (__dbf) fprintf(__dbf, "%03d   0x%08lx  0x%08lx  %-7s  %03d %s CNT=%d\n",
					i,
					d[i].gd.selector,
					d[i].gd.looffset | (d[i].gd.hioffset << 16),
					desc_type[d[i].gd.type],
					d[i].gd.dpl,
					d[i].gd.p ? " PRESENT" : "",
					d[i].gd.stkcpy);
			} else {
				if (__dbf) fprintf(__dbf, "%03d   0x%08lx  0x%08lx  %-7s  %03d %s%s%s%s%s%s%s\n",
					i,
					ldt[i].base, ldt[i].limit,
					desc_type[ldt[i].type],
					ldt[i].dpl,
					d[i].sd.type & 1 ? " ACCS'D" : "",
					d[i].sd.type & 2 ? " R&W" : " R&X",
					d[i].sd.p ? " PRESENT" : "",
					d[i].sd.user ? " USER" : "",
					d[i].sd.x ? " X" : "",
					d[i].sd.def32 ? " 32" : "",
					d[i].sd.gran ? " PAGES" : "");
			}
#endif
		}
	}
	ldt[init_ds >> 3].limit = limit_stk;
}
