#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <linux/unistd.h>

#include <ibcs/xout.h>

#include "x286emul.h"
#include "ldt.h"
#include "debug.h"


struct xexec *xexec = (struct xexec *)X286_MAP_ADDR;
struct xext *xext = (struct xext *)(X286_MAP_ADDR + sizeof(struct xexec));

#ifdef DEBUG
FILE *__dbf = NULL;
#endif

static volatile int in_emulator = 1;


#ifdef DEBUG
void
dump_state(unsigned char *laddr, unsigned short *stkladdr,
	struct sigcontext_struct *sc)
{
	int i;

	if (!__dbf)
		return;

	fprintf(__dbf, "Trap 0x%08lx, error 0x%08lx\n", sc->trapno, sc->err);
	fprintf(__dbf, "EIP: 0x%02x:0x%08lx (0x%08lx)\n",
		sc->cs, sc->eip, laddr);
	fprintf(__dbf, "STK: 0x%02x:0x%08lx (0x%08lx)\n",
		sc->ss, sc->esp_at_signal, stkladdr);
	fprintf(__dbf, "EAX: 0x%08lx  EBX: 0x%08lx  ECX: 0x%08lx  EDX: 0x%08lx\n",
		sc->eax, sc->ebx, sc->ecx, sc->edx);
	fprintf(__dbf, "ESI: 0x%08lx  EDI: 0x%08lx\n", sc->esi, sc->edi);
	fprintf(__dbf, "DS:  0x%02x        ES:  0x%02x        SS:  0x%02x\n",
		sc->ds, sc->es, sc->ss);
	fprintf(__dbf, "CODE: ");
	for (i=-8; i<0; i++)
		fprintf(__dbf, " %02x", laddr[i]);
	fprintf(__dbf, " |");
	for (i=0; i<8; i++)
		fprintf(__dbf, " %02x", laddr[i]);
	fprintf(__dbf, "\n");
	fprintf(__dbf, "STACK:");
	for (i=0; i<8; i++)
		fprintf(__dbf, " %04x", stkladdr[i]);
	fprintf(__dbf, "\n");
}
#endif


/* Libc sigaction buggers about with sa_restorer. */
int
sigaction(int sig, struct sigaction *new, struct sigaction *old)
{
	__asm__("int $0x80"
		: "=a" (sig)
		: "0" (__NR_sigaction), "b" (sig), "c" (new), "d" (old));
	if (sig >= 0)
		return 0;
	errno = -sig;
	return -1;
}


void
trap_signal(int signr, __sighandler_t func, int flags)
{
	static unsigned long sig_stack[4096];
	struct sigaction sa;

	sa.sa_handler = func;
	sa.sa_mask = 0;
	sa.sa_flags = flags;
#if 0
	sa.sa_restorer = (void (*)()) (((unsigned long)sig_stack + sizeof(sig_stack) - 4) & ~3);
#else
	sa.sa_restorer = (void (*)()) 0xbffffffc;
#endif
	sigaction(signr, &sa, NULL);
}


void
sig_segv(int nr, struct sigcontext_struct sc)
{
	unsigned char *laddr;
	unsigned short *stkladdr;

	/* Look up the linear address for the segment:offset of the
	 * eip and stack.
	 */
	laddr = (unsigned char *)(ldt[sc.cs >> 3].base + sc.eip);
	stkladdr = (unsigned short *)(ldt[sc.ss >> 3].base
			+ (sc.esp_at_signal & 0xffff));

	if (!in_emulator) {
		in_emulator = 1;

		/* Xenix 286 uses call 0x0:0x2b0 which causes a seg fault.
		 * We would perhaps be better with a call gate?
		 */
		if (laddr[-2] == 0xcd && laddr[-1] == 0x05) {
			x286syscall(&sc);
			in_emulator = 0;
			trap_signal(SIGSEGV, sig_segv, SA_ONESHOT);
			return;
		}

		in_emulator = 0;
	}

	fprintf(stderr, "x286emul: segv%s!\n", in_emulator ? " in emulator" : "");
#ifdef DEBUG
	if (__dbf) {
		fprintf(__dbf, "x286emul: segv%s!\n",
			in_emulator ? " in emulator" : "");
		dump_state(laddr, stkladdr, &sc);
	}
#endif
	exit(1);
}


static unsigned short
set_frame(unsigned long base, unsigned long offset, char *argv[], char *envp[])
{
	unsigned char *sp;
	char *ap, *ep;
	int envc, argc, i;

	sp = (unsigned char *)(base + offset);

	for (envc=0; envp[envc]; envc++) {
		sp -= (strlen(envp[envc])+2) & ~1;
#ifdef DEBUG
		fprintf(__dbf, "string: 0x%04lx (0x%08lx): \"%s\"\n",
			(unsigned long)(sp-base), sp, envp[envc]);
#endif
		strcpy(sp, envp[envc]);
	}
	ep = sp;

	for (argc=0; argv[argc]; argc++) {
		sp -= strlen(argv[argc])+1;
#ifdef DEBUG
		fprintf(__dbf, "string: 0x%04lx (0x%08lx): \"%s\"\n",
			(unsigned long)(sp-base), sp, argv[argc]);
#endif
		strcpy(sp, argv[argc]);
	}
	ap = sp;

	*(--sp) = 0;
	*(--sp) = 0;
#ifdef DEBUG
	fprintf(__dbf, "array:  0: 0x%04lx (0x%08lx) -> NULL\n",
		(unsigned long)(sp-base), sp);
#endif
	for (i=envc; i--;) {
		sp -= 2;
#ifdef DEBUG
		fprintf(__dbf, "array: %2d: 0x%04lx (0x%08lx) -> 0x%04lx (0x%08lx)\n",
			i+1,
			(unsigned long)(sp-base), sp,
			(unsigned long)(ep-base), ep);
#endif
		*((unsigned short *)sp) = (unsigned long)(ep - base);
		while (*ep++);
	}

	*(--sp) = 0;
	*(--sp) = 0;
#ifdef DEBUG
	fprintf(__dbf, "array:  0: 0x%04lx (0x%08lx) -> NULL\n",
		(unsigned long)(sp-base), sp);
#endif

	for (i=argc; i--;) {
		sp -= 2;
#ifdef DEBUG
		fprintf(__dbf, "array: %2d: 0x%04lx (0x%08lx) -> 0x%04lx (0x%08lx)\n",
			i+1,
			(unsigned long)(sp-base), sp,
			(unsigned long)(ap-base), ap);
#endif
		*((unsigned short *)sp) = (unsigned long)(ap - base);
		while (*ap++);
	}

	sp -= 2;
	*((unsigned short *)sp) = argc;

	return (unsigned long)(sp-base);
}


void
main(int argc, char *argv[], char *envp[])
{
	unsigned long ds_base;

#ifdef DEBUG
	char *p;

	if ((p = getenv("X286DEBUG")))
		if (!(__dbf = fopen(p, "w")))
			perror(p);

	if (__dbf) {
		setbuf(__dbf, 0);
		fprintf(__dbf, "Xenix 286 emulation: entry 0x%02x:0x%04lx, data 0x%02x, stack 0x%02x:0x%04x\n",
			init_cs, init_entry, init_ds, init_ds, limit_stk);
	}
#endif

	ldt_init();
	base_desc = init_ds >> 3;

	/* If the stack size isn't already specified in the header set
	 * it to the Xenix default of 0x1000. We need to know this at run
	 * time in order to implement the stkgro check.
	 */
	if (!(xexec->x_renv & XE_FS))
		xext->xe_stksize = 0x1000;

	ds_base = (unsigned long)ldt[base_desc].base;
	init_stk = set_frame(ds_base, 0x10000, argv, envp);

#ifdef DEBUG
	fprintf(__dbf, "stack addr = 0x%04lx\n", init_stk);
#endif

	trap_signal(SIGSEGV, sig_segv, SA_ONESHOT);

	in_emulator = 0;
	__asm__("jmp _x286boot\n");
}
