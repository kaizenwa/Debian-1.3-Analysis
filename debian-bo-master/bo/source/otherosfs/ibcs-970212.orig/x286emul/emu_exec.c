#include <errno.h>
#include <signal.h>
#include <stdio.h>

#include "ldt.h"
#include "syscall.h"
#include "lcall7.h"

#ifdef DEBUG
# include "debug.h"
#endif


int
emu_exec(struct sigcontext_struct *sc)
{
	unsigned short *p;
	int argc, envc;
	char **q;

	sc->ebx = ldt[sc->ds>>3].base + (sc->ebx & 0xffff);	/* pgm */
	sc->ecx = ldt[sc->ds>>3].base + (sc->ecx & 0xffff);	/* arg */
	sc->esi = ldt[sc->ds>>3].base + (sc->esi & 0xffff);	/* env */

#ifdef DEBUG
	if (__dbf)
		fprintf(__dbf, "x286emul:   exec: program = \"%s\"\n", sc->ebx);
#endif

	for (argc=0,p=(unsigned short *)sc->ecx; p && *p; p++,argc++) {
#ifdef DEBUG
		if (__dbf)
			fprintf(__dbf, "x286emul:         arg %d: \"%s\"\n",
				argc, ldt[sc->ds>>3].base + *p);
#endif
	}

	for (envc=0,p=(unsigned short *)sc->esi; p && *p; p++,envc++) {
#ifdef DEBUG
		if (__dbf)
			fprintf(__dbf, "x286emul:         env %d: \"%s\"\n",
				envc, ldt[sc->ds>>3].base + *p);
#endif
	}

	if (!(q = alloca(sizeof(char *) * (argc + envc + 2)))) {
		errno = ENOMEM;
		return -1;
	}

	p = (unsigned short *)sc->ecx;
	sc->ecx = (int)q;
	for (; p && *p; p++)
		*(q++) = (char *)(ldt[sc->ds>>3].base + *p);
	*(q++) = (char *)0;

	p = (unsigned short *)sc->esi;
	sc->esi = (int)q;
	for (; p && *p; p++)
		*(q++) = (char *)(ldt[sc->ds>>3].base + *p);
	*(q++) = (char *)0;

	return lcall7(sc->eax & 0xffff, sc->ebx, sc->ecx, sc->esi);
}
