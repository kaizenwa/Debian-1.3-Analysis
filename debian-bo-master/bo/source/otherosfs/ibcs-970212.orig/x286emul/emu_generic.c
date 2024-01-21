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
emu_i_sas(struct sigcontext_struct *sc)
{
	return lcall7(sc->eax & 0xffff,
		sc->ebx & 0xffff,
		ldt[sc->ds>>3].base + (sc->ecx & 0xffff),
		sc->esi & 0xffff);
}


int
emu_i_ass(struct sigcontext_struct *sc)
{
	return lcall7(sc->eax & 0xffff,
		ldt[sc->ds>>3].base + (sc->ebx & 0xffff),
		(sc->ecx & 0xffff),
		(sc->esi & 0xffff));
}


int
emu_i_s(struct sigcontext_struct *sc)
{
	return lcall7(sc->eax & 0xffff,
			sc->ebx & 0xffff);
}


int
emu_i_sls(struct sigcontext_struct *sc)
{
	return lcall7(sc->eax & 0xffff,
			sc->ebx & 0xffff,
			(sc->ecx & 0xffff) | (sc->esi << 16),
			(sc->edi & 0xffff));
}


int
emu_i_a(struct sigcontext_struct *sc)
{
	return lcall7(sc->eax & 0xffff,
			ldt[sc->ds>>3].base + (sc->ebx & 0xffff));
}


int
emu_i_aa(struct sigcontext_struct *sc)
{
	return lcall7(sc->eax & 0xffff,
			ldt[sc->ds>>3].base + (sc->ebx & 0xffff),
			ldt[sc->ds>>3].base + (sc->ecx & 0xffff));
}


int
emu_i_ssa(struct sigcontext_struct *sc)
{
	return lcall7(sc->eax & 0xffff,
		(sc->ebx & 0xffff),
		(sc->ecx & 0xffff),
		ldt[sc->ds>>3].base + (sc->esi & 0xffff));
}


int
emu_i_v(struct sigcontext_struct *sc)
{
	return lcall7(sc->eax & 0xffff);
}
