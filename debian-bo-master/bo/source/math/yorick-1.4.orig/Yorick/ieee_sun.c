/*
   IEEE_SUN.C
   Hack for Sun/Solaris with gcc, but no sunmath library.
   I don't really understand this.  It fails with -O.
   Compile separately and place in libyor.a.
   If you fix it, let me know.

   $Id$
 */

extern void nonstandard_arithmetic(void);
extern int ieee_handler(char *action, char *exception, void (*hdl)(int));

static unsigned int sparc_get_fsr(void);
static void sparc_set_fsr(unsigned int fsr);

/* ------------------------------------------------------------------------ */

void nonstandard_arithmetic(void)
{
  /* just set the fsr flags */
  sparc_set_fsr(sparc_get_fsr() | 0x400000);
}

int ieee_handler(char *action, char *exception, void (*hdl)(int))
{
  /* just set the fsr flags */
  unsigned int fsr= sparc_get_fsr();
  fsr&= ~0xf800000;
  sparc_set_fsr(fsr | 0xd000000);
  return 0;
}

/* ------------------------------------------------------------------------ */

static unsigned int sparc_get_fsr(void)
{
  unsigned int fsr;
  asm("st  %%fsr, %0" : "=Q" (fsr));
  return fsr;
}

static void sparc_set_fsr(unsigned int fsr)
{
  asm("ld  %0, %%fsr" : : "Q" (fsr));
}

/* ------------------------------------------------------------------------ */
