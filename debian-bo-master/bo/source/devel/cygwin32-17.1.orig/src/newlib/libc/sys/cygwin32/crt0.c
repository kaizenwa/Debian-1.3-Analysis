
#ifdef __PPC__
/* For the PowerPC, we want to make this function have its structured
   exception table exception function point to something we control.  */

extern void __cygwin_exception_handler();
extern void mainCRTStartup(void) __attribute__((__exception__(__cygwin_exception_handler)));
#endif

#ifdef __i386__
/* For debugging on *#!$@ windbg.  bp for breakpoint.  */
int __cygwin_crt0_bp = 0;
#endif

void
mainCRTStartup ()
{
#ifdef __i386__
  if (__cygwin_crt0_bp)
    asm volatile ("int3");
#endif

  cygwin_crt0 ();
}
