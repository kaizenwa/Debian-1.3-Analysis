/* crt0 for libc [newlib calls this one]

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NxOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
*/

#include "winsup.h"
#include "version.h"
#include <reent.h>

/* For fork */
#ifdef _POWER
extern char __data_start__;
extern char __data_end__;
extern char __bss_start__;
extern char __bss_end__;
#else
extern char _data_start__;
extern char _data_end__;
extern char _bss_start__;
extern char _bss_end__;
#endif

extern "C" 
{
  char **environ;
  void cygwin_crt0();
  extern void (*__CTOR_LIST__)(void);
  extern void (*__DTOR_LIST__)(void);
  int main (int, char **, char **);
  struct _reent *_impure_ptr;
  int _fmode;
};

static per_process cygwin_statu;

/* Set up pointers to various pieces so the dll can then use them,
   and then jump to the dll.  */

void cygwin_crt0()
{
  /* This is used to record what the initial sp was.  The value is needed
     when copying the parent's stack to the child during a fork.  */
  int onstack;

  /* The version numbers are the main source of compatibility checking.
     As a backup to them, we use the size of the per_process struct.  */
  cygwin_statu.magic_biscuit = sizeof (per_process);

  /* cygwin.dll version number in effect at the time the app was created.  */
  cygwin_statu.version_major = CYGWIN_DLL_VERSION_MAJOR;
  cygwin_statu.version_minor = CYGWIN_DLL_VERSION_MINOR;

  cygwin_statu.ctors = &__CTOR_LIST__;
  cygwin_statu.dtors = &__DTOR_LIST__;
  cygwin_statu.envptr = &environ;
  cygwin_statu.impure_ptr_ptr = &_impure_ptr;
  cygwin_statu.main = &main;
  cygwin_statu.fmode_ptr = &_fmode;
  cygwin_statu.initial_sp = (char *) &onstack;

  /* Remember whatever the user linked his application with - or
     point to entries in the dll.  */
  cygwin_statu.malloc = &malloc; 
  cygwin_statu.free = &free;
  cygwin_statu.realloc = &realloc;

  /* variables for fork */
#ifdef _POWER
  cygwin_statu.data_start = &__data_start__;
  cygwin_statu.data_end = &__data_end__;
  cygwin_statu.bss_start = &__bss_start__;
  cygwin_statu.bss_end = &__bss_end__;
#else
  cygwin_statu.data_start = &_data_start__;
  cygwin_statu.data_end = &_data_end__;
  cygwin_statu.bss_start = &_bss_start__;
  cygwin_statu.bss_end = &_bss_end__;
#endif

  /* Jump into the dll.  */
  dll_crt0 (&cygwin_statu);
}

/* This is needed to terminate the list of inport stuff */
/* FIXME: Doesn't the ppc port handle this differently?  Standardize.  */
asm (".section .idata$3\n" ".long 0,0,0,0,0,0,0,0");

