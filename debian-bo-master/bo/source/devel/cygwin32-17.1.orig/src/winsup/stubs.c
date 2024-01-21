/* Miscellaneous stubs for functions that haven't really been
   implemented yet.  

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#define MARK() mark(__FILE__,__LINE__)
#define small_printf small_printf__FPCce
#include <stdio.h>
#include <unistd.h>

int
endgrent ()
{
  printf ("endgrent () called, and it's not done yet.\n");
  return 2;
}

int
mknod ()
{
  printf ("mknod () called, and it's not done yet.\n");
  return -1;
}

int
sync ()
{
  printf ("sync () called, and it's not done yet.\n");
  return 0;
}

int
getpass ()
{
  printf ("getpass called, and it's not done yet.\n");
  return 0;
}

int
crypt ()
{
  printf ("crypt called, and it's not done yet.\n");
  return 0;
}

int
setgid (gid_t a)
{
  printf ("setgid called, and it's not done yet.\n");
  return 0;
}

int
setuid (uid_t b)
{
  printf ("setuid called, and it's not done yet.\n");
  return 0;
}

int
setgrent ()
{
  printf ("setgrent called, and it's not done yet.\n");
  return 0;
}

int
setpgrp ()
{
  printf ("Setpgrp called, and it's not done yet.\n");
  return 0;
}

int
getpgrp ()

{
  printf ("Getpgrp called, and it's not done yet.\n");
  return 0;
}

int
getgrent ()
{
  printf ("Getgrent called, and it's not done yet.\n");
  return 0;
}


#if _PPC_
__chkstk () { printf ("alloca! called\n"); return 0;}

_alloca () { printf ("alloca! called\n"); return 0;}

asm (".global dll_entry@12");
asm ("dll_entry@12:");
#endif




