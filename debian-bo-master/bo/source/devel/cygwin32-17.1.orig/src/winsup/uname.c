/* uname for WIN32.

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#include <string.h>
#include <stdio.h>
#include <sys/utsname.h>
#include <windows.h>
#include "syscalls.h"
#include "version.h"

int
uname (struct utsname *name)
{
  DWORD len, version;
  SYSTEM_INFO sysinfo;

  GetSystemInfo (&sysinfo);
  memset (name, 0, sizeof (*name));

  len = sizeof (name->nodename)-1;
  GetComputerNameA (name->nodename, &len);

  version = GetVersion ();

  if (version < 0x80000000)
    strcpy (name->sysname, "CYGWIN32/NT");
  else if ((version & 255) >= 4 )
    strcpy (name->sysname, "CYGWIN32/95");
  else
    strcpy (name->sysname, "CYGWIN32/32s");

  sprintf (name->release, "%d.%d", version & 255, (version >> 8) & 255);

  sprintf (name->version, "%d.%d",
	   CYGWIN_DLL_VERSION_MAJOR, CYGWIN_DLL_VERSION_MINOR);

#ifdef __PPC__
  strcpy (name->machine, "ppc");
#endif

#ifdef __i386__
  sprintf (name->machine, "i%d86", sysinfo.wProcessorLevel);
#endif

#if !defined(__PPC__) && !defined(__i386__)
  sprintf (name->machine, "unknown");
#endif

  return 0;
}
