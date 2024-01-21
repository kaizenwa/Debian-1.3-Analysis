/* Checker stubs for functions defined in sys/utsname.h
   Copyright 1995, 1996 Tristan Gingold
		  Written December 1995 by Tristan Gingold

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License 
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

 The author may be reached by US/French mail:
		Tristan Gingold 
		8 rue Parmentier
		F-91120 PALAISEAU
		FRANCE
*/
#include "available-stubs.h"

#ifdef HAVE_SYS_UTSNAME_H
#include <sys/utsname.h>
#include "checker_api.h"

#if 0
#define HAVE_uname
#endif
                                                                                                                                                          
/* compiled from: . */
#ifdef HAVE_uname
/* From `/usr/include/sys/utsname.h:19'.  */
int
chkr$uname (struct utsname *name)
{
  int res;
  stubs_chkr_check_addr (&(name->sysname), sizeof (name->sysname), CHKR_MW, "name->sysname");
  stubs_chkr_check_addr (&(name->nodename), sizeof (name->nodename), CHKR_MW, "name->nodename");
  stubs_chkr_check_addr (&(name->release), sizeof (name->release), CHKR_MW, "name->release");
  stubs_chkr_check_addr (&(name->version), sizeof (name->version), CHKR_MW, "name->version");
  stubs_chkr_check_addr (&(name->machine), sizeof (name->machine), CHKR_MW, "name->machine");
#ifdef HAVE_UTS_DOMAINNAME
  stubs_chkr_check_addr (&(name->domainname), sizeof (name->domainname), CHKR_MW, "name->domainname");
#endif
  res = uname (name);
  if (res == 0)
    {
      stubs_chkr_set_right (&(name->sysname), strlen (name->sysname) + 1, CHKR_RW);
      stubs_chkr_set_right (&(name->nodename), strlen (name->nodename) + 1, CHKR_RW);
      stubs_chkr_set_right (&(name->release), strlen (name->release) + 1, CHKR_RW);
      stubs_chkr_set_right (&(name->version), strlen (name->version) + 1, CHKR_RW);
      stubs_chkr_set_right (&(name->machine), strlen (name->machine) + 1, CHKR_RW);
#ifdef HAVE_UTS_DOMAINNAME
      stubs_chkr_set_right (&(name->domainname), strlen (name->domainname) + 1, CHKR_RW);
#endif
    }
  return res;
}
#endif /* HAVE_uname */

#endif /* HAVE_SYS_UTSNAME_H */
