/* sysconf() function.
   Copyright 1995 Tristan Gingold
		  Written June 1995 by Tristan Gingold

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
#include <unistd.h>
#include <sys/sysconfig.h>

extern long chkr_sysconfig (int);
long chkr_sysconf (int arg);

long
chkr_sysconf (int arg)
{
  switch(arg)
    {
    case _SC_PAGESIZE:
      return chkr_sysconfig (_CONFIG_PAGESIZE);
    case _SC_OPEN_MAX:
      return chkr_sysconfig (_CONFIG_OPEN_FILES);
    default:
      return -1;
    }
}
