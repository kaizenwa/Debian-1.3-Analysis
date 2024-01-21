/* Checker stubs for functions defined in ioctl.h
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

#ifdef HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#include <termio.h>
#include "checker_api.h"

static void
chkr_check_struct_termio (struct termio *buf, int right)
{
  chkr_check_addr (&(buf->c_iflag), sizeof (unsigned short), right);
  chkr_check_addr (&(buf->c_oflag), sizeof (unsigned short), right);
  chkr_check_addr (&(buf->c_cflag), sizeof (unsigned short), right);
  chkr_check_addr (&(buf->c_lflag), sizeof (unsigned short), right);
  chkr_check_addr (&(buf->c_line), sizeof (unsigned char), right);
  chkr_check_addr (&(buf->c_cc), NCC, right);
}
  
int
chkr$ioctl (int fd, int req, char *arg)
{
  fd_used_by_prog (fd);

  switch (req)
    {
  case TIOCGWINSZ:
      chkr_check_addr ((PTR) arg, sizeof (struct winsize), CHKR_WO);
      break;
  case TIOCSWINSZ:
      chkr_check_addr ((PTR) arg, sizeof (struct winsize), CHKR_RO);
      break;
#ifdef FIONREAD
  case FIONREAD:
      chkr_check_addr ((PTR) arg, sizeof (int), CHKR_WO);
      break;
#endif /* FIONREAD */      
  case TCGETA:
      chkr_check_struct_termio ((struct termio *) arg, CHKR_WO);
      break;
  case TCSETA:
      chkr_check_struct_termio ((struct termio *) arg, CHKR_RO);
      break;
  default:
      chkr_printf ("Ioctl 0x%x not yet implemented.\n", req);
    }
  return ioctl (fd, req, arg);
}

#endif /* HAVE_SYS_IOCTL_H */
