/* Checker stubs for functions defined in sys/uio.h
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

#ifdef HAVE_SYS_UIO_H
#include <sys/types.h>
#include <sys/uio.h>
#include "checker_api.h"

#if 0
#define HAVE_readv
#define HAVE_writev
#endif

/* compiled from: . */
#ifdef HAVE_readv
/* From `/usr/include/sys/uio.h:50'.  */
int
chkr$readv (int fd, const struct iovec *iov, size_t iovcnt)
{
  int res;
  int i;
  fd_used_by_prog (fd);
  stubs_chkr_check_addr (iov, iovcnt * sizeof (struct iovec), CHKR_RO, "iov");
  res = readv (fd, iov, iovcnt);
  if (res > 0)
    for (i = 0; i < iovcnt && res > 0; i++)
      {
        stubs_chkr_set_right (iov[i].iov_base, res < iov[i].iov_len ? res : iov[i].iov_len, CHKR_RW);
        res -= iov[i].iov_len;
      }
  return res;
}
#endif /* HAVE_readv */

#ifdef HAVE_writev
/* From `/usr/include/sys/uio.h:57'.  */
int
chkr$writev (int fd, const struct iovec *iov, size_t iovcnt)
{
  fd_used_by_prog (fd);
  stubs_chkr_check_addr (iov, iovcnt * sizeof (struct iovec), CHKR_RO, "iov");
#if USE_BI_JUMP
  __builtin_jump (writev);
#else
  return writev (fd, iov, iovcnt);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_writev */

#endif /* HAVE_SYS_UIO_H */
