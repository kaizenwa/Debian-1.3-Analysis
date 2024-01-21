/* Checker stubs for functions defined in termios.h
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

#ifdef HAVE_TERMIOS_H
#include <termios.h>
#include "checker_api.h"

#undef HAVE_cfgetispeed
#undef HAVE_cfgetospeed
#undef HAVE_cfmakeraw
#undef HAVE_cfsetispeed
#undef HAVE_cfsetospeed

#if 0
#define HAVE_tcgetattr
#define HAVE_tcsetattr
#endif

#ifdef HAVE_tcgetattr
static void
stubs_chkr_set_right_termios (struct termios *t)
{
  stubs_chkr_set_right (&(t->c_iflag), sizeof (tcflag_t), CHKR_RW);
  stubs_chkr_set_right (&(t->c_oflag), sizeof (tcflag_t), CHKR_RW);
  stubs_chkr_set_right (&(t->c_cflag), sizeof (tcflag_t), CHKR_RW);
  stubs_chkr_set_right (&(t->c_lflag), sizeof (tcflag_t), CHKR_RW);
  stubs_chkr_set_right (&(t->c_cc), NCCS * sizeof (cc_t), CHKR_RW);
}
#endif

#ifdef HAVE_tcsetattr
void
stubs_chkr_check_termios (struct termios *t, int right)
{
  stubs_chkr_check_addr (&(t->c_iflag), sizeof (tcflag_t), right, "t->c_iflag");
  stubs_chkr_check_addr (&(t->c_oflag), sizeof (tcflag_t), right, "t->c_oflag");
  stubs_chkr_check_addr (&(t->c_cflag), sizeof (tcflag_t), right, "t->c_cflag");
  stubs_chkr_check_addr (&(t->c_lflag), sizeof (tcflag_t), right, "t->c_lflag");
  stubs_chkr_check_addr (&(t->c_cc), NCCS * sizeof (cc_t), right, "t->c_cc");
}
#else
void stubs_chkr_check_termios (struct termios *t, int right);
#endif
  
/* compiled from: . */
#ifdef HAVE_cfgetispeed
/* From `/usr/include/termios.h:33'.  */
speed_t
chkr$cfgetispeed (struct termios * arg0)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (struct termios), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (cfgetispeed);
#else
  {
    speed_t res;
    res = cfgetispeed (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_cfgetispeed */

#ifdef HAVE_cfgetospeed
/* From `/usr/include/termios.h:36'.  */
speed_t
chkr$cfgetospeed (struct termios * arg0)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (struct termios), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (cfgetospeed);
#else
  {
    speed_t res;
    res = cfgetospeed (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_cfgetospeed */

#ifdef HAVE_cfmakeraw
/* From `/usr/include/termios.h:40'.  */
void
chkr$cfmakeraw (struct termios * arg0)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (struct termios), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (cfmakeraw);
#else
  cfmakeraw (arg0);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_cfmakeraw */

#ifdef HAVE_cfsetispeed
/* From `/usr/include/termios.h:44'.  */
int
chkr$cfsetispeed (struct termios * arg0, speed_t arg1)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (struct termios), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (cfsetispeed);
#else
  {
    int res;
    res = cfsetispeed (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_cfsetispeed */

#ifdef HAVE_cfsetospeed
/* From `/usr/include/termios.h:48'.  */
int
chkr$cfsetospeed (struct termios * arg0, speed_t arg1)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (struct termios), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (cfsetospeed);
#else
  {
    int res;
    res = cfsetospeed (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_cfsetospeed */

#ifdef HAVE_tcdrain
/* From `/usr/include/termios.h:52'.  */
int
chkr$tcdrain (int arg0)
{
#if USE_BI_JUMP
  __builtin_jump (tcdrain);
#else
  {
    int res;
    res = tcdrain (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_tcdrain */

#ifdef HAVE_tcflow
/* From `/usr/include/termios.h:56'.  */
int
chkr$tcflow (int arg0, int arg1)
{
#if USE_BI_JUMP
  __builtin_jump (tcflow);
#else
  {
    int res;
    res = tcflow (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_tcflow */

#ifdef HAVE_tcflush
/* From `/usr/include/termios.h:60'.  */
int
chkr$tcflush (int arg0, int arg1)
{
#if USE_BI_JUMP
  __builtin_jump (tcflush);
#else
  {
    int res;
    res = tcflush (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_tcflush */

#ifdef HAVE_tcgetattr
/* From `/usr/include/termios.h:66'.  */
int
chkr$tcgetattr (int fd, struct termios *t)
{
  int res;
  fd_used_by_prog (fd);
  stubs_chkr_check_termios (t, CHKR_MW);
  res = tcgetattr (fd, t);
  if (res == 0)
    stubs_chkr_set_right_termios (t);
  return res;
}
#endif /* HAVE_tcgetattr */

#ifdef HAVE_tcsendbreak
/* From `/usr/include/termios.h:70'.  */
int
chkr$tcsendbreak (int arg0, int arg1)
{
#if USE_BI_JUMP
  __builtin_jump (tcsendbreak);
#else
  {
    int res;
    res = tcsendbreak (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_tcsendbreak */

#ifdef HAVE_tcsetattr
/* From `/usr/include/termios.h:75'.  */
int
chkr$tcsetattr (int fd, int oa, struct termios *t)
{
  fd_used_by_prog (fd);
  stubs_chkr_check_termios (t, CHKR_RO);
#if USE_BI_JUMP
  __builtin_jump (tcsetattr);
#else
  return tcsetattr (fd, oa, t);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_tcsetattr */

#endif /* HAVE_TERMIOS_H */
