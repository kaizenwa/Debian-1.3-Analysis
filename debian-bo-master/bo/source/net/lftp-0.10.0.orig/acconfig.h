/*
 * lftp and utils
 *
 * Copyright (c) 1996-1997 by Alexander V. Lukyanov (lav@yars.free.net)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Library General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#ifndef CONFIG_H
#define CONFIG_H
@TOP@

/* the version of package */
#undef VERSION

/* Define if your system defines TIOCGWINSZ in sys/ioctl.h.  */
#undef GWINSZ_IN_SYS_IOCTL

#undef HAVE_GETPW_DECLS

#undef NO_SYS_FILE

#undef HAVE_BSD_SIGNALS

#undef HAVE_POSIX_SIGNALS

#undef HAVE_USG_SIGHOLD

@BOTTOM@

#if RETSIGTYPE == void
#define VOID_SIGHANDLER
#endif

#ifndef HAVE_STRERROR
#define strerror(e)  (sys_errlist[e])
#endif

#ifndef HAVE_LSTAT
#define lstat stat
#endif

#endif /* CONFIG_H */
