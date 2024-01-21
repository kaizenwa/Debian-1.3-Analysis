// $Id: acconfig.h.in,v 1.15 1997/04/25 06:38:30 zeller Exp $ -*- C++ -*-
// General configuration file.

// Copyright (C) 1995 Technische Universitaet Braunschweig, Germany.
// Written by Andreas Zeller <zeller@ips.cs.tu-bs.de>.
// 
// This file is part of the ICE Library.
// 
// The ICE Library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Library General Public
// License as published by the Free Software Foundation; either
// version 2 of the License, or (at your option) any later version.
// 
// The ICE Library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
// See the GNU Library General Public License for more details.
// 
// You should have received a copy of the GNU Library General Public
// License along with the ICE Library -- see the file COPYING.LIB.
// If not, write to the Free Software Foundation, Inc.,
// 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
// 
// ICE is the incremental configuration engine.
// Contact ice@ips.cs.tu-bs.de for details.

#ifndef _ICE_config_h
#define _ICE_config_h
// @TOP@

// Descriptive text for C preprocessor macros that autoconf may define.
// Leave the following blank line there!!  Autoheader needs it.


/* Define if your C++ compiler has ANSI-compliant lifetime of temporaries. */
#undef HAVE_ANSI_LIFETIME_OF_TEMPORARIES

/* Define if your C++ compiler supports explicit template instantiation. */
#undef HAVE_EXPLICIT_TEMPLATE_INSTANTIATION

/* Define if your C++ compiler supports overloaded operator new[]. */
#undef HAVE_ARRAY_OPERATOR_NEW

/* Define if you have the X Athena widgets. */
#undef HAVE_ATHENA_WIDGETS

/* Define if your C++ compiler supports bool types. */
#undef HAVE_BOOL

/* Define if <sys/core.h> defines CORE_MAGIC to identify core files. */
#undef HAVE_CORE_MAGIC

/* Define if your C++ library has an ostrstream::frozen() function. */
#undef HAVE_FROZEN_OSTRSTREAM

/* Define if your C++ compiler supports long long types. */
#undef HAVE_LONG_LONG

/* Define if MAXPATHLEN is defined or declared in <sys/param.h>. */
#undef HAVE_MAXPATHLEN

/* Define if you have XPM widgets. */
#undef HAVE_XPM_WIDGETS

/* Define if you have Motif widgets. */
#undef HAVE_MOTIF_WIDGETS

/* Define if your C++ compiler supports named return values. */
#undef HAVE_NAMED_RETURN_VALUES

/* Define if PATH_MAX is defined or declared in <limits.h>. */
#undef HAVE_PATH_MAX

/* Define if your C++ compiler supports placement new. */
#undef HAVE_PLACEMENT_NEW

/* Define if _POSIX_PATH_MAX is defined or declared in <limits.h>. */
#undef HAVE_POSIX_PATH_MAX

/* Define if your C++ compiler supports the __PRETTY_FUNCTION__ macro. */
#undef HAVE_PRETTY_FUNCTION

/* Define if the regex_t type has a n_subexps member, as in GNU RX 1.0 */
#undef HAVE_REGEX_T_N_SUBEXPS

/* Define if the regex_t type has a re_nsub member, according to POSIX.2 */
#undef HAVE_REGEX_T_RE_NSUB

/* Define to the signal handler type accepted by signal(). 
   See the signal man page and the decl in <signal.h> for the exact type.
   Typically values are int, ..., and int .... */
#undef SIGHANDLERARGS

/* Define if regcomp() from <regex.h> does not work. */
#undef REGCOMP_BROKEN

/* Define if you want ICE to use X Athena widgets. */
#undef WITH_ATHENA_WIDGETS

/* Define if you want to replace the ICE built-in 'test' command by
   the external test program. */
#undef WITH_EXTERNAL_TEST

/* Define if you want ICE to use Motif widgets. */
#undef WITH_MOTIF_WIDGETS


// @BOTTOM@

/* Define if asctime is declared in time.h. */
#undef HAVE_ASCTIME_DECL

/* Define if difftime is declared in time.h. */
#undef HAVE_DIFFTIME_DECL

/* Define if dup2 is declared in unistd.h. */
#undef HAVE_DUP2_DECL

/* Define if endpwent is declared in pwd.h. */
#undef HAVE_ENDPWENT_DECL

/* Define if fcntl is declared in fcntl.h. */
#undef HAVE_FCNTL_DECL

/* Define if ftime is declared in sys/timeb.h. */
#undef HAVE_FTIME_DECL

/* Define if gethostname is declared in unistd.h. */
#undef HAVE_GETHOSTNAME_DECL

/* Define if gethostbyname is declared in netdb.h. */
#undef HAVE_GETHOSTBYNAME_DECL

/* Define if getrusage is declared in sys/resource.h. */
#undef HAVE_GETRUSAGE_DECL

/* Define if getwd is declared in unistd.h. */
#undef HAVE_GETWD_DECL

/* Define if getpty is declared in unistd.h. */
#undef HAVE_GETPTY_DECL

/* Define if _getpty is declared in unistd.h. */
#undef HAVE__GETPTY_DECL

/* Define if grantpt is declared in stdlib.h. */
#undef HAVE_GRANTPT_DECL

/* Define if ioctl is declared in unistd.h. */
#undef HAVE_IOCTL_DECL

/* Define if lstat is declared in sys/stat.h. */
#undef HAVE_LSTAT_DECL

/* Define if malloc is declared in stdlib.h. */
#undef HAVE_MALLOC_DECL

/* Define if memcpy is declared in string.h. */
#undef HAVE_MEMCPY_DECL

/* Define if memset is declared in string.h. */
#undef HAVE_MEMSET_DECL

/* Define if popen is declared in stdio.h. */
#undef HAVE_POPEN_DECL

/* Define if pclose is declared in stdio.h. */
#undef HAVE_PCLOSE_DECL

/* Define if ptsname is declared in stdlib.h. */
#undef HAVE_PTSNAME_DECL

/* Define if raise is declared in signal.h. */
#undef HAVE_RAISE_DECL

/* Define if rand is declared in stdlib.h. */
#undef HAVE_RAND_DECL

/* Define if random is declared in stdlib.h. */
#undef HAVE_RANDOM_DECL

/* Define if readlink is declared in unistd.h. */
#undef HAVE_READLINK_DECL

/* Define if setbuf is declared in stdio.h. */
#undef HAVE_SETBUF_DECL

/* Define if setpgrp is declared in unistd.h. */
#undef HAVE_SETPGRP_DECL

/* Define if setpgrp2 is declared in unistd.h. */
#undef HAVE_SETPGRP2_DECL

/* Define if setpgid is declared in unistd.h. */
#undef HAVE_SETPGID_DECL

/* Define if setpwent is declared in pwd.h. */
#undef HAVE_SETPWENT_DECL

/* Define if setsid is declared in unistd.h. */
#undef HAVE_SETSID_DECL

/* Define if setvbuf is declared in stdio.h. */
#undef HAVE_SETVBUF_DECL

/* Define if sigblock is declared in signal.h. */
#undef HAVE_SIGBLOCK_DECL

/* Define if sigsetmask is declared in signal.h. */
#undef HAVE_SIGSETMASK_DECL

/* Define if sigprocmask is declared in signal.h. */
#undef HAVE_SIGPROCMASK_DECL

/* Define if sigsuspend is declared in signal.h. */
#undef HAVE_SIGSUSPEND_DECL

/* Define if srand is declared in stdlib.h. */
#undef HAVE_SRAND_DECL

/* Define if srandom is declared in stdlib.h. */
#undef HAVE_SRANDOM_DECL

/* Define if stat is declared in sys/stat.h. */
#undef HAVE_STAT_DECL

/* Define if strerror is declared in string.h. */
#undef HAVE_STRERROR_DECL

/* Define if strftime is declared in time.h. */
#undef HAVE_STRFTIME_DECL

/* Define if strsignal is declared in string.h. */
#undef HAVE_STRSIGNAL_DECL

/* Define if strtol is declared in stdlib.h. */
#undef HAVE_STRTOL_DECL

/* Define if strtod is declared in stdlib.h. */
#undef HAVE_STRTOD_DECL

/* Define if tcgetattr is declared in termios.h. */
#undef HAVE_TCGETATTR_DECL

/* Define if tcsetattr is declared in termios.h. */
#undef HAVE_TCSETATTR_DECL

/* Define if tcgetsid is declared in sys/termios.h. */
#undef HAVE_TCGETSID_DECL

/* Define if tcgetpgrp is declared in unistd.h. */
#undef HAVE_TCGETPGRP_DECL

/* Define if tcsetpgrp is declared in unistd.h. */
#undef HAVE_TCSETPGRP_DECL

/* Define if times is declared in sys/times.h. */
#undef HAVE_TIMES_DECL

/* Define if uname is declared in sys/utsname.h. */
#undef HAVE_UNAME_DECL

/* Define if unlink is declared in unistd.h. */
#undef HAVE_UNLINK_DECL

/* Define if unlockpt is declared in stdlib.h. */
#undef HAVE_UNLOCKPT_DECL

/* Define if vfork is declared in vfork.h unistd.h. */
#undef HAVE_VFORK_DECL

/* Define if waitpid is declared in sys/wait.h. */
#undef HAVE_WAITPID_DECL

#endif // _ICE_config_h
// DON'T ADD ANYTHING BEHIND THIS #endif
