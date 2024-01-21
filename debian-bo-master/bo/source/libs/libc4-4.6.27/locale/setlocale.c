/* Copyright (C) 1991, 1992, 1993 Free Software Foundation, Inc.
This file is part of the GNU C Library.

The GNU C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU C Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

#include <ansidecl.h>
#include <localeinfo.h>
#include <errno.h>
#include <locale.h>
#include <string.h>
#include <stdlib.h>

#if defined(__linux__)
#include <ctype.h>
#include <fcntl.h>
#include <unistd.h>
#include <paths.h>

extern CONST unsigned short int __ctype_b_C[];
extern CONST unsigned char __ctype_tolower_C[];
extern CONST unsigned char __ctype_toupper_C[];
extern CONST struct ctype_info __ctype_C;

extern CONST struct collate_info __collate_C;
extern CONST struct monetary_info __monetary_C;
extern CONST struct numeric_info __numeric_C;
extern CONST struct response_info __response_C;
extern CONST struct time_info __time_C;

extern CONST unsigned short int __ctype_b_ISO_8859_1[];
extern CONST unsigned char __ctype_tolower_ISO_8859_1[];
extern CONST unsigned char __ctype_toupper_ISO_8859_1[];
extern CONST struct ctype_info __ctype_ISO_8859_1;

extern CONST unsigned short int __ctype_b_KOI_8[];
extern CONST unsigned char __ctype_tolower_KOI_8[];
extern CONST unsigned char __ctype_toupper_KOI_8[];
extern CONST struct ctype_info __ctype_KOI_8;

static int loc_open __P ((CONST unsigned char *, CONST unsigned char *));
static int guard_check __P ((CONST int, CONST unsigned char *));
static int loc_rdline __P ((CONST int, unsigned char *, CONST int, unsigned char **[]));

static int loc_collate __P ((CONST int, CONST unsigned char *));
static int loc_ctype __P ((CONST int, CONST unsigned char *));
static int loc_monetary __P ((CONST int, CONST unsigned char *));
static int loc_numeric __P ((CONST int, CONST unsigned char *));
static int loc_time __P ((CONST int, CONST unsigned char *));
static int loc_response __P ((CONST int, CONST unsigned char *));

#endif /* __linux__ */


/* Switch to the locale called NAME in CATEGORY.
   Return a string describing the locale.  This string can
   be used as the NAME argument in a later call.
   If NAME is NULL, don't switch locales, but return the current one.
   If NAME is "", switch to a locale based on the environment variables,
   as per POSIX.  Return NULL on error.  */
char *
DEFUN (setlocale, (category, name), int category AND CONST char *name)
{
#if defined(__linux__)
    /* Don't ask me why I did this. H.J. */

    /* Fixed up setlocale() by Mitch (m.dsouza@mrc-apu.cam.ac.uk) */
    /* Nickolay Saukh <nms@ussr.EU.net> was here */
    /* as was Steve Robbins <steve@nyongwa.montreal.qc.ca> */

    static struct message_struct {
	int category;
	CONST char *name;
	char *locale;
	char *saved_locale;
    } ms[] = {
	{ LC_COLLATE, "LC_COLLATE", "C" },
	{ LC_CTYPE, "LC_CTYPE", "C" },
	{ LC_MONETARY, "LC_MONETARY", "C" },
	{ LC_NUMERIC, "LC_NUMERIC", "C" },
	{ LC_TIME, "LC_TIME", "C" },
	{ LC_RESPONSE, "LC_RESPONSE", "C" },
	{ LC_MESSAGES, "LC_MESSAGES", "C" },
	/*
	 * New categories go in between here.
	 */
	{ LC_ALL, "LC_ALL", "C" },
    };

#define SIZE_MS (sizeof (ms) / sizeof (struct message_struct))

    int i, j, len;
    char *ptr;

    if (name == NULL) {		/* We just asking for current settings */
	for (i = 0; i < SIZE_MS; i++) {
	    if (category == ms[i].category)
	      return (char *) ms[i].locale;
	}
	errno = EINVAL;
	return NULL;		/* Not a valid category */
    }

    /*
     * What the category asked (excluding LC_ALL)?
     */
    if (category != LC_ALL) {
	for (i = 0; i < SIZE_MS - 1; i++) {
	    if (category != ms[i].category)
	      continue;
	    
	  /* If "" is given as the locale then we check environment vars */
	  if (*name == '\0') {
	      if ((ptr = getenv ("LC_ALL")) == NULL &&
		  (ptr = getenv (ms[i].name)) == NULL &&
		  (ptr = getenv ("LANG")) == NULL) {
		  /*
		   * Can't find a relevant variable; POSIX says the result
		   * is implementation-dependent.  Let's just return the
		   * previous setting, so that setlocale (LC_ALL, "") doesn't
		   * always fail.
		   */
		  return (char *) ms[i].locale;
	      }
	  } else
	    ptr = (char *)name;

	    if (!strcmp (ptr, ms[i].locale))
	      return (char *) ms[i].locale;

#define C_LOCALE (!strcmp (ptr, "C") || !strcmp (ptr, "POSIX"))

	    switch (category) {
		int fd;

	      case LC_COLLATE:
		if (C_LOCALE) {
		    _collate_info = &__collate_C;
		} else if ((fd = loc_open (ptr, ms[i].name)) < 0 ||
			   !loc_collate (fd, ptr)) {
		    errno = ENOENT;
		    return NULL;	/* Unknown locale */
		}
		break;
	      case LC_CTYPE:
		if (C_LOCALE) {
		    __ctype_b = __ctype_b_C + 1;
		    __ctype_tolower = __ctype_tolower_C + 1;
		    __ctype_toupper = __ctype_toupper_C + 1;
		    _ctype_info = &__ctype_C;
		} else if (!strcmp (ptr, "ISO-8859-1")) {
		    __ctype_b = __ctype_b_ISO_8859_1 + 1;
		    __ctype_tolower = __ctype_tolower_ISO_8859_1 + 1;
		    __ctype_toupper = __ctype_toupper_ISO_8859_1 + 1;
		    _ctype_info = &__ctype_ISO_8859_1;
		} else if (!strcmp (ptr, "koi8-r")) {
		    __ctype_b = __ctype_b_KOI_8 + 1;
		    __ctype_tolower = __ctype_tolower_KOI_8 + 1;
		    __ctype_toupper = __ctype_toupper_KOI_8 + 1;
		    _ctype_info = &__ctype_KOI_8;
		} else if ((fd = loc_open (ptr, ms[i].name)) < 0 ||
			   !loc_ctype (fd, ptr)) {
		    errno = ENOENT;
		    return NULL;	/* Unknown locale */
		}
		break;
	      case LC_MONETARY:
		if (C_LOCALE) {
		    _monetary_info = &__monetary_C;
		} else if ((fd = loc_open (ptr, ms[i].name)) < 0 ||
			   !loc_monetary (fd, ptr)) {
		    errno = ENOENT;
		    return NULL;	/* Unknown locale */
		}
		break;
	      case LC_NUMERIC:
		if (C_LOCALE) {
		    _numeric_info = &__numeric_C;
		} else if ((fd = loc_open (ptr, ms[i].name)) < 0 ||
			   !loc_numeric (fd, ptr)) {
		    errno = ENOENT;
		    return NULL;	/* Unknown locale */
		}
		break;
	      case LC_TIME:
		if (C_LOCALE) {
		    _time_info = &__time_C;
		} else if ((fd = loc_open (ptr, ms[i].name)) < 0 ||
			   !loc_time (fd, ptr)) {
		    errno = ENOENT;
		    return NULL;	/* Unknown locale */
		}
		break;
	      case LC_RESPONSE:
		if (C_LOCALE) {
		    _response_info = &__response_C;
		} else if ((fd = loc_open (ptr, ms[i].name)) < 0 ||
			   !loc_response (fd, ptr)) {
		    errno = ENOENT;
		    return NULL;	/* Unknown locale */
		}
		break;
	      case LC_MESSAGES:
		if (ptr == NULL) {
		    errno = ENOENT;
		    return NULL;	/* Unknown locale */
		}
		
		/* Some interaction with nls required to
		 * ensure correct settings
		 */
		break;
	      default:
		/*
		 * Invalid category requested
		 */
		errno = EINVAL;
		return NULL;
	    }

	    if ((ms[i].locale = malloc (strlen (ptr) + 1)) == NULL) {
		errno = ENOMEM;
		return NULL;
	    }
	    strcpy (ms[i].locale, ptr);
	    return (ms[i].locale);
	}
    }
    
    /*
     * If we here, then LC_ALL was requested
     * Save total length of all locale strings in 'len'
     */

    len = 0;
    for (i = 0; i < SIZE_MS - 1; i++) {
	/* Save current setting */
	ms[i].saved_locale = setlocale (ms[i].category, NULL);
	
	if ((ptr = setlocale (ms[i].category, name)) == NULL) {
	    /* Oops! Recover original state */
	    int se = errno;
	    
	    for (j = 0; j < i; j++)
	      (void) setlocale (ms[j].category, ms[j].saved_locale);
	    
	    errno = se;
	    return NULL;
	}
	len += strlen (ptr) + 1; /* include one for the slash */
    }
    
    /* if we here, then all individual locales were installed */
    if ((ms[i].locale = malloc (len + 1)) == NULL) {
	errno = ENOMEM;
	return NULL;
    }
    *(ms[i].locale) = 0;
    for (j = 0; j < SIZE_MS - 1; ++j) {
	strcat (ms[i].locale, ms[j].locale);
	strcat (ms[i].locale, "/");
    }
    return (char *) ms[i].locale;
    
#else

  /* Braindead implementation until I finish the fancy one.  */

  if (name == NULL || name[0] == '\0')
      return (char *) "C";

  if (!strcmp (name, "C") || !strcmp (name, "POSIX"))
    return (char *) name;

  errno = EINVAL;
  return NULL;
#endif
}

#if defined(__linux__) && defined(USE_ISO_8859_1)

static int
DEFUN (loc_open, (loc_name, cat_name), CONST unsigned char *loc_name AND CONST unsigned char *cat_name)
{
  unsigned char buffer[128];

  (void) strcpy (buffer, _PATH_LOCALE);
  (void) strcat (buffer, "/");
  (void) strcat (buffer, loc_name);
  (void) strcat (buffer, "/");
  (void) strcat (buffer, cat_name);

  return open (buffer, O_RDONLY);
}

static int
DEFUN (guard_check, (fd, cname), CONST int fd AND CONST unsigned char *cname)
{
#ifndef NOGUARD
  unsigned char fcname[256];

  /*
   * Guard check (overkill?)
   */

  fcname[sizeof (fcname) - 1] = '\0';
  if (read (fd, fcname, sizeof (fcname) - 1) < strlen (cname) + 1)
    {
      (void) close (fd);
      return 0;
    }
  else if (strcmp (cname, fcname))
    {
      (void) close (fd);
      return 0;
    }

#endif

  return 1;
}

static int
DEFUN (loc_collate, (fd, cname), CONST int fd AND CONST unsigned char *cname)
{
  static unsigned char values[UCHAR_MAX + 1];
  static unsigned char offsets[UCHAR_MAX + 1];
  static struct collate_info cinfo =
  {
    0,
    NULL,
    values,
    offsets
  };

  if (read (fd, values, sizeof (values)) != sizeof (values))
    {
      (void) close (fd);
      return 0;
    }
  if (read (fd, offsets, sizeof (offsets)) != sizeof (offsets))
    {
      (void) close (fd);
      return 0;
    }
  if (read (fd, &cinfo.nsubsts, sizeof (cinfo.nsubsts)) != sizeof (cinfo.nsubsts))
    {
      (void) close (fd);
      return 0;
    }
  /*
   * XXX: for a while a did not like substitutions
   */
  if (cinfo.nsubsts)
    {
      (void) close (fd);
      return 0;
    }

  if (!guard_check (fd, cname))
    return 0;

  (void) close (fd);

  _collate_info = &cinfo;

  return 1;
}

static int
DEFUN (loc_ctype, (fd, cname), CONST int fd AND CONST unsigned char *cname)
{
  /*
   * There is no urgent need for multibytes
   */
  extern CONST struct ctype_mbchar_info __ctype_mbchar_C;
  static struct ctables
    {
      unsigned short int xtype[UCHAR_MAX + 2];
      unsigned char xlower[UCHAR_MAX + 2];
      unsigned char xupper[UCHAR_MAX + 2];
    }
  ctables;
  static struct ctype_ctype_info c2info =
  {
    ctables.xtype,
    ctables.xlower,
    ctables.xupper
  };
  static CONST struct ctype_info cinfo =
  {
    &c2info,
    (struct ctype_mbchar_info *) &__ctype_mbchar_C	/* No multibytes!!! */
  };

  if (read (fd, &ctables, sizeof (ctables)) != sizeof (ctables))
    {
      (void) close (fd);
      return 0;
    }

  if (!guard_check (fd, cname))
    return 0;

  (void) close (fd);

  __ctype_b = ctables.xtype + 1;
  __ctype_tolower = ctables.xlower + 1;
  __ctype_toupper = ctables.xupper + 1;

  _ctype_info = &cinfo;

  return 1;
}

static int
DEFUN (loc_rdline, (fd, buf, buflen, target), CONST int fd AND unsigned char *buf AND CONST int buflen AND unsigned char **target[])
{
  int i;
  short len;
  unsigned char *cp = buf;

  for (i = 0; target[i] != NULL; i++)
    {
      if (read (fd, &len, sizeof (len)) != sizeof (len) ||
	  len > buflen - (cp - buf) ||
	  read (fd, cp, len) != len ||
	  *(cp + len - 1) != '\0')
	{
	  (void) close (fd);
	  return 0;
	}
      *target[i] = cp;
      cp += len;
    }
  return 1;
}

static int
DEFUN (loc_monetary, (fd, cname), CONST int fd AND CONST unsigned char *cname)
{
  static struct monetary_info minfo;
  static unsigned char **lines[] =
  {
    (unsigned char **) &minfo.int_curr_symbol,
    (unsigned char **) &minfo.currency_symbol,
    (unsigned char **) &minfo.mon_decimal_point,
    (unsigned char **) &minfo.mon_thousands_sep,
    (unsigned char **) &minfo.mon_grouping,
    (unsigned char **) &minfo.positive_sign,
    (unsigned char **) &minfo.negative_sign,
    NULL
  };
  static unsigned char mbuffer[128];	/* Hope it long enough */

  if (!loc_rdline (fd, mbuffer, sizeof (mbuffer), lines))
    return 0;

#define fclen (((&minfo.n_sign_posn) - (&minfo.int_frac_digits)) + sizeof (minfo.n_sign_posn))

  if (read (fd, &minfo.int_frac_digits, fclen) != fclen)
    {
      (void) close (fd);
      return 0;
    }

#undef fclen

  if (!guard_check (fd, cname))
    return 0;

  (void) close (fd);

  _monetary_info = &minfo;

  return 1;
}

static int
DEFUN (loc_numeric, (fd, cname), CONST int fd AND CONST unsigned char *cname)
{
  static struct numeric_info ninfo;
  static unsigned char **lines[] =
  {
    (unsigned char **) &ninfo.decimal_point,
    (unsigned char **) &ninfo.thousands_sep,
    (unsigned char **) &ninfo.grouping,
    NULL
  };
  static unsigned char nbuffer[64];	/* Hope it long enough */

  if (!loc_rdline (fd, nbuffer, sizeof (nbuffer), lines))
    return 0;

  if (!guard_check (fd, cname))
    return 0;

  (void) close (fd);

  _numeric_info = &ninfo;

  return 1;
}

static int
DEFUN (loc_time, (fd, cname), CONST int fd AND CONST unsigned char *cname)
{
  static struct time_info tinfo;
  static unsigned char **lines[] =
  {
    (unsigned char **) &tinfo.abbrev_wkday[0],
    (unsigned char **) &tinfo.abbrev_wkday[1],
    (unsigned char **) &tinfo.abbrev_wkday[2],
    (unsigned char **) &tinfo.abbrev_wkday[3],
    (unsigned char **) &tinfo.abbrev_wkday[4],
    (unsigned char **) &tinfo.abbrev_wkday[5],
    (unsigned char **) &tinfo.abbrev_wkday[6],
    (unsigned char **) &tinfo.full_wkday[0],
    (unsigned char **) &tinfo.full_wkday[1],
    (unsigned char **) &tinfo.full_wkday[2],
    (unsigned char **) &tinfo.full_wkday[3],
    (unsigned char **) &tinfo.full_wkday[4],
    (unsigned char **) &tinfo.full_wkday[5],
    (unsigned char **) &tinfo.full_wkday[6],
    (unsigned char **) &tinfo.abbrev_month[0],
    (unsigned char **) &tinfo.abbrev_month[1],
    (unsigned char **) &tinfo.abbrev_month[2],
    (unsigned char **) &tinfo.abbrev_month[3],
    (unsigned char **) &tinfo.abbrev_month[4],
    (unsigned char **) &tinfo.abbrev_month[5],
    (unsigned char **) &tinfo.abbrev_month[6],
    (unsigned char **) &tinfo.abbrev_month[7],
    (unsigned char **) &tinfo.abbrev_month[8],
    (unsigned char **) &tinfo.abbrev_month[9],
    (unsigned char **) &tinfo.abbrev_month[10],
    (unsigned char **) &tinfo.abbrev_month[11],
    (unsigned char **) &tinfo.full_month[0],
    (unsigned char **) &tinfo.full_month[1],
    (unsigned char **) &tinfo.full_month[2],
    (unsigned char **) &tinfo.full_month[3],
    (unsigned char **) &tinfo.full_month[4],
    (unsigned char **) &tinfo.full_month[5],
    (unsigned char **) &tinfo.full_month[6],
    (unsigned char **) &tinfo.full_month[7],
    (unsigned char **) &tinfo.full_month[8],
    (unsigned char **) &tinfo.full_month[9],
    (unsigned char **) &tinfo.full_month[10],
    (unsigned char **) &tinfo.full_month[11],
    (unsigned char **) &tinfo.ampm[0],
    (unsigned char **) &tinfo.ampm[1],
    (unsigned char **) &tinfo.date_time,
    (unsigned char **) &tinfo.date,
    (unsigned char **) &tinfo.time,
    (unsigned char **) &tinfo.ut0,
    (unsigned char **) &tinfo.tz,
    NULL
  };
  static unsigned char tbuffer[1024];	/* Hope it long enough */

  if (!loc_rdline (fd, tbuffer, sizeof (tbuffer), lines))
    return 0;

  if (!guard_check (fd, cname))
    return 0;

  (void) close (fd);

  _time_info = &tinfo;

  return 1;
}

static int
DEFUN (loc_response, (fd, cname), CONST int fd AND CONST unsigned char *cname)
{
  static struct response_info rinfo;
  static unsigned char **lines[] =
  {
    (unsigned char **) &rinfo.yesexpr,
    (unsigned char **) &rinfo.noexpr,
    NULL
  };
  static unsigned char rbuffer[64];	/* Hope it long enough */

  if (!loc_rdline (fd, rbuffer, sizeof (rbuffer), lines))
    return 0;

  if (!guard_check (fd, cname))
    return 0;

  (void) close (fd);

  _response_info = &rinfo;

  return 1;
}

#endif
