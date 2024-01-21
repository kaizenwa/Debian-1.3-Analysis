/* conf.h.  Generated automatically by configure.  */
/*
 * Configuration defines
 *
 * Copyright 1993 by Gray Watson and the Antaire Corporation
 *
 * This file is part of the argv library.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose and without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies, and that
 * the name of Antaire not be used in advertising or publicity pertaining to
 * distribution of the document or software without specific, written prior
 * permission.
 *
 * The Antaire Corporation makes no representations about the suitability of
 * the software described herein for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * The author may be contacted at gray.watson@antaire.com
 *
 * $Id: conf.h.in,v 1.9 1994/02/20 05:41:43 gray Exp $
 */

#ifndef __CONF_H__
#define __CONF_H__

/*
 ************************************************************************
 * MANUAL SETTINGS:
 ************************************************************************
 */

/*
 * some compilation options
 */

/* to include RCS ids in the code */
#ifndef INCLUDE_RCS_IDS
#define INCLUDE_RCS_IDS 1
#endif

/* define this option to 1 to complain if an option is used only once */
#ifndef USE_ONCE
#define USE_ONCE 0
#endif

/* define this to the default usage format */
#ifndef ARGV_USAGE_DEFAULT
#define ARGV_USAGE_DEFAULT	ARGV_USAGE_SHORT
#endif

/* define this option to 1 to DISable the -x=10 format args */
#ifndef NO_CLOSE_ARGUMENT
#define NO_CLOSE_ARGUMENT 0
#endif

/*
 ************************************************************************
 * CONFIGURE DEFINES:
 ************************************************************************
 */

/*
 * LIBRARY DEFINES:
 */

/*
 * the argv library provides its own versions of the following
 * functions,  or knows how to work around their absence.
 */

#define HAVE_RINDEX 1

#define HAVE_STRCMP 1
#define HAVE_STRCPY 1
#define HAVE_STRLEN 1
#define HAVE_STRNCMP 1
#define HAVE_STRNCPY 1

#define HAVE_STRDUP 1
#define HAVE_STRSTR 1
#define HAVE_STRTOK 1

#endif /* ! __CONF_H__ */
