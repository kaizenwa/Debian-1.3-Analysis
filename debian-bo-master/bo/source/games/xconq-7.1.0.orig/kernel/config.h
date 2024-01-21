/* Configuration-related definitions for Xconq.
   Copyright (C) 1987, 1988, 1989, 1991, 1992, 1993, 1994, 1995
   Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* This file has many things that can be tweaked for local preferences.
   In general, there will not be any need to touch these. */
   
/* System dependencies can also go here, although it is much preferable
   to write things so that they are not needed in the first place. */

/* Universally required include files. */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <time.h>

#if defined(__STDC__) || defined(MPW_C)
#include <stddef.h>
#include <stdarg.h>
#include <stdlib.h>
#endif

#if defined(__STDC__) || defined(MPW_C)
#ifndef ANSI_PROTOTYPES
#define ANSI_PROTOTYPES
#endif
#endif

/* Default names for special files. */

#ifndef NEWSFILE
#define NEWSFILE "news.txt"
#endif

#ifndef SCOREFILE
#define SCOREFILE "scores.xconq"
#endif

#ifndef STATSFILE
#define STATSFILE "stats.xconq"
#endif

/* The name of the default game. */

#ifndef STANDARD_GAME
#define STANDARD_GAME "standard"
#endif

/* Default random world size.  Adjust these to taste - for the standard
   game, 60x60 is a moderate length game, 30x30 is short, 360x120 is
   L-O-N-G !  (Remember to multiply the two numbers to get an idea of the
   number of cells that will be in the world!)  Defaults can also be set
   by modules explicitly, so this is really a "default default" :-) . */

#ifndef DEFAULTWIDTH
#define DEFAULTWIDTH 60
#endif
#ifndef DEFAULTHEIGHT
#define DEFAULTHEIGHT 30
#endif

/* Default world circumference.  If this matches the default width,
   then the game area wraps around in x; otherwise it will be a
   polygon. */

#ifndef DEFAULTCIRCUMFERENCE
#define DEFAULTCIRCUMFERENCE 360
#endif

/* Absolute maximum number of sides that can play.  This cannot be
   set higher than 30 without hacking up side mask machinery.  Smaller
   values will incur fewer assorted memory penalties, and in any case
   games with lots of sides don't usually play very well. */

#ifndef MAXSIDES
#define MAXSIDES 15
#endif

/* Absolute maximum number of kinds of units.  The lower this is set,
   the less memory needed (although most allocation actually works off
   the actual number of types, so effect is minor), but 126 is
   absolutely the top unless you want to do lots of rewriting. */

#ifndef MAXUTYPES
#define MAXUTYPES 126
#endif

/* Maximum number of material/supply types.  Limits same as for unit types. */

#ifndef MAXMTYPES
#define MAXMTYPES 126
#endif

/* Maximum number of terrain types.  Limits same as for unit types. */

#ifndef MAXTTYPES
#define MAXTTYPES 126
#endif

/* Some parts of the code rarely need the full magnitude of the MAX*TYPE
   values, so these values are used to cut down average allocation.  They
   can be adjusted to favor the kinds of game designs that you usually run. */

#define INITMAXUTYPES 20
#define INITMAXMTYPES  5
#define INITMAXTTYPES 15

/* All names, phrases, and messages must be able to fit in statically
   allocated buffers of this size. */

#define BUFSIZE 255

/* The following options are (should be anyway) mostly independent of
   each other, and settable at will. */

/* When defined, save files will be removed after the game starts up
   successfully.  This keeps junk from lying around, but some folks
   prefer that they be kept just in case. */
/* (should make this work or remove it) */
#define RMSAVE

/* When defined, messages will be somewhat nastier.  Set this to be
   appropriate for your intended audience, heh-heh :-) */

#define RUDE

/* This sets a limit on how many previous views will be retained.
   Each increment adds 4*area bytes of allocation. */

#define MAXVIEWHISTORY 0

/* If defined, then an editing mode is available.  This is also a
   powerful way to cheat, which is why it can be disabled. */

#define DESIGNERS

/* If defined, some (actually a lot) debugging code will be included. */

#define DEBUGGING

/* If defined, the compiler will be included. */

#define COMPILER
