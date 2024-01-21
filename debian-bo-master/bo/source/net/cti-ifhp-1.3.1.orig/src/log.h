/***************************************************************************
 * LPRng - An Extended Print Spooler System
 *
 * Copyright 1988-1995 Patrick Powell, San Diego State University
 *     papowell@sdsu.edu
 * See LICENSE for conditions of use.
 *
 ***************************************************************************
 * MODULE: errormsg.h
 * PURPOSE: identifies error message information, see errormsg.c
 * $Id: log.h,v 1.1 1995/09/02 15:12:44 papowell Exp $
 **************************************************************************/

#ifndef _LOG_H
#define _LOG_H

#if defined(HAVE_STDARGS)
void log (int kind, char *msg,...);
void fatal ( char *msg,...);
void logerr (int kind, char *msg,...);
void logerr_die (int kind, char *msg,...);
#else
void log ();
void fatal ();
void logerr ();
void logerr_die ();
#endif

#endif
