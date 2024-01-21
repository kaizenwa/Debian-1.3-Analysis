/*  Copyright (c) 1996 John E. Davis (davis@space.mit.edu)
 *  All rights reserved.
 */
#include "config.h"

#include <stdio.h>
#include <string.h>

#include "ttymsg.h"

void slrn_tty_vmessage (FILE *fp, char *fmt, va_list ap)
{
   FILE *last_fp;
   
   if ((fp == stdout) || (last_fp = stdout)) fputc ('\n', fp);
   (void) vfprintf(fp, fmt, ap);
   if (fp == stderr) fputc ('\n', fp);
   fflush (fp);
   
   last_fp = fp;
}

void slrn_tty_message (char *fmt, ...)
{
   va_list ap;
   
   va_start (ap, fmt);
   slrn_tty_vmessage (stdout, fmt, ap);
   va_end (ap);
}

void slrn_tty_error (char *fmt, ...)
{
   va_list ap;
   
   va_start (ap, fmt);
   slrn_tty_vmessage (stderr, fmt, ap);
   va_end (ap);
}

