/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* This has a module all to itself so that it can be included in various
utilities without having to drag in unwanted code. If debug_pid is nonzero,
print the pid at the start of each message and add a newline if there isn't
one. This is for tidier output when running parallel remote deliveries
with debugging turned on. Must do the whole thing with a single printf
and flush, as otherwise output may get interleaved. */

#include "exim.h"

void
debug_printf(char *format, ...)
{
va_list ap;
if (debug_file == NULL) return;
va_start(ap, format);

if (debug_pid <= 0) vfprintf(debug_file, format, ap); else
  {
  char buffer[256];
  char *ptr = buffer;
  sprintf(buffer, "%6d ", (int)debug_pid);
  while(*ptr) ptr++;
  vsprintf(ptr, format, ap);
  if (format[(int)strlen(format)-1] != '\n')
    {
    while(*ptr) ptr++;
    strcat(ptr, "\n");
    }
  fprintf(debug_file, "%s", buffer);
  }

fflush(debug_file);
va_end(ap);
}

/* End of debug.c */
