/* Checker stubs for functions defined in getopt.h
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

#ifdef HAVE_GETOPT_H
#include <getopt.h>
#else
#include <stdlib.h>
#endif
#include "checker_api.h"

#if 0
#define HAVE_getopt
#define HAVE_getopt_long
#define HAVE_getopt_long_only
#endif

/* Rq: argv[0] is never checked.  */

#ifdef HAVE_getopt_long
void
stubs_chkr_check_struct_option (const struct option *longopt)
{
 do
   {
     stubs_chkr_check_str (&(longopt->name), CHKR_RO, "longopt->name");
     stubs_chkr_check_addr (&(longopt->has_arg), sizeof (int), CHKR_RO, "longopt->has_arg");
     stubs_chkr_check_addr (&(longopt->flag), sizeof (int), CHKR_RO, "longopt->flag");
     if (longopt->flag)
       stubs_chkr_check_addr (longopt->flag, sizeof (int), CHKR_TW, "*longopt->flag");
     stubs_chkr_check_addr (&(longopt->val), sizeof (int), CHKR_RO, "longopt->val");
     if (longopt->name == (char *)0
         && longopt->has_arg == 0
         && longopt->flag == 0
         && longopt->val == 0)
       break;
     longopt++;
   }
 while (1);
}
#else
# ifdef HAVE_getopt_long_only
void stubs_chkr_check_struct_option (const struct option *longopt);
# endif
#endif

/* compiled from: . */
#ifdef HAVE_getopt
int
chkr$getopt (int argc, char *const *argv, const char *shortopts)
{
  int i;
  
  stubs_chkr_check_addr (argv, (argc + 1) * sizeof (char *), CHKR_RO, "argv");
  for (i = 1; i < argc; i++)
    stubs_chkr_check_str (argv[i], CHKR_RO, "argv[i]");
  stubs_chkr_check_str (shortopts, CHKR_RO, "shortopts");
#if USE_BI_JUMP
  __builtin_jump (getopt);
#else
  return getopt (argc, argv, shortopts);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_getopt */

#ifdef HAVE_getopt_long
int
chkr$getopt_long (int argc, char *const *argv, const char *shortopts,
                  const struct option *longopt, int *indexptr)
{
  int i;
  int res;
  
  stubs_chkr_check_addr (argv, (argc + 1) * sizeof (char *), CHKR_RO, "argv");
  for (i = 1; i < argc; i++)
    stubs_chkr_check_str (argv[i], CHKR_RO, "argv[i]");
  stubs_chkr_check_str (shortopts, CHKR_RO, "shortopts");
  stubs_chkr_check_struct_option (longopt);
  if (indexptr)
    {
      stubs_chkr_check_addr (indexptr, sizeof (int), CHKR_MW, "indexptr");
      *indexptr = -1;	/* Kuldge.  */
    }
  
  res = getopt_long (argc, argv, shortopts, longopt, indexptr);
  if (indexptr && *indexptr != -1)
    {
      stubs_chkr_set_right (indexptr, sizeof (int), CHKR_RW);
      if (longopt[*indexptr].flag)
        stubs_chkr_set_right (longopt[*indexptr].flag, sizeof (int), CHKR_RW);
    }
  return res;
}
#endif /* HAVE_getopt_long */

#ifdef HAVE_getopt_long_only
int
chkr$getopt_long_only (int argc, char *const *argv, const char *shortopts,
                       const struct option *longopt, int *indexptr)
{
  int i;
  int res;
  
  stubs_chkr_check_addr (argv, (argc + 1) * sizeof (char *), CHKR_RO, "argv");
  for (i = 1; i < argc; i++)
    stubs_chkr_check_str (argv[i], CHKR_RO, "argv[i]");
  stubs_chkr_check_str (shortopts, CHKR_RO, "shortopts");
  stubs_chkr_check_struct_option (longopt);
  if (indexptr)
    {
      stubs_chkr_check_addr (indexptr, sizeof (int), CHKR_MW, "indexptr");
      *indexptr = -1;	/* Kuldge.  */
    }
  
  res = getopt_long_only (argc, argv, shortopts, longopt, indexptr);
  if (indexptr && *indexptr != -1)
    {
      stubs_chkr_set_right (indexptr, sizeof (int), CHKR_RW);
      if (longopt[*indexptr].flag)
        stubs_chkr_set_right (longopt[*indexptr].flag, sizeof (int), CHKR_RW);
    }
  return res;
}
#endif /* HAVE_getopt_long_only */
