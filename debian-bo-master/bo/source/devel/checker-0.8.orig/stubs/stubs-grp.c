/* Checker stubs for functions defined in grp.h
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

#ifdef HAVE_GRP_H
#include <sys/types.h>
#include <grp.h>
#include "checker_api.h"

#undef HAVE_fgetgrent
#undef HAVE_setgroups
#undef HAVE_initgroups

#if 0
#define HAVE_getgrgid
#define HAVE_getgrent
#define HAVE_setgrent
#define HAVE_endgrent
#define HAVE_getgrnam
#endif

#ifdef HAVE_chkr_func
void
set_right_struct_group (struct group *ent)
{
  int len;
  char **mem;
  
  len = strlen (ent->gr_name);
  stubs_chkr_set_right (ent->gr_name, len + 1, CHKR_RW);
  stubs_chkr_set_right (&(ent->gr_name), sizeof (char *), CHKR_RW);
  
  len = strlen (ent->gr_passwd);
  stubs_chkr_set_right (ent->gr_passwd, len + 1, CHKR_RW);
  stubs_chkr_set_right (&(ent->gr_passwd), sizeof (char *), CHKR_RW);
  
  stubs_chkr_set_right (&(ent->gr_gid), sizeof (gid_t), CHKR_RW);
  
  stubs_chkr_set_right (&(ent->gr_mem), sizeof (char *), CHKR_RW);
  for (mem = ent->gr_mem; *mem; mem++)
    {
      len = strlen (*mem);
      stubs_chkr_set_right (*mem, len + 1, CHKR_RW);
    }
  stubs_chkr_set_right (ent->gr_mem, (mem + 1) - ent->gr_mem, CHKR_RW);
}
#else
void set_right_struct_group (struct group *ent);
#endif

/* compiled from: . */
#ifdef HAVE_setgrent
void
chkr$setgrent (void)
{
#if USE_BI_JUMP
  __builtin_jump (setgrent);
#else
  setgrent ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_setgrent */

#ifdef HAVE_endgrent
void
chkr$endgrent (void)
{
#if USE_BI_JUMP
  __builtin_jump (endgrent);
#else
  endgrent ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_endgrent */

#ifdef HAVE_getgrent
struct group *
chkr$getgrent (void)
{
  struct group * res;
  res = getgrent ();
  if (res)
    set_right_struct_group (res);
  return res;
}
#endif /* HAVE_getgrent */

#ifdef HAVE_fgetgrent
struct group *
chkr$fgetgrent (FILE * arg0)
{
  /* This function require a stub */
  stubs_chkr_check_addr (arg0, sizeof (FILE), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (fgetgrent);
#else
  {
    struct group * res;
    res = fgetgrent (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_fgetgrent */

#ifdef HAVE_getgrgid
struct group *
chkr$getgrgid (gid_t gid)
{
  struct group * res;
  res = getgrgid (gid);
  if (res)
    set_right_struct_group (res);
  return res;
}
#endif /* HAVE_getgrgid */

#ifdef HAVE_getgrnam
struct group *
chkr$getgrnam (const char *name)
{
  struct group *res;
  
  stubs_chkr_check_str (name, CHKR_RO, "name");
  res = getgrnam (name);
  if (res)
    set_right_struct_group (res);
  return res;
}
#endif /* HAVE_getgrnam */

#ifdef HAVE_setgroups
int
chkr$setgroups (size_t arg0, const gid_t * arg1)
{
  /* This function require a stub */
  stubs_chkr_check_addr (arg1, sizeof (gid_t), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (setgroups);
#else
  {
    int res;
    res = setgroups (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_setgroups */

#ifdef HAVE_initgroups
int
chkr$initgroups (const char * arg0, gid_t arg1)
{
  /* This function require a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (initgroups);
#else
  {
    int res;
    res = initgroups (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_initgroups */

#endif /* HAVE_GRP_H */
