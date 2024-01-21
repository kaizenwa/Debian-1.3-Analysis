/* Checker stubs for functions defined in pwd.h
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

#ifdef HAVE_PWD_H
#include <sys/types.h>
#include <pwd.h>
#include "checker_api.h"

#undef HAVE_getpw
#undef HAVE_fgetpwent
#undef HAVE_putpwent

#if 0
#define HAVE_getpwuid
#define HAVE_getpwnam
#define HAVE_endpwent
#define HAVE_setpwent
#endif

#ifdef HAVE_getpwnam
void
set_right_struct_passwd (struct passwd *ent)
{
  int len;

  len = strlen (ent->pw_name);
  stubs_chkr_set_right (ent->pw_name, len + 1, CHKR_RW);
  stubs_chkr_set_right (&(ent->pw_name), sizeof (char *), CHKR_RW);

  len = strlen (ent->pw_passwd);
  stubs_chkr_set_right (ent->pw_passwd, len + 1, CHKR_RW);
  stubs_chkr_set_right (&(ent->pw_passwd), sizeof (char *), CHKR_RW);

  stubs_chkr_set_right (&(ent->pw_uid), sizeof (uid_t), CHKR_RW);
  stubs_chkr_set_right (&(ent->pw_gid), sizeof (gid_t), CHKR_RW);

  len = strlen (ent->pw_gecos);
  stubs_chkr_set_right (ent->pw_gecos, len + 1, CHKR_RW);
  stubs_chkr_set_right (&(ent->pw_gecos), sizeof (char *), CHKR_RW);

  len = strlen (ent->pw_dir);
  stubs_chkr_set_right (ent->pw_dir, len + 1, CHKR_RW);
  stubs_chkr_set_right (&(ent->pw_dir), sizeof (char *), CHKR_RW);

  len = strlen (ent->pw_shell);
  stubs_chkr_set_right (ent->pw_shell, len + 1, CHKR_RW);
  stubs_chkr_set_right (&(ent->pw_shell), sizeof (char *), CHKR_RW);
}
#else
void set_right_struct_passwd (struct passwd *ent);
#endif

/* compiled from: . */
#ifdef HAVE_setpwent
void
chkr$setpwent (void)
{
#if USE_BI_JUMP
  __builtin_jump (setpwent);
#else
  setpwent ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_setpwent */

#ifdef HAVE_endpwent
void
chkr$endpwent (void)
{
#if USE_BI_JUMP
  __builtin_jump (endpwent);
#else
  endpwent ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_endpwent */

#ifdef HAVE_getpwent
struct passwd *
chkr$getpwent (void)
{
#if USE_BI_JUMP
  __builtin_jump (getpwent);
#else
  {
    struct passwd * res;
    res = getpwent ();
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_getpwent */

#ifdef HAVE_getpw
int
chkr$getpw (__uid_t arg0, char * arg1)
{
  /* This function require a stub */
  stubs_chkr_check_addr (arg1, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (getpw);
#else
  {
    int res;
    res = getpw (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_getpw */

#ifdef HAVE_fgetpwent
struct passwd *
chkr$fgetpwent (FILE * arg0)
{
  /* This function require a stub */
  stubs_chkr_check_addr (arg0, sizeof (FILE), CHKR_XX, "arg0");
#if USE_BI_JUMP
  __builtin_jump (fgetpwent);
#else
  {
    struct passwd * res;
    res = fgetpwent (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_fgetpwent */

#ifdef HAVE_putpwent
int
chkr$putpwent (const struct passwd * arg0, FILE * arg1)
{
  /* This function require a stub */
  stubs_chkr_check_addr (arg0, sizeof (struct passwd), CHKR_XX, "arg0");
  stubs_chkr_check_addr (arg1, sizeof (FILE), CHKR_XX, "arg1");
#if USE_BI_JUMP
  __builtin_jump (putpwent);
#else
  {
    int res;
    res = putpwent (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_putpwent */

#ifdef HAVE_getpwuid
struct passwd *
chkr$getpwuid (uid_t uid)
{
  struct passwd * res;
  res = getpwuid (uid);
  if (res)
    set_right_struct_passwd (res);
  return res;
}
#endif /* HAVE_getpwuid */

#ifdef HAVE_getpwnam
struct passwd *
chkr$getpwnam (const char *name)
{
  struct passwd *res;
  stubs_chkr_check_str (name, CHKR_RO, "name");
  res = getpwnam (name);
  if (res)
    set_right_struct_passwd (res);
  return res;
}
#endif /* HAVE_getpwnam */

#endif /* HAVE_PWD_H */
