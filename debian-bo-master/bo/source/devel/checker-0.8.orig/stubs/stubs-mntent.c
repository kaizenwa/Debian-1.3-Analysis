/* Checker stubs for functions defined in mntent.h
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

#ifdef HAVE_MNTENT_H
#include <mntent.h>
#include "checker_api.h"

#ifdef HAVE_addmntent
void
stubs_chkr_check_struct_mntent (const struct mntent *ent, int right)
{
  stubs_chkr_check_str (&ent->mnt_fsname, right, "ent->mnt_fsname");
  stubs_chkr_check_str (&ent->mnt_dir, right, "ent->mnt_dir");
  stubs_chkr_check_str (&ent->mnt_type, right, "ent->mnt_type");
  stubs_chkr_check_str (&ent->mnt_opts, right, "ent->mnt_opts");
  stubs_chkr_check_addr (&ent->mnt_freq, sizeof (ent->mnt_freq), right, "ent->mnt_freq");
  stubs_chkr_check_addr (&ent->mnt_passno, sizeof (ent->mnt_passno), right, "ent->mnt_passno");
}
#else
void stubs_chkr_check_struct_mntent (const struct mntent *ent, int right);
#endif

#ifdef HAVE_getmntent
static void
stubs_chkr_set_right_struct_mntent (struct mntent *ent, int right)
{
  stubs_chkr_set_right (&ent->mnt_fsname, strlen (ent->mnt_fsname) + 1, right);
  stubs_chkr_set_right (&ent->mnt_dir, strlen (ent->mnt_dir) + 1, right);
  stubs_chkr_set_right (&ent->mnt_type, strlen (ent->mnt_type) + 1, right);
  stubs_chkr_set_right (&ent->mnt_opts, strlen (ent->mnt_opts) + 1, right);
  stubs_chkr_set_right (&ent->mnt_freq, sizeof (ent->mnt_freq), right);
  stubs_chkr_set_right (&ent->mnt_passno, sizeof (ent->mnt_passno), right);
}
#endif

/* compiled from: . */
#ifdef HAVE_setmntent
/* From `/usr/include/mntent.h:98'.  */
FILE *
chkr$setmntent (const char *file, const char *type)
{
  FILE *res;
  
  stubs_chkr_check_str (file, CHKR_RO, "file");
  stubs_chkr_check_str (type, CHKR_RO, "type");
  res = setmntent (file, type);
  if (res != NULL)
    fd_returned_by_system (fileno (res));
  return res;
}
#endif /* HAVE_setmntent */

#ifdef HAVE_getmntent
/* From `/usr/include/mntent.h:100'.  */
struct mntent *
chkr$getmntent (FILE *file)
{
  struct mntent *res;
  
  stubs_chkr_check_addr (file, sizeof (FILE), CHKR_TW, "file");
  res = getmntent (file);
  stubs_chkr_set_right_struct_mntent (res, CHKR_WO);
  return res;
}
#endif /* HAVE_getmntent */

#ifdef HAVE_addmntent
/* From `/usr/include/mntent.h:102'.  */
int
chkr$addmntent (FILE *filep, const struct mntent *mnt)
{
  stubs_chkr_check_addr (filep, sizeof (FILE), CHKR_TW, "filep");
  stubs_chkr_check_struct_mntent (mnt, CHKR_RO);
#if USE_BI_JUMP
  __builtin_jump (addmntent);
#else
  return addmntent (filep, mnt);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_addmntent */

#ifdef HAVE_hasmntopt
/* From `/usr/include/mntent.h:104'.  */
char *
chkr$hasmntopt (const struct mntent *mnt, const char *opt)
{
  stubs_chkr_check_str (opt, CHKR_RO, "opt");
  stubs_chkr_check_struct_mntent (mnt, CHKR_RO);
#if USE_BI_JUMP
  __builtin_jump (hasmntopt);
#else
  return hasmntopt (mnt, opt);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_hasmntopt */

#ifdef HAVE_endmntent
/* From `/usr/include/mntent.h:105'.  */
int
chkr$endmntent (FILE *filep)
{
  stubs_chkr_check_addr (filep, sizeof (FILE), CHKR_TW, "filep");
#if USE_BI_JUMP
  __builtin_jump (endmntent);
#else
  return endmntent (filep);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_endmntent */

#endif /* HAVE_MNTENT_H */
