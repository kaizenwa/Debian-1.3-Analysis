/*
 * QUOTA    An implementation of the diskquota system for the LINUX
 *          operating system. QUOTA is implemented using the BSD systemcall
 *          interface as the means of communication with the user level.
 *          Should work for all filesystems because of integration into the
 *          VFS layer of the operating system.
 *          This is based on the Melbourne quota system wich uses both user and
 *          group quota files.
 *
 *          Determines if a filesystem has quota enabled and how the quotafile
 *          is named.
 *
 * Version: $Id: hasquota.c,v 2.4 1995/07/23 09:58:06 mvw Exp mvw $
 *
 * Author:  Marco van Wieringen <mvw@planets.ow.nl> <mvw@tnix.net>
 *
 *          This program is free software; you can redistribute it and/or
 *          modify it under the terms of the GNU General Public License
 *          as published by the Free Software Foundation; either version
 *          2 of the License, or (at your option) any later version.
 */
#include <sys/types.h>
#include <limits.h>
#include <linux/quota.h>
#include <string.h>
#include <mntent.h>

#define CORRECT_FSTYPE(type) \
(!strcmp(type,MNTTYPE_EXT2))

char *qfextension[] = INITQFNAMES;
static char *qfname = QUOTAFILENAME;
static char qfullname[PATH_MAX];

/*
 * Check to see if a particular quota is to be enabled.
 */
hasquota(struct mntent *mnt, int type, char **qfnamep)
{
   char *buf, *option, *pathname;

   if (!CORRECT_FSTYPE(mnt->mnt_type))
      return (0);

   if ((type == USRQUOTA) && (option = hasmntopt(mnt, MNTOPT_USRQUOTA)) != (char *)0 ||
       (type == GRPQUOTA) && (option = hasmntopt(mnt, MNTOPT_GRPQUOTA)) != (char *)0) {
      if ((pathname = strchr(option, '=')) == (char *)0) {
         (void) sprintf(qfullname, "%s%s%s.%s", mnt->mnt_dir,
                       (mnt->mnt_dir[strlen(mnt->mnt_dir) - 1] == '/') ? "" : "/",
                       qfname, qfextension[type]);
      } else {
         /*
          * Splice this option on the start of any following option.
          */
         if ((option = strchr(++pathname, ',')) != (char *)NULL)
            *option = '\0';
         strncpy(qfullname, pathname, sizeof(qfullname));
      }
      *qfnamep = strdup(qfullname);
      return (1);
   } else
      return (0);
}
