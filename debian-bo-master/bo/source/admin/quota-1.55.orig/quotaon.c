/*
 * Copyright (c) 1980, 1990 Regents of the University of California. All
 * rights reserved.
 * 
 * This code is derived from software contributed to Berkeley by Robert Elz at
 * The University of Melbourne.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met: 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer. 2.
 * Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution. 3. All advertising
 * materials mentioning features or use of this software must display the
 * following acknowledgement: This product includes software developed by the
 * University of California, Berkeley and its contributors. 4. Neither the
 * name of the University nor the names of its contributors may be used to
 * endorse or promote products derived from this software without specific
 * prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980, 1990 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char rcsid[] = "$Id: quotaon.c,v 1.3 1995/05/15 13:49:14 mvw Exp $";
#endif /* not lint */

/*
 * Turn quota on/off for a filesystem.
 */
#include <sys/param.h>
#include <sys/file.h>
#include <sys/mount.h>
#include <linux/quota.h>
#include <stdio.h>
#include <string.h>
#include <mntent.h>

extern char *qfextension[];

int aflag; /* all file systems */
int gflag; /* operate on group quotas */
int uflag; /* operate on user quotas */
int vflag; /* verbose */

main(int argc, char **argv)
{
   register struct mntent *mnt;
   FILE *fp;
   char ch, *qfnp, *whoami;
   long argnum, done = 0;
   int cnt, offmode = 0, errs = 0;
   extern char *optarg;
   extern int optind;

   whoami = strrchr(*argv, '/');
   if (whoami == (char *) 0)
      whoami = *argv;
   else
      whoami = whoami++;

   if (strcmp(whoami, "quotaoff") == 0)
      offmode++;
   else if (strcmp(whoami, "quotaon") != 0) {
      fprintf(stderr, "Name must be quotaon or quotaoff not %s\n",
         whoami);
      exit(1);
   }
   while ((ch = getopt(argc, argv, "avug")) != EOF) {
      switch (ch) {
         case 'a':
            aflag++;
            break;
         case 'g':
            gflag++;
            break;
         case 'u':
            uflag++;
            break;
         case 'v':
            vflag++;
            break;
         default:
            usage(whoami);
      }
   }
   argc -= optind;
   argv += optind;

   if (argc <= 0 && !aflag)
      usage(whoami);
   if (!gflag && !uflag) {
      gflag++;
      uflag++;
   }
   fp = setmntent(MNTTAB, "r");
   while ((mnt = getmntent(fp)) != (struct mntent *) 0) {
      if (aflag) {
         if (hasmntopt(mnt, MNTOPT_NOAUTO))
            continue;
         if (gflag && hasquota(mnt, GRPQUOTA, &qfnp))
            errs += quotaonoff(mnt, offmode, GRPQUOTA, qfnp);
         if (uflag && hasquota(mnt, USRQUOTA, &qfnp))
            errs += quotaonoff(mnt, offmode, USRQUOTA, qfnp);
         continue;
      }
      if ((argnum = oneof(mnt->mnt_dir, argv, argc)) >= 0 ||
          (argnum = oneof(mnt->mnt_fsname, argv, argc)) >= 0) {
         done |= 1 << argnum;
         if (gflag && hasquota(mnt, GRPQUOTA, &qfnp))
            errs += quotaonoff(mnt, offmode, GRPQUOTA, qfnp);
         if (uflag && hasquota(mnt, USRQUOTA, &qfnp))
            errs += quotaonoff(mnt, offmode, USRQUOTA, qfnp);
      }
   }
   endmntent(fp);

   for (cnt = 0; cnt < argc; cnt++)
      if ((done & (1 << cnt)) == 0)
         fprintf(stderr, "%s not found in fstab\n", argv[cnt]);
   exit(errs);
}

usage(char *whoami)
{

   fprintf(stderr, "Usage:\n\t%s [-g] [-u] [-v] -a\n", whoami);
   fprintf(stderr, "\t%s [-g] [-u] [-v] filesys ...\n", whoami);
   exit(1);
}

quotaonoff(struct mntent *mnt, int offmode, int type, char *qfpathname)
{
   if (offmode) {
      if (quotactl(QCMD(Q_QUOTAOFF, type), mnt->mnt_fsname, 0, (caddr_t)0) < 0) {
         fprintf(stderr, "quotaoff: ");
         perror(mnt->mnt_fsname);
         return (1);
      }
      if (vflag)
         printf("%s: %s quotas turned off\n", mnt->mnt_fsname,
                qfextension[type]);
      return (0);
   }
   if (quotactl(QCMD(Q_QUOTAON, type), mnt->mnt_fsname, 0, (caddr_t) qfpathname) < 0) {
      fprintf(stderr, "quotaon: using %s on ", qfpathname);
      perror(mnt->mnt_fsname);
      return (1);
   }
   if (vflag)
      printf("%s: %s quotas turned on\n", mnt->mnt_fsname,
             qfextension[type]);
   return (0);
}

/*
 * Check to see if target appears in list of size cnt.
 */
oneof(char *target, char *list[], int cnt)
{
   register int i;

   for (i = 0; i < cnt; i++)
      if (strcmp(target, list[i]) == 0)
         return (i);
   return (-1);
}
