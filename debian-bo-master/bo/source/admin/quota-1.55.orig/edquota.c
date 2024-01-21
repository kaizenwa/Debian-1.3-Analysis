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
static char rcsid[] = "$Id: edquota.c,v 1.6 1995/07/23 09:58:06 mvw Exp mvw $";
#endif /* not lint */

/*
 * Disk quota editor.
 */
#include <sys/types.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <linux/quota.h>
#include <errno.h>
#include <mntent.h>
#include <pwd.h>
#include <grp.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>
#include "pathnames.h"

extern char *qfextension[];
char *quotagroup = QUOTAGROUP;
char tmpfil[] = _PATH_TMP;

struct quotause {
   struct quotause *next;
   long flags;
   struct dqblk dqblk;
   char fsname[MAXPATHLEN + 1];
   char qfname[1];   /* actually longer */
} *getprivs();

#define   FOUND   0x01

main(int argc, char **argv)
{
   struct quotause *qup, *protoprivs, *curprivs, *pprivs, *cprivs;
   extern char *optarg;
   extern int optind;
   long id, protoid;
   int quotatype, tmpfd;
   char *protoname, ch;
   int tflag = 0, pflag = 0;

   if (argc < 2)
      usage();

   quotatype = USRQUOTA;
   while ((ch = getopt(argc, argv, "ugtp:")) != EOF) {
      switch (ch) {
      case 'p':
         protoname = optarg;
         pflag++;
         break;
      case 'g':
         quotatype = GRPQUOTA;
         break;
      case 'u':
         quotatype = USRQUOTA;
         break;
      case 't':
         tflag++;
         break;
      default:
         usage();
      }
   }
   argc -= optind;
   argv += optind;

   if (pflag) {
      if ((protoid = getentry(protoname, quotatype)) == -1)
         exit(1);
      protoprivs = getprivs(protoid, quotatype);
      for (qup = protoprivs; qup; qup = qup->next) {
         qup->dqblk.dqb_btime = 0;
         qup->dqblk.dqb_itime = 0;
      }
      while (argc-- > 0) {
         if ((id = getentry(*argv++, quotatype)) < 0)
            continue;
         curprivs = getprivs(id, quotatype);

	 for (pprivs = protoprivs, cprivs = curprivs;
	      pprivs && cprivs;
	      pprivs = pprivs->next, cprivs = cprivs->next) {
	    if (strcmp(pprivs->fsname, cprivs->fsname)) {
	       fprintf(stderr, "fsname mismatch\n");
	    } else {
	       pprivs->dqblk.dqb_curblocks = cprivs->dqblk.dqb_curblocks;
	       pprivs->dqblk.dqb_curinodes = cprivs->dqblk.dqb_curinodes;
	    }
	 }
         putprivs(id, quotatype, protoprivs);
      }
      exit(0);
   }

   umask(077);
   tmpfd = mkstemp(tmpfil);
   fchown(tmpfd, getuid(), getgid());
   if (tflag) {
      protoprivs = getprivs(0, quotatype);
      if (writetimes(protoprivs, tmpfd, quotatype) == 0)
         exit(1);
      if (editit(tmpfil) && readtimes(protoprivs, tmpfd))
         putprivs(0, quotatype, protoprivs);
      freeprivs(protoprivs);
      close(tmpfd);
      unlink(tmpfil);
   } else {
      for (; argc > 0; argc--, argv++) {
         if ((id = getentry(*argv, quotatype)) == -1)
            continue;
         curprivs = getprivs(id, quotatype);
         if (writeprivs(curprivs, tmpfd, *argv, quotatype) == 0)
            continue;
         if (editit(tmpfil) && readprivs(curprivs, tmpfd))
            putprivs(id, quotatype, curprivs);
         freeprivs(curprivs);
      }
   }

   close(tmpfd);
   unlink(tmpfil);
   exit(0);
}

usage(void)
{
   fprintf(stderr, "%s%s%s%s",
      "Usage:\tedquota [-u] [-p username] username ...\n",
      "\tedquota -g [-p groupname] groupname ...\n",
      "\tedquota [-u] -t\n", "\tedquota -g -t\n");
   exit(1);
}

/*
 * This routine converts a name for a particular quota type to an identifier.
 */
getentry(char *name, int quotatype)
{
   struct passwd  *pw;
   struct group   *gr;

   if (alldigits(name))
      return (atoi(name));
   switch (quotatype) {
      case USRQUOTA:
         if (pw = getpwnam(name))
            return (pw->pw_uid);
         fprintf(stderr, "%s: no such user\n", name);
         break;
      case GRPQUOTA:
         if (gr = getgrnam(name))
            return (gr->gr_gid);
         fprintf(stderr, "%s: no such group\n", name);
         break;
      default:
         fprintf(stderr, "%d: unknown quota type\n", quotatype);
         break;
   }
   sleep(1);
   return (-1);
}

/*
 * Collect the requested quota information.
 */
struct quotause *getprivs(long id, int quotatype)
{
   struct mntent *mnt;
   struct quotause *qup, *quptail;
   FILE *fp;
   struct quotause *quphead;
   int qcmd, qupsize, fd;
   char *qfpathname;
   static int warned = 0;
   extern int errno;

   fp = setmntent(MNTTAB, "r");
   quphead = (struct quotause *) 0;
   qcmd = QCMD(Q_GETQUOTA, quotatype);
   while ((mnt = getmntent(fp)) != (struct mntent *) 0) {
      if (hasmntopt(mnt, MNTOPT_NOAUTO) || !hasquota(mnt, quotatype, &qfpathname))
         continue;

      if (access(qfpathname, R_OK | W_OK)) {
         fprintf(stderr, "edquota: open %s permission denied\n", qfpathname);
         continue;
      }

      qupsize = sizeof(*qup) + strlen(qfpathname);
      if ((qup = (struct quotause *) malloc(qupsize)) == (struct quotause *)NULL) {
         fprintf(stderr, "edquota: out of memory\n");
         exit(2);
      }

      if (quotactl(qcmd, mnt->mnt_fsname, id, (caddr_t) &qup->dqblk) != 0) {
         if ((errno == EOPNOTSUPP || errno == ENOSYS || errno == ENOPKG) && !warned) {
            warned++;
            fprintf(stderr, "Warning: %s\n",
            "Quotas are not compiled into this kernel");
            sleep(3);
         }
         if ((fd = open(qfpathname, O_RDONLY)) < 0) {
            fd = open(qfpathname, O_RDWR | O_CREAT, 0640);
            if (fd < 0 && errno != ENOENT) {
               perror(qfpathname);
               free(qup);
               continue;
            }
            fprintf(stderr, "Creating quota file %s\n", qfpathname);
            sleep(3);
            (void) fchown(fd, getuid(),
                   getentry(quotagroup, GRPQUOTA));
            (void) fchmod(fd, 0640);
         }

         lseek(fd, (long) (id * sizeof(struct dqblk)), L_SET);
         switch (read(fd, &qup->dqblk, sizeof(struct dqblk))) {
            case 0:/* EOF */
               /*
                * Convert implicit 0 quota (EOF) into an
                * explicit one (zero'ed dqblk)
                */
               bzero((caddr_t) & qup->dqblk,
                     sizeof(struct dqblk));
               break;
            case sizeof(struct dqblk):   /* OK */
               break;
            default:   /* ERROR */
               fprintf(stderr, "edquota: read error in ");
               perror(qfpathname);
               close(fd);
               free(qup);
               continue;
         }
         close(fd);
      }

      strcpy(qup->qfname, qfpathname);
      strcpy(qup->fsname, mnt->mnt_fsname);
      if (quphead == NULL)
         quphead = qup;
      else
         quptail->next = qup;
      quptail = qup;
      qup->next = 0;
   }
   endmntent(fp);
   return (quphead);
}

/*
 * Store the requested quota information.
 */
putprivs(long id, int quotatype, struct quotause *quplist)
{
   struct quotause *qup;
   int qcmd, fd;

   qcmd = QCMD(Q_SETQUOTA, quotatype);
   for (qup = quplist; qup; qup = qup->next) {
      if (quotactl(qcmd, qup->fsname, id, (caddr_t) &qup->dqblk) == 0)
         continue;
      if ((fd = open(qup->qfname, O_WRONLY)) < 0) {
         perror(qup->qfname);
      } else {
         lseek(fd, (long) id * (long) sizeof(struct dqblk), 0);
         if (write(fd, &qup->dqblk, sizeof(struct dqblk)) !=
             sizeof(struct dqblk)) {
            fprintf(stderr, "edquota: ");
            perror(qup->qfname);
         }
         close(fd);
      }
   }
}

/*
 * Take a list of priviledges and get it edited.
 */
editit(char *tmpfile)
{
   long omask;
   int pid, stat;
   extern char *getenv();

   omask = sigblock(sigmask(SIGINT) | sigmask(SIGQUIT) | sigmask(SIGHUP));
   if ((pid = fork()) < 0) {
      extern errno;

      perror("fork");
      return (0);
   }
   if (pid == 0) {
      char  *ed;

      sigsetmask(omask);
      setgid(getgid());
      setuid(getuid());
      if ((ed = getenv("VISUAL")) == (char *) 0)
         if ((ed = getenv("EDITOR")) == (char *) 0)
            ed = _PATH_VI;
      execlp(ed, ed, tmpfile, 0);
      perror(ed);
      exit(1);
   }
   waitpid(pid, &stat, 0);
   sigsetmask(omask);

   return (1);
}

/*
 * Convert a quotause list to an ASCII file.
 */
writeprivs(struct quotause *quplist, int outfd, char *name, int quotatype)
{
   struct quotause *qup;
   FILE *fd;

   ftruncate(outfd, 0);
   lseek(outfd, 0, L_SET);
   if ((fd = fdopen(dup(outfd), "w")) == NULL) {
      fprintf(stderr, "edquota: ");
      perror(tmpfil);
      exit(1);
   }

#if defined(ALT_FORMAT)
   fprintf(fd, "Disk quotas for %s %s (%cid %d):\n", 
           qfextension[quotatype], name,
           *qfextension[quotatype], getentry(name, quotatype));

   fprintf(fd, "  Filesystem       blocks       soft       hard     inodes     soft     hard\n");

   for (qup = quplist; qup; qup = qup->next) {
      fprintf(fd, "  %-12s %10d %10d %10d %10d %8d %8d\n",
              qup->fsname,
              dbtob(qup->dqblk.dqb_curblocks) / 1024,
              dbtob(qup->dqblk.dqb_bsoftlimit) / 1024,
              dbtob(qup->dqblk.dqb_bhardlimit) / 1024,
              qup->dqblk.dqb_curinodes,
              qup->dqblk.dqb_isoftlimit, 
	      qup->dqblk.dqb_ihardlimit);
   }
#else
   fprintf(fd, "Quotas for %s %s:\n", qfextension[quotatype], name);
   for (qup = quplist; qup; qup = qup->next) {
      fprintf(fd, "%s: %s %d, limits (soft = %d, hard = %d)\n",
              qup->fsname, "blocks in use:",
              dbtob(qup->dqblk.dqb_curblocks) / 1024,
              dbtob(qup->dqblk.dqb_bsoftlimit) / 1024,
              dbtob(qup->dqblk.dqb_bhardlimit) / 1024);
      fprintf(fd, "%s %d, limits (soft = %d, hard = %d)\n",
              "\tinodes in use:", qup->dqblk.dqb_curinodes,
              qup->dqblk.dqb_isoftlimit, qup->dqblk.dqb_ihardlimit);
   }
#endif
   fclose(fd);
   return (1);
}

/*
 * Merge changes to an ASCII file into a quotause list.
 */
readprivs(struct quotause *quplist, int infd)
{
   struct quotause *qup;
   FILE *fd;
   int cnt;
   char *cp;
   struct dqblk dqblk;
#if defined(ALT_FORMAT)
   char fsp[BUFSIZ], line[BUFSIZ];
#else
   char *fsp, line1[BUFSIZ], line2[BUFSIZ];
#endif

   lseek(infd, 0, L_SET);
   fd = fdopen(dup(infd), "r");
   if (fd == NULL) {
      fprintf(stderr, "Can't re-read temp file!!\n");
      return (0);
   }

#if defined(ALT_FORMAT)
   /*
    * Discard title lines, then read lines to process.
    */
   (void) fgets(line, sizeof(line), fd);
   (void) fgets(line, sizeof(line), fd);
	
   while (fgets(line, sizeof(line), fd)) {
      cnt = sscanf(line, "%s %d %d %d %d %d %d",
	           fsp, &dqblk.dqb_curblocks, &dqblk.dqb_bsoftlimit,
                   &dqblk.dqb_bhardlimit, &dqblk.dqb_curinodes,
                   &dqblk.dqb_isoftlimit, &dqblk.dqb_ihardlimit);

      if (cnt != 7) {
         fprintf(stderr, "bad format:\n%s\n", line);
         return (0);
      }
	   
      dqblk.dqb_curblocks  = btodb(dqblk.dqb_curblocks * 1024);
      dqblk.dqb_bsoftlimit = btodb(dqblk.dqb_bsoftlimit * 1024);
      dqblk.dqb_bhardlimit = btodb(dqblk.dqb_bhardlimit * 1024);

      for (qup = quplist; qup; qup = qup->next) {
         if (strcmp(fsp, qup->fsname))
            continue;
         /*
          * Cause time limit to be reset when the quota is
          * next used if previously had no soft limit or were
          * under it, but now have a soft limit and are over
          * it.
          */
         if (dqblk.dqb_bsoftlimit &&
            (qup->dqblk.dqb_curblocks >= dqblk.dqb_bsoftlimit) &&
            (qup->dqblk.dqb_bsoftlimit == 0 ||
             qup->dqblk.dqb_curblocks < qup->dqblk.dqb_bsoftlimit))
            qup->dqblk.dqb_btime = 0;

         if (dqblk.dqb_isoftlimit &&
            (qup->dqblk.dqb_curinodes >= dqblk.dqb_isoftlimit) &&
            (qup->dqblk.dqb_isoftlimit == 0 ||
             qup->dqblk.dqb_curinodes < qup->dqblk.dqb_isoftlimit))
            qup->dqblk.dqb_itime = 0;

         qup->dqblk.dqb_bsoftlimit = dqblk.dqb_bsoftlimit;
         qup->dqblk.dqb_bhardlimit = dqblk.dqb_bhardlimit;
         qup->dqblk.dqb_isoftlimit = dqblk.dqb_isoftlimit;
         qup->dqblk.dqb_ihardlimit = dqblk.dqb_ihardlimit;
         qup->flags |= FOUND;

         if (dqblk.dqb_curblocks != qup->dqblk.dqb_curblocks)
            fprintf(stderr, "%s: cannot change current block allocation\n", fsp);
	 if (dqblk.dqb_curinodes != qup->dqblk.dqb_curinodes)
            fprintf(stderr, "%s: cannot change current inode allocation\n", fsp);
      }
   }
#else
   /*
    * Discard title line, then read pairs of lines to process.
    */
   (void) fgets(line1, sizeof(line1), fd);
   while (fgets(line1, sizeof(line1), fd) != NULL &&
          fgets(line2, sizeof(line2), fd) != NULL) {
      if ((fsp = strtok(line1, " \t:")) == NULL) {
         fprintf(stderr, "%s: bad format\n", line1);
         return (0);
      }
      if ((cp = strtok((char *) 0, "\n")) == NULL) {
         fprintf(stderr, "%s: %s: bad format\n", fsp,
            &fsp[strlen(fsp) + 1]);
         return (0);
      }

      cnt = sscanf(cp, " blocks in use: %d, limits (soft = %d, hard = %d)",
                   &dqblk.dqb_curblocks, &dqblk.dqb_bsoftlimit, &dqblk.dqb_bhardlimit);
      if (cnt != 3) {
         fprintf(stderr, "%s:%s: bad format\n", fsp, cp);
         return (0);
      }

      dqblk.dqb_curblocks = btodb(dqblk.dqb_curblocks * 1024);
      dqblk.dqb_bsoftlimit = btodb(dqblk.dqb_bsoftlimit * 1024);
      dqblk.dqb_bhardlimit = btodb(dqblk.dqb_bhardlimit * 1024);

      if ((cp = strtok(line2, "\n")) == NULL) {
         fprintf(stderr, "%s: %s: bad format\n", fsp, line2);
         return (0);
      }

      cnt = sscanf(cp, "\tinodes in use: %d, limits (soft = %d, hard = %d)",
                   &dqblk.dqb_curinodes, &dqblk.dqb_isoftlimit, &dqblk.dqb_ihardlimit);
      if (cnt != 3) {
         fprintf(stderr, "%s: %s: bad format\n", fsp, line2);
         return (0);
      }

      for (qup = quplist; qup; qup = qup->next) {
         if (strcmp(fsp, qup->fsname))
            continue;
         /*
          * Cause time limit to be reset when the quota is
          * next used if previously had no soft limit or were
          * under it, but now have a soft limit and are over
          * it.
          */
         if (dqblk.dqb_bsoftlimit &&
             qup->dqblk.dqb_curblocks >= dqblk.dqb_bsoftlimit &&
             (qup->dqblk.dqb_bsoftlimit == 0 ||
              qup->dqblk.dqb_curblocks <
              qup->dqblk.dqb_bsoftlimit))
            qup->dqblk.dqb_btime = 0;

         if (dqblk.dqb_isoftlimit &&
             qup->dqblk.dqb_curinodes >= dqblk.dqb_isoftlimit &&
             (qup->dqblk.dqb_isoftlimit == 0 ||
              qup->dqblk.dqb_curinodes <
              qup->dqblk.dqb_isoftlimit))
            qup->dqblk.dqb_itime = 0;

         qup->dqblk.dqb_bsoftlimit = dqblk.dqb_bsoftlimit;
         qup->dqblk.dqb_bhardlimit = dqblk.dqb_bhardlimit;
         qup->dqblk.dqb_isoftlimit = dqblk.dqb_isoftlimit;
         qup->dqblk.dqb_ihardlimit = dqblk.dqb_ihardlimit;
         qup->flags |= FOUND;

         if (dqblk.dqb_curblocks == qup->dqblk.dqb_curblocks &&
             dqblk.dqb_curinodes == qup->dqblk.dqb_curinodes)
            break;
         fprintf(stderr,
              "%s: cannot change current allocation\n", fsp);
         break;
      }
   }
#endif
   fclose(fd);

   /*
    * Disable quotas for any filesystems that have not been found.
    */
   for (qup = quplist; qup; qup = qup->next) {
      if (qup->flags & FOUND) {
         qup->flags &= ~FOUND;
         continue;
      }
      qup->dqblk.dqb_bsoftlimit = 0;
      qup->dqblk.dqb_bhardlimit = 0;
      qup->dqblk.dqb_isoftlimit = 0;
      qup->dqblk.dqb_ihardlimit = 0;
   }
   return (1);
}

/*
 * Convert a quotause list to an ASCII file of grace times.
 */
writetimes(struct quotause *quplist, int outfd, int quotatype)
{
   struct quotause *qup;
   char *cvtstoa();
   FILE *fd;

   ftruncate(outfd, 0);
   lseek(outfd, 0, L_SET);
   if ((fd = fdopen(dup(outfd), "w")) == NULL) {
      fprintf(stderr, "edquota: ");
      perror(tmpfil);
      exit(1);
   }

#if defined(ALT_FORMAT)
   fprintf(fd, "Grace period before enforcing soft limits for %ss:\n", qfextension[quotatype]);
   fprintf(fd, "Time units may be: days, hours, minutes, or seconds\n");
   fprintf(fd, "  Filesystem             Block grace period     Inode grace period\n");

   for (qup = quplist; qup; qup = qup->next) {
      fprintf(fd, "  %-12s %22s ", qup->fsname, cvtstoa(qup->dqblk.dqb_btime));
      fprintf(fd, "%22s\n", cvtstoa(qup->dqblk.dqb_itime));
   }
#else
   fprintf(fd, "Time units may be: days, hours, minutes, or seconds\n");
   fprintf(fd, "Grace period before enforcing soft limits for %ss:\n",
      qfextension[quotatype]);
   for (qup = quplist; qup; qup = qup->next) {
      fprintf(fd, "%s: block grace period: %s, ",
         qup->fsname, cvtstoa(qup->dqblk.dqb_btime));
      fprintf(fd, "file grace period: %s\n",
         cvtstoa(qup->dqblk.dqb_itime));
   }
#endif

   fclose(fd);
   return (1);
}

/*
 * Merge changes of grace times in an ASCII file into a quotause list.
 */
readtimes(struct quotause *quplist, int infd)
{
   struct quotause *qup;
   FILE *fd;
   int cnt;
   char  *cp;
   time_t itime, btime, iseconds, bseconds;
#if defined(ALT_FORMAT)
   char fsp[BUFSIZ], bunits[10], iunits[10], line[BUFSIZ];
#else
   char *fsp, bunits[10], iunits[10], line1[BUFSIZ];
#endif

   lseek(infd, 0, L_SET);
   fd = fdopen(dup(infd), "r");
   if (fd == NULL) {
      fprintf(stderr, "Can't re-read temp file!!\n");
      return (0);
   }

#if defined(ALT_FORMAT)
   /*
    * Discard three title lines, then read lines to process.
    */
   (void) fgets(line, sizeof(line), fd);
   (void) fgets(line, sizeof(line), fd);
   (void) fgets(line, sizeof(line), fd);

   while (fgets(line, sizeof(line), fd) != NULL) {
      cnt = sscanf(line, "%s %d %s %d %s",
                   fsp, &btime, bunits, &itime, iunits);
      if (cnt != 5) {
         fprintf(stderr, "bad format:\n%s\n", line);
         return (0);
      }
#else
   /*
    * Discard two title lines, then read lines to process.
    */
   (void) fgets(line1, sizeof(line1), fd);
   (void) fgets(line1, sizeof(line1), fd);

   while (fgets(line1, sizeof(line1), fd) != NULL) {
      if ((fsp = strtok(line1, " \t:")) == NULL) {
         fprintf(stderr, "%s: bad format\n", line1);
         return (0);
      }
      if ((cp = strtok((char *) 0, "\n")) == NULL) {
         fprintf(stderr, "%s: %s: bad format\n", fsp,
            &fsp[strlen(fsp) + 1]);
         return (0);
      }
      cnt = sscanf(cp,
            " block grace period: %d %s file grace period: %d %s",
              &btime, bunits, &itime, iunits);
      if (cnt != 4) {
         fprintf(stderr, "%s:%s: bad format\n", fsp, cp);
         return (0);
      }
#endif
      if (cvtatos(btime, bunits, &bseconds) == 0)
         return (0);
      if (cvtatos(itime, iunits, &iseconds) == 0)
         return (0);
      for (qup = quplist; qup; qup = qup->next) {
         if (strcmp(fsp, qup->fsname))
            continue;
         qup->dqblk.dqb_btime = bseconds;
         qup->dqblk.dqb_itime = iseconds;
         qup->flags |= FOUND;
         break;
      }
   }
   fclose(fd);

   /*
    * reset default grace periods for any filesystems that have not been
    * found.
    */
   for (qup = quplist; qup; qup = qup->next) {
      if (qup->flags & FOUND) {
         qup->flags &= ~FOUND;
         continue;
      }
      qup->dqblk.dqb_btime = 0;
      qup->dqblk.dqb_itime = 0;
   }
   return (1);
}

/*
 * Convert seconds to ASCII times.
 */
char *cvtstoa(time_t time)
{
   static char buf[20];

   if (time % (24 * 60 * 60) == 0) {
      time /= 24 * 60 * 60;
      sprintf(buf, "%d day%s", time, time == 1 ? "" : "s");
   } else if (time % (60 * 60) == 0) {
      time /= 60 * 60;
      sprintf(buf, "%d hour%s", time, time == 1 ? "" : "s");
   } else if (time % 60 == 0) {
      time /= 60;
      sprintf(buf, "%d minute%s", time, time == 1 ? "" : "s");
   } else
      sprintf(buf, "%d second%s", time, time == 1 ? "" : "s");
   return (buf);
}

/*
 * Convert ASCII input times to seconds.
 */
cvtatos(time_t time, char *units, time_t *seconds)
{
   if (bcmp(units, "second", 6) == 0)
      *seconds = time;
   else if (bcmp(units, "minute", 6) == 0)
      *seconds = time * 60;
   else if (bcmp(units, "hour", 4) == 0)
      *seconds = time * 60 * 60;
   else if (bcmp(units, "day", 3) == 0)
      *seconds = time * 24 * 60 * 60;
   else {
      printf("%s: bad units, specify %s\n", units,
             "days, hours, minutes, or seconds");
      return (0);
   }
   return (1);
}

/*
 * Free a list of quotause structures.
 */
freeprivs(struct quotause *quplist)
{
   struct quotause *qup, *nextqup;

   for (qup = quplist; qup; qup = nextqup) {
      nextqup = qup->next;
      free(qup);
   }
}

/*
 * Check whether a string is completely composed of digits.
 */
alldigits(char *s)
{
   int c;

   c = *s++;
   do {
      if (!isdigit(c))
         return (0);
   } while (c = *s++);
   return (1);
}
