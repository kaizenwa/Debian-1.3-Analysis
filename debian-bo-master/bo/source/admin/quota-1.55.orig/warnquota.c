/*
 * QUOTA    An implementation of the diskquota system for the LINUX operating
 *          system. QUOTA is implemented using the BSD systemcall interface
 *          as the means of communication with the user level. Should work for
 *          all filesystems because of integration into the VFS layer of the
 *          operating system. This is based on the Melbourne quota system wich
 *          uses both user and group quota files.
 * 
 *          Program to mail to users that they are over there quota.
 * 
 * Author:  Marco van Wieringen <mvw@planets.ow.nl> <mvw@tnix.net>
 *
 * Version: $Id: warnquota.c,v 1.4 1995/07/23 09:58:06 mvw Exp mvw $
 *
 *          This program is free software; you can redistribute it and/or
 *          modify it under the terms of the GNU General Public License as
 *          published by the Free Software Foundation; either version 2 of
 *          the License, or (at your option) any later version.
 */

#include <sys/types.h>
#include <sys/param.h>
#include <stdio.h>
#include <linux/quota.h>
#include <stdarg.h>
#include <fcntl.h>
#include <mntent.h>
#include <limits.h>
#include <unistd.h>
#include <stdlib.h>
#include <pwd.h>

#define MAIL_CMD "/usr/lib/sendmail -t"
#define FROM     "root"
#define SUBJECT  "Quota usage on system"
#define CC_TO    "root"

#define DEF_MESSAGE "Hi,\n\nWe noticed that you are in violation with the quotasystem\n\
used on this system. We have found the following violations:\n"

#define DEF_SIGNATURE "\nWe hope that you will cleanup before your grace period expires\n\
\nroot (System Super User).\n"

struct usage
{
   char *devicename;
   struct dqblk dq_dqb;
   struct usage *next;
};

struct offenderlist
{
   int offender_id;
   char *offender_name;
   struct usage *usage;
   struct offenderlist *next;
};

/*
 * Global pointers to list.
 */
static struct offenderlist *offenders = (struct offenderlist *)0;

void *xmalloc(size_t size)
{
   void *new;

   if ((new = (void *)malloc(size)) == (void *)0) {
      fprintf(stderr, "Virtual memory exhausted\n");
      exit(1);
   }
   return(new);
}

struct offenderlist *add_offender(int id)
{
   struct passwd *pwd;
   struct group *grp;
   struct offenderlist *offender;

   offender = (struct offenderlist *)xmalloc(sizeof(struct offenderlist));
   offender->offender_id = id;
   if ((pwd = getpwuid(id)) == (struct passwd *)0)
      return ((struct offenderlist *)0);
   offender->offender_name = (char *)xmalloc(strlen(pwd->pw_name) + 1);
   strcpy(offender->offender_name, pwd->pw_name);
   offender->next = offenders;
   offenders = offender;
   return offender;
}

void add_offence(int id, char *devicename, struct dqblk *used)
{
   struct offenderlist *lptr;
   struct usage *usage;

   for (lptr = offenders; lptr != (struct offenderlist *)0; lptr = lptr->next)
      if (lptr->offender_id == id)
         break;

   if (lptr == (struct offenderlist *)0)
      if ((lptr = add_offender(id)) == (struct offenderlist *)0)
         return;

   usage = (struct usage *)xmalloc(sizeof(struct usage));
   memcpy(&usage->dq_dqb, used, sizeof(struct dqblk));
   usage->devicename = xmalloc(strlen(devicename) + 1);
   strcpy(usage->devicename, devicename);
   /*
    * Stuff it in front
    */
   usage->next = lptr->usage;
   lptr->usage = usage;
}

void read_quotafile(char *qfilename, char *devicename)
{
   int fd, id = 0;
   struct dqblk dq_dqb;

   if ((fd = open(qfilename, O_RDONLY)) < 0) {
      perror("open");
      exit(1);
   }

   while (read(fd, &dq_dqb, sizeof(struct dqblk)) == sizeof(struct dqblk)) {
      if ((dq_bsoftlimit && dq_curblocks >= dq_bsoftlimit) ||
          (dq_isoftlimit && dq_curinodes >= dq_isoftlimit))
         add_offence(id, devicename, &dq_dqb);
      id++;
   }
   close(fd);
}

/*
 * Calculate the grace period and return a printable string for it.
 */
char *timeprt(time_t seconds)
{
   time_t hours, minutes;
   static char buf[20];
   static time_t now;

   if (now == 0)
      time(&now);
   if (now > seconds)
      return ("none");
   seconds -= now;
   minutes = (seconds + 30) / 60;
   hours = (minutes + 30) / 60;
   if (hours >= 36) {
      sprintf(buf, "%ddays", (hours + 12) / 24);
      return (buf);
   }
   if (minutes >= 60) {
      sprintf(buf, "%2d:%d", minutes / 60, minutes % 60);
      return (buf);
   }
   sprintf(buf, "%2d", minutes);
   return (buf);
}

void mail_user(struct offenderlist *offender)
{
   struct usage *lptr;
   FILE *fp;

   if ((fp = popen(MAIL_CMD, "w")) != (FILE *)0) {
      fprintf(fp, "From: %s\n", FROM);
      fprintf(fp, "Subject: %s\n", SUBJECT);
      fprintf(fp, "To: %s\n", offender->offender_name);
      fprintf(fp, "Cc: %s\n", CC_TO);
      fprintf(fp, DEF_MESSAGE);
      fprintf(fp, "\n                        Block limits               File limits\n");
      fprintf(fp, "Filesystem           used    soft    hard  grace    used  soft  hard  grace\n");
      for (lptr = offender->usage; lptr != (struct usage *)0; lptr = lptr->next) {
         fprintf(fp, "%-15s", lptr->devicename);
         fprintf(fp, "%c%c%8d%8d%8d%7s",
                lptr->dq_bsoftlimit &&
                lptr->dq_curblocks >=
                lptr->dq_bsoftlimit ? '+' : '-',
                lptr->dq_isoftlimit &&
                lptr->dq_curinodes >=
                lptr->dq_isoftlimit ? '+' : '-',
                dbtob(lptr->dq_curblocks) / 1024,
                dbtob(lptr->dq_bsoftlimit) / 1024,
                dbtob(lptr->dq_bhardlimit) / 1024,
                lptr->dq_bsoftlimit &&
                lptr->dq_curblocks >=
                lptr->dq_bsoftlimit ?
                timeprt(lptr->dq_btime) : "");
         fprintf(fp, "  %6d%6d%6d%7s\n",
                lptr->dq_curinodes,
                lptr->dq_isoftlimit,
                lptr->dq_ihardlimit,
                lptr->dq_isoftlimit &&
                lptr->dq_curinodes >=
                lptr->dq_isoftlimit ?
                timeprt(lptr->dq_itime) : "");
      }
      fprintf(fp, "\n%s\n", DEF_SIGNATURE);
      fclose(fp);
   }
}

void mail_to_offenders()
{
   struct offenderlist *lptr;
   struct usage *used;
   /*
    * Dump offenderlist.
    */
   for (lptr = offenders; lptr != (struct offenderlist *)0; lptr = lptr->next)
      mail_user(lptr);
}

warn_quota()
{
   FILE *fp;
   struct mntent *mnt;
   char *qfilename;

   fp = setmntent(MNTTAB, "r");
   while ((mnt = getmntent(fp)) != (struct mntent *)0) {
      if (hasquota(mnt, USRQUOTA, &qfilename)) {
         /*
          * First sync quotafile to disk.
          */
         quotactl(QCMD(Q_SYNC, USRQUOTA), mnt->mnt_fsname, 0, (caddr_t)0);

         read_quotafile(qfilename, mnt->mnt_fsname);
      }
   }
   endmntent(fp);

   mail_to_offenders();
}

main(int argc, char **argv)
{
   warn_quota(USRQUOTA);
}
