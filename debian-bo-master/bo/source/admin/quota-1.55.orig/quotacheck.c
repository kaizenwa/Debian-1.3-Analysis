/*
 * QUOTA    An implementation of the diskquota system for the LINUX operating
 *          system. QUOTA is implemented using the BSD systemcall interface
 *          as the means of communication with the user level. Should work for
 *          all filesystems because of integration into the VFS layer of the
 *          operating system. This is based on the Melbourne quota system wich
 *          uses both user and group quota files.
 * 
 *          Program to check disk quotas.
 * 
 * Authors:
 *          Disk reading routines: Edvard Tuinder <ed@ow.org> <ed@tnix.net>
 *          Quota storing routines: Marco van Wieringen <mvw@planets.ow.org> <mvw@tnix.net>
 *
 *          This program is free software; you can redistribute it and/or
 *          modify it under the terms of the GNU General Public License as
 *          published by the Free Software Foundation; either version 2 of
 *          the License, or (at your option) any later version.
 */

#include <sys/types.h>
#include <sys/param.h>
#include <dirent.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include <linux/quota.h>
#include <stdarg.h>
#include <sys/file.h>
#include <mntent.h>
#include <getopt.h>
#include <limits.h>
#include <unistd.h>
#include <stdlib.h>

#if defined(EXT2_DIRECT)
#include <linux/ext2_fs.h>
#include <ext2fs/ext2fs.h>
#endif

#define DEF_BLOCKSIZE 1024
#define NODQUOT (struct dquot *)NULL

#ifndef lint
static char RCS_checkquota[] = "$Id: quotacheck.c,v 2.12 1995/07/23 09:58:06 mvw Exp mvw $";
#endif

struct dquot {
   int dq_id;  /* id this applies to (uid, gid) */
   struct dqblk dq_dqb; /* diskquota for id */
   struct dquot *next;  /* pointer to next id */
};

struct dlinks {
   ino_t i_num;
   __u32 id;
   __u32 blocks;
   struct dlinks *next;
};

struct dirs {
   char *dir_name;
   struct dirs *next;
};

void add_to_quota(int, ino_t, uid_t, gid_t, umode_t, nlink_t, __u32);
void store_dlinks(int, uid_t, gid_t, ino_t, __u32);
void dump_to_file(char *, char *, int);
void remove_list(void);
void scan_dir(char *);
void scan_fs(char *);
void abort_now(const char *, const char *, ...);
void add_dlinks(__u32 *, __u32 *, int, int);
#if defined(EXT2_DIRECT)
void ext2_direct_scan(char *device);
#endif

static char bits[] = "|/-\\";
#define BITS_SIZE 4 /* sizeof(bits) == 5 */

dev_t cur_dev;
char dflag = 0, vflag = 0;
char check_usr, check_grp;
__u32 files_done, dirs_done;
__u32 highestid[MAXQUOTAS];
char *quotatypes[] = INITQFNAMES;
char log_buf[16384]; /* for dflag stderr buffering */
#ifdef DEBUG_MALLOC
static size_t malloc_mem = 0;
static size_t free_mem = 0;
#endif

struct dquot *dquot_list[MAXQUOTAS] = {NODQUOT, NODQUOT};
struct dquot *mru_dquot[MAXQUOTAS] = {NODQUOT, NODQUOT};
struct dlinks *stored_links[MAXQUOTAS] = {(struct dlinks *)NULL, (struct dlinks *)NULL};

/*
 * Ok check each memory allocation.
 */
void *xmalloc(size_t size)
{
   void *ptr;

#ifdef DEBUG_MALLOC
   malloc_mem += size;
#endif
   ptr = malloc(size);
   if (ptr == (void *)NULL)
      abort_now("xmalloc", "Virtual memory exhausted\n");
   memset(ptr, 0, size);
   return(ptr);
}

/*
 * Do a lookup of a type of quota for a specific id. Use short cut with
 * most recently used dquot struct pointer.
 */
static inline struct dquot *lookup_dquot(int id, int type)
{
   register struct dquot *lptr = NODQUOT;

   /*
    * First fast lookup when same as used before.
    */
   if (mru_dquot[type] != NODQUOT && mru_dquot[type]->dq_id == id)
      return (mru_dquot[type]);

   for (lptr = dquot_list[type]; lptr != NODQUOT; lptr = lptr->next)
      if (lptr->dq_id == id) {
         mru_dquot[type] = lptr;
         return (lptr);
      }
   return(NODQUOT);
}

/*
 * Add a new dquot for a new id to the list.
 */
static inline struct dquot *add_dquot(int id, int type)
{
   register struct dquot *lptr;

   if (dflag)
      fprintf(stderr, "Adding dquot structure type %s for %d\n",
	      quotatypes[type], id);

   lptr = (struct dquot *)xmalloc(sizeof(struct dquot));

   lptr->dq_id = id;
   lptr->next = dquot_list[type];
   dquot_list[type] = lptr;
   lptr->dq_btime = lptr->dq_itime = (time_t) 0;

   if (id > highestid[type])
      highestid[type] = id;

   return(lptr);
}

/*
 * Check the programs arguments for a specific target.
 */
int oneof(char *target, char *list[], int cnt)
{
   register int i;

   for (i = 0; i < cnt; i++)
      if (strcmp(target, list[i]) == 0)
         return (i);
   return(-1);
}

/*
 * Show a blitting cursor as means of visual progress indicator.
 */
static inline void blit()
{
   static short bitc = 0;

   putc(bits[bitc], stdout);
   putc('\b', stdout);
   bitc++;
   bitc %= BITS_SIZE;
}

void usage()
{
   fputs("Usage:\n\tquotacheck [-g] [-u] [-vd] -a\n", stderr);
   fputs("\tquotacheck [-g] [-u] [-vd] filesys ...\n", stderr);
   exit(1);
}

int main(int argc, char **argv)
{
   FILE *fp;
   int cnt, ch;
   struct stat st;
   char aflag = 0, gflag = 0, uflag = 0;
   unsigned done;
   int argnum;
   char *usr_qfnp, *grp_qfnp;
   register struct mntent *mnt;

   while ((ch = getopt(argc, argv, "avugd")) != EOF) {
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
         case 'd':
            dflag++;
	    setbuf(stderr, log_buf);
	    break;
         case 'v':
            vflag++;
	    setbuf(stdout, (char *)NULL);
            break;
         default:
            usage();
      }
   }
   argc -= optind;
   argv += optind;

   if (vflag && dflag)
     vflag = 0;

   if (!uflag && !gflag)
      uflag++;

   if (!aflag && argc == 0)
      usage();

   fp = setmntent(MNTTAB, "r");
   while ((mnt = getmntent(fp)) != (struct mntent *) NULL) {
      check_usr = check_grp = 0;

      if (!aflag) { /* .. or: argc != 0 */
	/* If the user specified a fs/part name, then check only
	   those, and skip all others */
	if (((argnum = oneof(mnt->mnt_dir, argv, argc)) >= 0) ||
	    ((argnum = oneof(mnt->mnt_fsname, argv, argc)) >= 0)) {
	  done |= (1<<argnum);
	} else
	  continue;
      } else {
	/* If 'all', then skip every fs, which is not marked as 'auto'
	   AND user or group quota */
	if (hasmntopt(mnt, MNTOPT_NOAUTO) ||
	    hasmntopt(mnt, MNTOPT_NOQUOTA))
	  continue;
      }

      if (gflag && hasquota(mnt, GRPQUOTA, &grp_qfnp))
         check_grp++;
      if (uflag && hasquota(mnt, USRQUOTA, &usr_qfnp))
         check_usr++;
      if (check_usr || check_grp) {
	 if ((lstat(mnt->mnt_dir, &st)) == -1) {
	   fprintf(stderr, "%s: not found\n", mnt->mnt_dir);
	   perror("lstat");
           continue;
	 }

	 if (vflag)
	   fprintf(stdout,"Scanning %s [%s] ", mnt->mnt_fsname, mnt->mnt_dir);

	 if (S_ISDIR(st.st_mode)) {
           cur_dev = st.st_dev;
           files_done = dirs_done = 0;
           if (check_usr)
              add_to_quota(USRQUOTA, st.st_ino, st.st_uid, st.st_gid, st.st_mode,
                           st.st_nlink, st.st_blocks);
           if (check_grp)
              add_to_quota(GRPQUOTA, st.st_ino, st.st_uid, st.st_gid, st.st_mode,
                           st.st_nlink, st.st_blocks);

#if defined(EXT2_DIRECT)
           if (!strcmp(mnt->mnt_type, MNTTYPE_EXT2)) {
              ext2_direct_scan(mnt->mnt_fsname);
           } else if (mnt->mnt_dir) {
#else
           if (mnt->mnt_dir) {
#endif
	      scan_dir(mnt->mnt_dir);
           }
	   dirs_done++;
	   if (vflag)
	      fputs("done\n", stderr);
           if (vflag || dflag)
              fprintf(stderr, "Checked %d directories and %d files\n",
                      dirs_done, files_done);
	 } else {
	   fprintf(stderr, "%s: not a directory\n", mnt->mnt_dir);
	   exit(0);
	 }

         if (check_usr)
            dump_to_file(mnt->mnt_fsname, usr_qfnp, USRQUOTA);
         if (check_grp)
            dump_to_file(mnt->mnt_fsname, grp_qfnp, GRPQUOTA);
         remove_list();
      }
   }
   endmntent(fp);

   for (cnt = 0; cnt < argc; cnt++)
      if ((done & (1 << cnt)) == 0)
         fprintf(stderr, "%s not found in fstab\n", argv[cnt]);

#ifdef DEBUG_MALLOC
   fprintf (stderr, "Allocated %d bytes memory\nFree'd %d bytes\nLost %d bytes\n", malloc_mem,
	free_mem, malloc_mem - free_mem);
#endif
   exit(0);
}

#if defined(EXT2_DIRECT)
void ext2_direct_scan(char *device)
{
   ino_t i_num;
   ext2_filsys fs;
   errcode_t error;
   ext2_inode_scan scan;
   struct ext2_inode inode;
   int inode_buffer_blocks = 0;
   ext2fs_inode_bitmap inode_used_map;
   ext2fs_inode_bitmap inode_dir_map;

   if ((error = ext2fs_open(device, 0, 0, 0, unix_io_manager, &fs))) {
      fprintf(stderr, "quotacheck: error while opening %s\n", device);
      exit (0);
   }

   if ((error = ext2fs_allocate_inode_bitmap(fs, "in-use inode map", &inode_used_map))) {
      fprintf (stderr, "quotacheck: error while allocating inode file bitmap\n");
      exit (0);
   }

   if ((error = ext2fs_allocate_inode_bitmap(fs, "directory inode map", &inode_dir_map))) {
      fprintf (stderr, "quotacheck: error while allocating inode directory bitmap\n");
      exit (0);
   }

   if ((error = ext2fs_open_inode_scan(fs, inode_buffer_blocks, &scan))) {
      fprintf(stderr, "quotacheck: error while opening inode scan\n");
      exit (0);
   }

   if ((error = ext2fs_get_next_inode(scan, &i_num, &inode))) {
      fprintf(stderr, "quotacheck: error while starting inode scan\n");
      exit (0);
   }

   while (i_num) {
      if (inode.i_links_count) {
         if (dflag)
	    printf ("Found i_num %ld\n", i_num);
         if (vflag)
	    blit();
         if (check_usr)
	    add_to_quota(USRQUOTA, i_num, inode.i_uid, inode.i_gid, inode.i_mode,
                         inode.i_links_count, inode.i_blocks);
         if (check_grp)
	    add_to_quota(GRPQUOTA, i_num, inode.i_uid, inode.i_gid, inode.i_mode,
                         inode.i_links_count, inode.i_blocks);
         if (S_ISDIR(inode.i_mode))
	    dirs_done++;
         else
	    files_done++;
      }

      if ((error = ext2fs_get_next_inode(scan, &i_num, &inode))) {
         fprintf (stderr, "Something weird while scanning\n");
         exit (0);
      }
   }
}
#endif

/*
 * Scan a directory with the readdir systemcall. Stat the files and add the sizes
 * of the files to the appropriate quotas. When we find a dir we recursivly call
 * ourself to scan that dir.
 */
void scan_dir(char *pathname)
{
   struct dirs *dir_stack = {(struct dirs *)NULL};
   struct dirs *new_dir;
   struct dirent *de;
   struct stat st;
   DIR *dp;

   if ((dp = opendir(pathname)) == (DIR *)NULL)
      abort_now("opendir", "\n%s\n", pathname);

   chdir(pathname);
   while ((de = readdir(dp)) != (struct dirent *)NULL) {
      if (!strcmp(de->d_name, ".") || !strcmp(de->d_name, ".."))
         continue;
      if (vflag)
         blit();

      if ((lstat(de->d_name, &st)) == -1) {
	 fprintf(stderr, "Hmm, file `%s/%s' not found\n", pathname, de->d_name);
         fputs("Guess you'd better run fsck first !\nexiting...\n", stderr);
         perror("lstat");
         exit(1);
      }

      if (check_usr)
         add_to_quota(USRQUOTA, st.st_ino, st.st_uid, st.st_gid, st.st_mode,
                      st.st_nlink, st.st_blocks);
      if (check_grp)
         add_to_quota(GRPQUOTA, st.st_ino, st.st_uid, st.st_gid, st.st_mode,
                      st.st_nlink, st.st_blocks);

      if (S_ISDIR(st.st_mode)) {
         if (st.st_dev != cur_dev)
            continue;
         /*
          * Add this to the directory stack and check this later on.
          */
	 if (dflag)
	    fprintf(stderr, "pushd %s/%s\n", pathname, de->d_name);
         new_dir = xmalloc(sizeof(struct dirs));
         new_dir->dir_name = xmalloc(strlen(pathname) + strlen(de->d_name) + 2);
         sprintf(new_dir->dir_name, "%s/%s", pathname, de->d_name);
         new_dir->next = dir_stack;
         dir_stack = new_dir;
      } else {
	 if (dflag)
	    fprintf(stderr, "\tAdding %s size %d ino %d links %d\n", de->d_name,
		    st.st_size, st.st_ino, st.st_nlink);
         files_done++;
      }
   }
   closedir(dp);

   /*
    * Traverse the directory stack, and check it.
    */
   if (dflag)
      fputs("Scanning stored directories from directory stack\n", stderr);
   while (dir_stack != (struct dirs *)NULL) {
      new_dir = dir_stack;
      dir_stack = dir_stack->next;
      if (dflag)
         fprintf(stderr, "popd %s\nEntering directory %s\n",
                 new_dir->dir_name, new_dir->dir_name);
      scan_dir(new_dir->dir_name);
      dirs_done++;
#ifdef DEBUG_MALLOC
      free_mem += sizeof(struct dirs) + strlen(new_dir->dir_name) + 1;
#endif
      free(new_dir->dir_name);
      free(new_dir);
   }
   if (dflag)
      fprintf(stderr, "Leaving %s\n", pathname);
}
    
/*
 * Store a hardlinked file for later. Add the end we add this to a users
 * quota because we don't wanna count it more then ones.
 */
void store_dlinks(int type, uid_t i_uid, gid_t i_gid, ino_t i_num, __u32 i_blocks)
{
   struct dlinks *lptr;

   if (dflag)
      fprintf(stderr, "Adding hardlink for ino %d\n", i_num);

   for (lptr = stored_links[type]; lptr != (struct dlinks *)NULL; lptr = lptr->next)
      if (lptr->i_num == i_num)
         return;

   lptr = (struct dlinks *)xmalloc(sizeof(struct dlinks));
   if (type == USRQUOTA)
     lptr->id = i_uid;
   else 
     lptr->id = i_gid;
   lptr->i_num = i_num;
   lptr->blocks = i_blocks;
   lptr->next = stored_links[type];
   stored_links[type] = lptr;
}

/*
 * Add a number of blocks and inodes to a quota.
 */
void add_to_quota(int type, ino_t i_num, uid_t i_uid, gid_t i_gid, umode_t i_mode,
                  nlink_t i_nlink, __u32 i_blocks)
{
   int wanted;
   struct dquot *lptr;

   /*
    * All blocks reported by i_blocks or st_blocks are 512 Bytes blocks. We calculate
    * in 1024 Bytes blocks so...
    */
   i_blocks /= 2;

   switch(type)
   {
      case USRQUOTA:
         wanted = i_uid;
         break;
      case GRPQUOTA:
         wanted = i_gid;
         break;
      default:
         return;
   }

   if ((lptr = lookup_dquot(wanted, type)) == NODQUOT)
      if ((lptr = add_dquot(wanted, type)) == NODQUOT)
	 abort_now("add_to_quota", "Can't add dquot structure type %s for uid %d\n",
		   quotatypes[type], wanted);

   if (i_nlink != 1) {
      store_dlinks(type, i_uid, i_gid, i_num, i_blocks);
      return;
   }
   lptr->dq_curinodes++;
   if (i_blocks)
      lptr->dq_curblocks += i_blocks;
}

void abort_now(const char *perror_mes, const char *fmt, ...)
{
   va_list args;

   va_start(args, fmt);
   vfprintf(stderr, fmt, args);
   va_end(args);

   fflush(stderr);
   perror(perror_mes);
   exit(-1);
}

void add_dlinks(__u32 * inodes, __u32 * blocks, int id, int type)
{
   struct dlinks  *lptr;

   if (dflag)
      fprintf(stderr, "Adding blocks from hardlinks for %s %d\n",
              quotatypes[type], id);

   for (lptr = stored_links[type]; lptr != (struct dlinks *)NULL; lptr = lptr->next) {
      if (lptr->id == id) {
         (*inodes)++;
         if (lptr->blocks)
            *blocks += lptr->blocks;
         files_done++;
      }
   }
}

/*
 * Clean up all list from a previous run.
 */
void remove_list()
{
   int cnt;
   struct dquot *dquot, *dquot_free;
   struct dlinks *dlink, *dlink_free;

   for (cnt = 0; cnt < MAXQUOTAS; cnt++) {
      if (dquot_list[cnt] != NODQUOT) {
         dquot = dquot_list[cnt];
         while (dquot != NODQUOT) {
            dquot_free = dquot;
            dquot = dquot->next;
#ifdef DEBUG_MALLOC
	    free_mem += sizeof(struct dquot);
#endif
            free(dquot_free);
         }
         mru_dquot[cnt] = NODQUOT;
      }
      dquot_list[cnt] = NODQUOT;
      if (stored_links[cnt] != (struct dlinks *)NULL) {
         dlink = stored_links[cnt];
         while (dlink != (struct dlinks *)NULL) {
            dlink_free = dlink;
            dlink = dlink->next;
#ifdef DEBUG_MALLOC
	    free_mem += sizeof(struct dlinks);
#endif
            free(dlink_free);
         }
      }
      stored_links[cnt] = (struct dlinks *)NULL;
   }
}

/*
 * Dump the quota info that we have in memory now to the appropriate
 * quota file. We lock it during the time we update it.
 */
void dump_to_file(char *fsname, char *quotafile, int type)
{
   struct dqblk dq_dqb;
   struct dquot *dquot;
   struct stat st;
   int quota_enabled = 0, max_id;
   int fd, id = 0;

   if (vflag || dflag)
      fprintf(stderr, "Using quotafile %s\n", quotafile);

   if (quotactl(QCMD(Q_GETQUOTA, type), fsname, 0, (caddr_t)&dq_dqb) == 0)
      quota_enabled = 1;

   if ((vflag || dflag) && quota_enabled)
      fprintf(stderr, "Updating in-core %s quotas\n", quotatypes[type]);

   if ((fd = open(quotafile, O_RDWR | O_CREAT, 0600)) < 0)
      abort_now("open", "dump_to_file(%s): ", quotafile);

   if (flock(fd, LOCK_EX) < 0)
      abort_now("flock", "dump_to_file(%s): ", quotafile);

   /*
    * First dump the gracetimes that are always a first in the 
    * quotafile. Only dump new gracetimes when creating a new 
    * quotafile.
    */
   if (read(fd, &dq_dqb, sizeof(struct dqblk)) <= 0) {
      memset((caddr_t *)&dq_dqb, 0, sizeof(struct dqblk));
      dq_dqb.dqb_btime = MAX_DQ_TIME;
      dq_dqb.dqb_itime = MAX_IQ_TIME;
      write(fd, &dq_dqb, sizeof(struct dqblk));
   }

   if (fstat(fd, &st) == 0) {
      max_id = (st.st_size / sizeof(struct dqblk)) - 1;
      if (max_id < highestid[type])
         max_id = highestid[type];
   } else
      max_id = highestid[type];

   for (id = 0; id <= max_id; id++) {
      memset((caddr_t)&dq_dqb, 0, sizeof(struct dqblk));

      if (lseek(fd, dqoff(id), SEEK_SET) == dqoff(id))
         read(fd, &dq_dqb, sizeof(struct dqblk));

      if ((dquot = lookup_dquot(id, type)) != NODQUOT) {
         dq_curinodes = dquot->dq_curinodes;
         dq_curblocks = dquot->dq_curblocks;
         if (dflag)
            fprintf(stderr, "%s %d: curinodes: %d curblocks: %d without hardlinks\n",
                    quotatypes[type], id, dq_curinodes, dq_curblocks);
         add_dlinks((__u32 *)&dq_curinodes, (__u32 *)&dq_curblocks, id, type);
         if (dflag)
            fprintf(stderr, "%s %d: curinodes: %d curblocks: %d with hardlinks\n",
                    quotatypes[type], id, dq_curinodes, dq_curblocks);
      } else {
         if (dq_curinodes == 0 && dq_curblocks == 0)
            continue;
         dq_curinodes = 0;
         dq_curblocks = 0;
      }

      /*
       * If the quota is updated with the systemcall it isn't needed to update
       * it in the file. Because the kernel will do that with the next sync.
       */
      if (quota_enabled)
         if (quotactl(QCMD(Q_SETUSE, type), fsname, id, (caddr_t)&dq_dqb) == 0)
            continue;

      if (lseek(fd, dqoff(id), SEEK_SET) == dqoff(id))
         write(fd, &dq_dqb, sizeof(struct dqblk));
   }
   flock(fd, LOCK_UN);
   close(fd);
}
