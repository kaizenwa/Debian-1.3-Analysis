/* Unix system calls */
/* Copyright (c) 1992, 1995 John E. Davis
 * All rights reserved.
 * 
 * You may distribute under the terms of either the GNU General Public
 * License or the Perl Artistic License.
 */


#include "config.h"

#if defined (__EMX__)
# include <io.h>		       /* for chmod */
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include <time.h>

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#include <errno.h>
#include <string.h>

#include "slang.h"

/* map value of errno to standard slang return values. */
static int map_errno(void)
{
   switch (errno)
     {
      case EACCES: return -1;	       /* insufficient privilege */
      case ENOENT: return -2;	       /* invalid path */
      default: return -50;
     }
}



#define EQS(a, b) ((*(a) == *(b)) && (!strcmp(a + 1, b + 1)))

static struct stat Stat_Buf;

static int parse_stat(struct stat *s, char *f)
{
   if (!strncmp(f, "st_", 3)) f += 3;
   
   if (EQS("dev", f)) return s->st_dev;
   if (EQS("ino", f)) return s->st_ino;
   if (EQS("mode", f)) return s->st_mode;
   if (EQS("nlink", f)) return s->st_nlink;
   if (EQS("uid", f)) return s->st_uid;
   if (EQS("gid", f)) return s->st_gid;
   if (EQS("rdev", f)) return s->st_rdev;
   if (EQS("size", f)) return s->st_size;
   if (EQS("atime", f)) return s->st_atime;
   if (EQS("mtime", f)) return s->st_mtime;
   if (EQS("ctime", f)) return s->st_ctime;
   SLang_doerror("Unknown stat field.");
   return (0);
}


static int unix_stat_file(char *file)
{
   if (!stat (file, &Stat_Buf)) return 0;
   return map_errno();
}

#if !defined (__GO32__) && !defined (__EMX__)
static int unix_lstat_file(char *file)
{
#ifdef __BEOS__
   SLang_doerror ("System does not support links.");
   return -1;
#else
   if (!lstat (file, &Stat_Buf)) return 0;
   return map_errno();
#endif
}
#endif

static int unix_stat_struct(char *field)
{
   return parse_stat(&Stat_Buf, field);
}

/* Well, it appears that on some systems, these are not defined.  Here I
 * provide them.  These are derived from the Linux stat.h file.
 */

#ifndef S_ISLNK
# ifdef S_IFLNK
#   define S_ISLNK(m) (((m) & S_IFMT) == S_IFLNK)
# else
#   define S_ISLNK(m) 0
# endif
#endif

#ifndef S_ISREG
# ifdef S_IFREG
#   define S_ISREG(m) (((m) & S_IFMT) == S_IFREG)
# else
#   define S_ISREG(m) 0
# endif
#endif

#ifndef S_ISDIR
# ifdef S_IFDIR
#   define S_ISDIR(m) (((m) & S_IFMT) == S_IFDIR)
# else
#   define S_ISDIR(m) 0
# endif
#endif

#ifndef S_ISCHR
# ifdef S_IFCHR
#   define S_ISCHR(m) (((m) & S_IFMT) == S_IFCHR)
# else
#   define S_ISCHR(m) 0
# endif
#endif

#ifndef S_ISBLK
# ifdef S_IFBLK
#   define S_ISBLK(m) (((m) & S_IFMT) == S_IFBLK)
# else
#   define S_ISBLK(m) 0
# endif
#endif

#ifndef S_ISFIFO
# ifdef S_IFIFO
#   define S_ISFIFO(m) (((m) & S_IFMT) == S_IFIFO)
# else
#   define S_ISFIFO(m) 0
# endif
#endif

#ifndef S_ISSOCK
# ifdef S_IFSOCK
#   define S_ISSOCK(m) (((m) & S_IFMT) == S_IFSOCK)
# else
#   define S_ISSOCK(m) 0
# endif
#endif

static int stat_is (char *what)
{
   if (!strcmp (what, "sock")) return S_ISSOCK(Stat_Buf.st_mode);
   if (!strcmp (what, "fifo")) return S_ISFIFO(Stat_Buf.st_mode);
   if (!strcmp (what, "blk")) return S_ISBLK(Stat_Buf.st_mode);
   if (!strcmp (what, "chr")) return S_ISCHR(Stat_Buf.st_mode);
   if (!strcmp (what, "dir")) return S_ISDIR(Stat_Buf.st_mode);
   if (!strcmp (what, "reg")) return S_ISREG(Stat_Buf.st_mode);
   if (!strcmp (what, "lnk")) return S_ISLNK(Stat_Buf.st_mode);
   SLang_doerror ("stat_is: Unrecognized type.");
   return 0;
}

static int unix_chmod(char *file, int *mode)
{
   if (!chmod(file, (mode_t) *mode)) return 0;
   return map_errno();
}

static int unix_chown(char *file, int *owner, int *group)
{
   if (!chown(file, (uid_t) *owner, (gid_t) *group)) return 0;
   return map_errno();
}

/* This is mainly designed to check pids, but who knows.... */
static int unix_kill(void)
{
   int pid, sig;
   
   if (SLang_pop_integer(&sig) || SLang_pop_integer(&pid)) return (-1);
   return kill ((pid_t) pid, sig);
}


static char *unix_ctime(int *tt)
{
   char *t;
   
   t = ctime ((time_t *) tt);
   t[24] = 0;  /* knock off \n */
   return (t);
}


static SLang_Name_Type slunix_table[] =
{
   MAKE_INTRINSIC(".unix_kill", unix_kill, INT_TYPE, 0),
   /* Prototype: Integer unix_kill(Integer pid, Integer sig);
    * This function may be used to send a signal given by the integer @sig@
    * to the process specified by @pid@.  The function returns zero upon
    * sucess and @-1@ upon failure.
    */
   MAKE_INTRINSIC(".unix_ctime", unix_ctime, STRING_TYPE, 1),
   /* Prototype: String unix_ctime(Integer secs);
    * This function returns a string representation of the time as given 
    * by @secs@ seconds since 1970. 
    */
#if !defined (__GO32__) && !defined (__EMX__) 
   MAKE_INTRINSIC(".lstat_file", unix_lstat_file, INT_TYPE, 1),
   /* Prototype: Integer lstat_file(String file);
    * This function is like @stat_file@ but if @file@ is a symbolic link,
    * this @lstat_file@ returns information about the link itself.  
    * See the documentation for @stat_file@ for more information.
    * Related Functions: @stat_file@ 
    */
#endif
   MAKE_INTRINSIC(".stat_file", unix_stat_file, INT_TYPE, 1),
   /* Prototype: Integer stat_file(String file);
    * This function returns information about @file@ through the use of the
    * system @stat@ call.  If the stat call fails, the function returns a
    * negative integer.  If it is successful, it returns zero.  Upon failure
    * it returns a negative number. To retrieve information obtained by this
    * this function call, use the @stat_struct@ function. 
    * Related Functions: @lstat_file@, @stat_struct@
    */
   MAKE_INTRINSIC(".stat_struct", unix_stat_struct, INT_TYPE, 1),
   /* Prototype: Integer stat_struct(String field);
    * This functions returns information obtained by the most recent call to
    * the @stat_file@ or @lstat_file@ functions.  The @field@ argument
    * specifies what piece of information to return.  Valid values for
    * @field@ are:
    * @ "dev"
    * @ "ino"
    * @ "mode"
    * @ "nlink"
    * @ "uid"
    * @ "gid"
    * @ "rdev"
    * @ "size"
    * @ "atime"
    * @ "mtime"
    * @ "ctime"
    * See the man page for @stat@ for a discussion of these fields.
    * Example:
    * @ define file_size (file)
    * @ {
    * @    if (stat_file(file) < 0) error ("Unable to stat file!");
    * @    return stat_struct("size");
    * @ }
    * Related Functions: @lstat_file@, @stat_file@, @stat_mode_parse@
    */
     MAKE_INTRINSIC(".stat_is", stat_is, INT_TYPE, 1),
     /* Prototype: Integer stat_is (String type);
      * This function returns a boolean value about the type of file specified
      * in the last call to @stat_file@.  Specifically, @type@ must be one of 
      * the strings:
      * @ "sock"     (socket)
      * @ "fifo"     (fifo)
      * @ "blk"      (block device)
      * @ "chr"      (character device)
      * @ "reg"      (regular file)
      * @ "lnk"      (link)
      * @ "dir"      (dir)
      * Related Functions: @stat_file@, @stat_struct@
      */
   MAKE_INTRINSIC(".chown", unix_chown, INT_TYPE, 3),
   /* Prototype: Integer chown(String file, Integer uid, Integer gid);
    * This function may be used to change the ownership of @file@ to that of
    * user id @uid@ and group id @gid@.  This function returns 0 upon
    * success and a negative number up failure.  Specifically, it returns -1
    * if the process does not have sufficent privileges to change the
    * ownership, or -2 if the file does not exist. 
    * Related Functions: @chmod@, @stat_file@
    */
   
   MAKE_INTRINSIC(".chmod", unix_chmod, INT_TYPE, 2),
   /* Prototype: Integer chmod(String file, Integer mode);
    * The @chmod@ function changes the permissions of @file@ to those
    * specified by @mode@. It returns 0 upon success, -1 if the process
    * lacks sufficient privilege for the operation, or -2 if the file does
    * not exist.  See the system specific documentation for the C library
    * function @chmod@ for a discussion of the @mode@ parameter.
    * Related Functions: @chown@, @stat_file@
    */
   SLANG_END_TABLE
};

int init_SLunix()
{
   if (!SLdefine_for_ifdef ("SLUNIX")) return 0;
   return SLang_add_table(slunix_table, "_Unix");
}
