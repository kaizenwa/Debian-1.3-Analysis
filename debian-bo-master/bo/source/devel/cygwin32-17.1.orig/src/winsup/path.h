/* path and mount data structures for winsup.

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

class path_conv 
{
  char path[MAX_PATH];
 public:

  char mixed_p;
  char binary_p;
  char silent_p;

  int error;

  path_conv (const char * const);
  inline char *get_win32 () { return path; }
};

/* Symlink marker.  */
#define SYMLINK_COOKIE "!<symlink>"

/* Maximum depth of symlinks (after which ELOOP is issued).  */
#define MAX_LINK_DEPTH 10

/* Symlink utilities.  */
int symlink_follow (const char *path, char *realpath);

/* Mount table entry.  */

class mount_item
{
public:
  /* FIXME: Nasty static allocation.  Need to have a heap in the shared
     area [with the user being able to configure at runtime the max size].  */

  /* DOS-style mounted partition source ("F:").
     device[0] == 0 for unused entries.  */
  char device[30];
  int devicelen;

  /* UNIX-style mount point ("/mnt") */
  char path[30];
  int pathlen;

  char mixed;
  char binary;
  char silent;

  void init (const char *dev, const char *path, int flags);

  struct mntent *getmntent ();
};

/* Mount table.  */

/* Warning: Decreasing this value will cause cygwin.dll to ignore existing
   higher numbered registry entries.  Don't change this number willy-nilly.
   What we need is to have a more dynamic allocation scheme, but the current
   scheme should be satisfactory for a long while yet.  */
#define MAX_MOUNTS 30

class mount_info
{
 public:
  int nmounts;
  mount_item mount[MAX_MOUNTS];

  void init ();
  int add_item (const char *dev, const char *path, int flags);
  int del_item (const char *path);
  void sort ();
  void to_registry ();
  void from_registry ();
  int posix_path_p ();
  int binary_win32_path_p (const char *path);
  int conv_to_win32_path (const char *src_path, char *win32_path,
			  char *full_win32_path);
  int conv_to_posix_path (const char *src_path, char *posix_path,
			  int keep_rel_p);
  struct mntent *getmntent (int x);
};

/* These are exported from the dll as cygwin32_foo.  */
extern "C" {
  void conv_to_win32_path (const char *src_path, char *win32_path);
  void conv_to_full_win32_path (const char *src_path, char *win32_path);
  void conv_to_posix_path (const char *src_path, char *posix_path);
  void conv_to_full_posix_path (const char *src_path, char *posix_path);
  int posix_path_list_p (const char *path);
  int win32_to_posix_path_list_buf_size (const char *path_list);
  int posix_to_win32_path_list_buf_size (const char *path_list);
  void win32_to_posix_path_list (char *win32, char *posix);
  void posix_to_win32_path_list (char *posix, char *win32);
  void split_path (const char *path, char *dir, char *file);
}

void unmixedcaseify (char *name);
