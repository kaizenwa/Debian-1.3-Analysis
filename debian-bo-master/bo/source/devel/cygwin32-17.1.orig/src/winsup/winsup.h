/* Winsup main header file.

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#define WIN32_LEAN_AND_MEAN
#include "windows.h"

#include <sys/types.h>
#include <sys/strace.h>
#include <setjmp.h>

#include <signal.h>
#undef sigemptyset	/* need to undefine macros created in newlib */
#undef sigaddset

#include <string.h>

#include "syscalls.h"
#include "fhandler.h"
#include "path.h"
#include "delqueue.h"

/* There is also PATH_MAX and MAXPATHLEN.
   PATH_MAX is from Posix and does *not* include the trailing NUL.
   MAXPATHLEN is from Unix.

   Thou shalt use MAX_PATH throughout.  It avoids the NUL vs no-NUL
   issue, and is neither of the Unixy ones [so we can punt on which
   one is the right one to use].  */

/* 260 = 256 + 1 (nul) + 3 (c:\) ??? */
#define MAX_PATH 260

/* Forward references.  */
class pinfo;
class shared_info;

/* Application interface.  */

/* This lives in the app and is initialized before jumping into the DLL.
   It should only contain stuff which the user's process needs to see, or
   which is needed before the user pointer is initialized, or is needed to
   carry inheritance information from parent to child.  Note that it cannot
   be used to carry inheritance information across exec!

   Remember, this structure is linked into the application's executable.
   Changes to this can invalidate existing executables, so we go to extra
   lengths to avoid having to do it.

   When adding/deleting members, remember to adjust {public,internal}_reserved.
   The size of the class shouldn't change [unless you really are prepared to
   invalidate all existing executables].  The program does a check (using
   SIZEOF_PER_PROCESS) to make sure you remember to make the adjustment.
*/

class per_process
{
 public:
  char *initial_sp;

  /* The offset of these 3 values can never change.  */
  /* magic_biscuit is the size of this class and should never change.  */
  int magic_biscuit;
  int version_major;
  int version_minor;

  struct _reent **impure_ptr_ptr;
  char ***envptr;

  /* Used to point to the memory machine we should use -
     usually points back into the dll, but can be overridden by the user.  */
  void *(*malloc)(size_t);
  void (*free)(void *);
  void *(*realloc)(void *, size_t);

  int *fmode_ptr;

  int (*main)(int, char **, char **);
  void (**ctors)();
  void (**dtors)();

  /* For fork */
  void *data_start;
  void *data_end;
  void *bss_start;
  void *bss_end;

  /* For future expansion of values set by the app.  */
  void *public_reserved[4];

  /* The rest are *internal* to cygwin.dll.
     Those that are here because we want the child to inherit the value from
     the parent (which happens when bss is copied) are marked as such.  */

  /* FIXME: Which of these can go elsewhere?  */

  /* FIXME: Delete, make `self' a global.  */
  pinfo *self;		/* pointer only valid in self process */

  /* non-zero of ctors have been run.  Inherited from parent.  */
  int run_ctors_p;

  /* These will be non-zero if the above (malloc,free,realloc) have been
     overridden.  */
  /* FIXME: not currently used */
  int __imp_malloc;
  int __imp_free;
  int __imp_realloc;

  /* Heap management.  Inherited from parent.  */
  void *base;			/* bottom of the heap */
  void *ptr;			/* current index into heap */
  int  size;			/* current size of heap */

  /* Mask of what to trace.  Inherited from parent.
     See sys/strace.h for details.  The value of this is changeable from other
     tasks via the `cygwin' utility so we want this in the shared data area
     (and thus the process table since there's one of these per task).
     However, we also want to turn on stracing as soon as possible and
     therefore before we know which process table entry to use.  So we put it
     here, and have a pointer to it in the process table.  */
  int strace_mask;

  /* Non-zero means the task was forked.  The value is the pid.
     Inherited from parent.  */
  int forkee;

  /* For future expansion, so apps won't have to be relinked if we
     add an item.  */
  void *internal_reserved[11];

  /*  struct file_queue *dq;	 !!! this may need to be nuked ? */
};

extern per_process *u; /* pointer into the application's static data */

/* This can't use sizeof.  We use this to test that sizeof hasn't changed.
   When adding/deleting members: insert fillers or use the reserved entries.
   Do not change this value.  */
#define SIZEOF_PER_PROCESS (42 * 4)

/* File descriptor table.  */

class hinfo
{
 public:
  class fhandler *h;
  fhandler_union item;	
  int is_open () const { return h != 0;}
  void clearout ();
};

class hinfo_vec
{
 public:
  hinfo vec[NOFILE];

  int not_open (int fd);
  hinfo & operator [] (int arg);
  int find_unused_handle (int start);
  void release (int fd);
  void lookup_fhandler (int fd, const char *path, int flags);
  void clearout ();
  void init_std_file_from_handle (int fd, HANDLE handle, int bin, int access, const char *name);
  fhandler * build_fhandler (const char *name, int n);
  void dup_for_exec ();
  void dup_for_fork (hinfo_vec *child);
  int dup2 (int oldfd, int newfd);

};

/* The process table.  */

class pinfo
{
 public:

  /* IF these handles are set, it's because they came from a
     CreateProcess call.  This means they're process relative
     to the thing which created the process.  That's ok because
     we only use these handles from the parent to see what the
     child is up to, then we close the handles in the parent. */
  HANDLE hProcess;		
  HANDLE hThread;

  /* The pid stays the same, while the hProcess moves due to execs.  */
  pid_t pid;
  /* Parent process id.  */
  pid_t ppid;

  /* Our win32 process id.  */
  int dwProcessId;

  /* User information.
     The information is derived from the GetUserName system call,
     with the name looked up in /etc/passwd and assigned a default value
     if not found.  This data resides in the shared data area (allowing
     tasks to store whatever they want here) so it's for informational
     purposes only.  */
  uid_t uid;
  gid_t gid;

  /* Used during forking to jump back into the fork support routines.  */
  jmp_buf restore;

  /* Non-zero if the heap has been split, which means we can't fork again.  */
  char split_heap_p;

  /* Non-zero if entry is in use (duh... :-).  */
  char inuse_p;

  /* Non-zero if parent was a cygwin app.  */
  char cygwin_parent_p;

  /* FIXME: There are 100(PSIZE) * 64(NOFILE) file table entries.
     Each entry has space for the file name.
     --> 100 * 64 * 1024 bytes (or 260 if that's all that win32 allows
     but still!) !!!
     Need to separate the open file table from the process table
     and only have NR_OPEN file table entries.  */
  hinfo_vec hmap;

  struct sigaction sigs[NSIG];
  sigset_t sig_mask;		/* one set for everything to ignore. */

  /* This is used, among other things, to spawn a child for fork().  */
  char progname[MAX_PATH];

  /* Pointer to strace mask, NULL if not set yet.
     See strace.cc for details.  */
  int *strace_mask_ptr;

  void record_death ();
  void init_self ();
  pid_t get_pid ();
  void terminate ();
  void init_from_exec ();
  void clearout ();
};

#define PSIZE 100

class pinfo_list
{
 public:
  int pid_base;
  pinfo vec[PSIZE];
  pinfo *get_empty_pinfo ();
  pinfo * operator[] (pid_t x);
  int size () { return PSIZE; }
  pinfo * lookup_by_handle (HANDLE *);
  pinfo * allocate_pid ();
  void init ();
};

void pinfo_init ();
void hmap_init ();
void uinfo_init ();

pinfo *procinfo (int n);

/* FIXME: Lot's of code uses u->self and lots uses this.  u->self should
   become either just `u' or `self', so this proc should go.
   (there may be benefits to wrapping `self' in a cover function, the point
   is to be consistent).  */
pinfo *this_procinfo ();

/* Shared data.

   This data is accessable to all tasks.
   It's therefore a source of instability.
   It could also be a security hole if one isn't careful.
   FIXME: It may be already.
  */

class shared_info
{
  int inited;

public:
  pinfo_list p;

  /* FIXME: Doesn't work if more than one user on system.  */
  mount_info mount;

  int heap_chunk_in_mb;
  int heap_chunk_size ();

  /* FIXME: This here, I think, from an early fork implementation.  */
  /*  void * base[4][2];*/

  delqueue_list delqueue;

  void initialize ();
};

extern shared_info *s;

void shared_init ();
void shared_terminate ();

/* This is for programs that want to access the shared data.  */
extern "C" class shared_info *cygwin32_getshared ();

/* Fork support and exec.  */
void fork_init ();
void fork_terminate ();
void dump_jmp_buf (jmp_buf buf);

void spawn_guts (const char *prog_arg, char *const *argv, char *const envp[], int pid);
const char * find_exec (const char *name, char *buf);

/* Strace facility.  See strace.cc and sys/strace.h.  */
void strace_init ();

/* Heap management.  */
#define HEAP_CHUNK_SIZE  (8 * 1024 * 1024)
void heap_init ();

void close_all_files (void);

/* exports */

extern "C" {
int ScreenRows ();
int ScreenCols ();
void  ScreenGetCursor (int *row, int *col);
void ScreenSetCursor (int row, int col);
pid_t _execve (const char *path, char *const argv[], char *const envp[]);
pid_t spawn (int mode, const char *prog, char *const *argv);
pid_t spawnvp (int mode, const char *prog, char *const *argv);
void __main ();
void __do_global_ctors ();
void __do_global_dtors ();
int _rename (const char *oldpath, const char *newpath);
int lstat (const char *name, struct stat *buf);
int access (const char *fn, int flags);
int _stat (const char *name, struct stat *buf);
int  chmod (const char *a, mode_t x);
int _unlink (const char *name);
pid_t _getpid ();
pid_t  setsid (void);
time_t time (time_t * ptr);
int _fcntl (int fd, int cmd,...);
int dup2 (int fd, int fd2);
int dup (int fd);
int pipe (int fildes[2]);
int kbhit ();
int getkey ();
int setpgid (int f, int p);
int tcsetpgrp (int f, int p);
int tcgetpgrp (int f);
int tcgetattr (int fd, struct termios *t);
int sigdelset (sigset_t *, const int);
int sigaddset (sigset_t *, const int);
int sigismember (const sigset_t *, int);
int sigemptyset (sigset_t *);
int sigfillset (sigset_t *);
int gethostname (char *name, size_t len);
int tcsetattr (int fd, int actions, const struct termios *t);
int tcdrain (int fd);
int tcdrain (int fd);
int tcsendbreak (int fd, int duration);
int tcflow (int fd, int action);

int _link (const char *, const char *);
int vfork ();
int kill (int, int);
int _kill (int, int);
int _raise (int sig);
pid_t cwait (int *status, pid_t intpid, int flags);
pid_t waitpid (pid_t intpid, int *status, int options);
pid_t _wait (int *status);
int tcflush (int fd, int queue);
int system (const char *);

unsigned int sleep (unsigned int seconds);
unsigned int usleep (unsigned int useconds);
unsigned int alarm (unsigned int seconds);

int _gettimeofday ();

int sigaction (int signum,
	       const struct sigaction *newaction,
	       struct sigaction *oldaction);

int getdtablesize ();

pid_t __spawnvp (int mode, const char *prog, char *const *argv);

pid_t __spawnv (int mode, const char *prog, char *const *argv);

extern unsigned long int	htonl (unsigned long int);
extern unsigned short int	htons (unsigned short int);

};

void mark (const char *s, int i);
#define MARK() mark (__FILE__,__LINE__)

extern "C" int __small_sprintf (char *dst, const char *fmt, ...);
#if 0 /* Commented out because of va_list reference?  */
extern "C" int __small_vsprintf (char *dst, const char *fmt, va_list ap);
#endif
void system_printf (const char *fmt, ...);
void small_printf (const char *fmt, ...);

void dll_crt0 (per_process *);

unsigned long hash_path_name (const char *name);

void process_deletion_queue (void);

long totime_t (FILETIME * ptr);

/* Internal declarations */
void totimeval (struct timeval *dst, FILETIME * src, int sub);
long to_time_t (FILETIME * ptr);
int fhandler_make_pipe (int fildes[2]);

extern const char *conv_path_names[];

#if DEBUG_NEST_ON!=0
#define in(x) { debug_printf ("mark in %s:%d %s\n",__FILE__,__LINE__, x); d++;}
#define out(x) { d--; debug_printf ("mark out %s:%d %s\n",__FILE__,__LINE__,x);}
#else
#define in(x)
#define out(x)
#endif

void select_init ();

void api_fatal (const char *);
