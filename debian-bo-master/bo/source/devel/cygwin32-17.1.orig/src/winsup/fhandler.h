/* winsup file handling

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

/* Classes

   fhandler			the root
   fhandler_base		normal I/O

     fhandler_dev_floppy
     fhandler_diskfile  
     fhandler_socket        

     fhandler_tty            adds vmin and vtime

        fhandler_com	     a com port

	fhandler_console
        fhandler_console_out with ansi control
        fhandler_console_in  

  fhandler_dev_null		not really I/O

  fhandler_proc			interesting possibility, not implemented yet
*/

/* Root class.  */
class fhandler
{
  /* Set if in binary write mode.  */
  char w_binary;

  /* Set if in binary read mode.  */
  char r_binary;

public:
  int get_w_binary ();
  int get_r_binary ();

  void set_w_binary (int);
  void set_r_binary (int);

  char close_exec_p;		/* Non-zero if close-on-exec bit set.  */
  virtual HANDLE get_handle ();
  virtual fhandler *open (const char *path, int flags);
  virtual int close ();
  virtual int fstat (struct stat *buf);
  virtual int ioctl (int cmd, void *);
  virtual const char * ttyname ();
  virtual int read (void *ptr, size_t len);
  virtual int write (const void *ptr, size_t len);
  virtual off_t lseek (off_t offset, int whence);
  virtual void dump ();
  virtual void dup (fhandler *child);
  virtual int has_handle_p ();

  virtual int tcflush (int);

  virtual int  get_access ();
  virtual void set_access (int);

  void *operator new (size_t, void *);
  fhandler ();

  virtual void init (HANDLE, int, int, const char *);
  virtual int tcsetattr (int a, const struct termios *t);
  virtual int tcgetattr (struct termios *t);
  virtual int is_tty ();
  virtual class fhandler_socket *is_socket ();
  virtual const char *get_name ();
  virtual const int always_ready (); /* 1 if it's pointless (and broken) to select on the handle */
};

/* FIXME: Need to try to either delete fhandler_base and have classes use
   the root class instead, or at the very least clarify why there's basically
   two root classes.  */
class fhandler_base : public fhandler
{
  /* If this file looked like it would run (ie, ends in .exe or .bat).
     If unknown, -1.  */
  signed char execable_p;		

public:
  int access;
  HANDLE handle;

  /* Used for crlf conversion in text files */
  char readahead_valid;
  char readahead;

  char append_p; /* Set if always appending */

  int rpos; /* Used in text reading */
  int rsize; 

  char had_eof;
  char symlink_p;

  int raw_write (const void *ptr, size_t ulen);

  /* FIXME: This used to be MAXPATHLEN bytes in size.
     If file handle allocation ever gets changed (which it should be) to
     either use a system-wide table or to use dynamic memory allocation
     (or both), we could again store the entire path name.  For now,
     allocating 6MB of space to record filenames is a bit much, so we only
     record the last 31 bytes worth (plus NUL).  The filename is used a lot
     in debugging messages (in which case truncating filenames is ok).
     Hopefully any real need for this will be for files with short
     filenames.  */
  char name[31+1];

  unsigned long namehash;	 /* hashed filename, used as inode num */
  virtual int raw_read (void *ptr, size_t ulen);

  int read (void *ptr, size_t len);
  int read_chunk (void *ptr, size_t len);
  int write (const void *ptr, size_t len);
  fhandler *open (const char *path, int flags);
  off_t lseek (off_t offset, int whence);
  int close ();
  int ioctl (int cmd, void *);
  virtual HANDLE get_handle ();
  int get_execable ();
  int fstat (struct stat *buf);

  void set_handle (HANDLE);
  virtual void init (HANDLE, int, int, const char *);
  virtual void dump ();

  virtual  int has_handle_p ();
  virtual  void dup (fhandler *child);
  fhandler_normal ();
  virtual int tcflush (int);
  int  get_access ();  
  void set_access (int x);
  fhandler_base ();
  inline const char *get_name () { return name; }
  void set_name (const char *);
};

class fhandler_socket: public fhandler_base
{
  int socket;
public:
  void setup (unsigned int);
  int get_socket ();
  fhandler_socket * is_socket ();
  virtual  int write (const void *ptr, size_t len);
  virtual  int read (void *ptr, size_t len);
  virtual int ioctl (int cmd, void *);
  virtual  int fstat (struct stat *buf);
  virtual int close ();
};

class fhandler_dev_floppy: public fhandler_base
{
public:
  fhandler *open (const char *path, int flags);
};

class fhandler_dev_tape: public fhandler_base
{
public:
  fhandler *open (const char *path, int flags);
};

/* Standard disk file */

class fhandler_disk_file : public fhandler_base
{
public:
  fhandler_disk_file ();
};

class fhandler_tty: public fhandler_base
{
public:
  virtual int raw_read (void *ptr, size_t ulen);
  unsigned  int vmin;			/* from termios */
  unsigned  int vtime;			/* from termios */
  virtual int tcsetattr (int a, const struct termios *t);
  virtual int tcgetattr (struct termios *t);
  virtual  int fstat (struct stat *buf);
  int close ();
  virtual void dump ();
  virtual const char *ttyname ();
  virtual int is_tty ();
  fhandler_tty ();
};

class fhandler_console: public fhandler_tty
{
public:
  fhandler *open (const char *path, int flags);
};

/* This is a console handle */
class fhandler_console_out: public fhandler_console
{
  // enum  {normal, gotesc, gotsquare, gotarg1, gotarg2, gotcommand} state;
#define  normal 1
#define  gotesc 2
#define  gotsquare 3
#define  gotarg1 4
#define  gotarg2 5
#define  gotcommand 6
  int state;
  int arg1;
  int arg2;
  void clear_screen ();
  void cursor_set (int x, int y);
  void clear_to_eol ();
  void cursor_rel (int x, int y);
  void get_info ();
  const unsigned char * write_normal (unsigned const char*, unsigned const char *);
  void char_command (char);
 public:
  fhandler_console_out ();
  int write (const void *ptr, size_t len);
  
  virtual int read (void *ptr, size_t len);
  virtual int tcflush (int);
  virtual int tcsetattr (int a, const struct termios *t);
  virtual int tcgetattr (struct termios *t);
};

class fhandler_console_in: public fhandler_console
{
  /* Bits are..
     IGNCR  ignore carriage return on input - whether we nuke '\r's
     the default for this is set by wheter the file is opened
     in text or binary mode.

     INLCR  translate NL to CR on input

     IUCLC  map uppercase characters to lowercase on input
  */
  int iflag;
  int lflag;
  int igncr_enabled ();

public:
  virtual int tcflush (int);
  virtual int tcsetattr (int a, const struct termios *t);
  virtual int tcgetattr (struct termios *t);
  virtual int read (void *ptr, size_t len);

  virtual void init (HANDLE, int, int, const char *);
  virtual const int always_ready ();
};

class fhandler_dev_null: public fhandler
{
public:
  virtual fhandler *open (const char *path, int flags);
  virtual int close ();
  virtual int fstat (struct stat *buf);
  virtual int ioctl (int cmd, void *);
  virtual int read (void *ptr, size_t len);
  virtual int write (const void *ptr, size_t len);
  virtual off_t lseek (off_t offset, int whence);
  virtual void dump ();
  virtual HANDLE get_handle ();
};

#if 0
/* You can't do this */
typedef union 
{
  fhandler_normal normal;
  fhandler_dev_null dev_null;
  fhandler bare;
  fhandler_tty tty;
} fhandler_union;
#else
#define fhandler_union fhandler_console_out
#endif

fhandler *lookup_fhandler (const char *, int flags);
void init_std_file_from_handle (fhandler &,HANDLE handle, int bin, int access, const char *name);
fhandler * build_fhandler (const char *name, int, fhandler &p);
