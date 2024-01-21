/*
 *  magicfilter.c
 *
 *  Copyright © 1993-96 H. Peter Anvin
 *
 *  "Magic" default filter for lpr/lpd which attempt to identify
 *  the type of file and filter it accordingly
 *
 */

#include "magicfilter.h"
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_MEMORY_H
#include <memory.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/fcntl.h>
#include <sys/types.h>
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

char *inblock;			/* Block read */
int inblock_size;		/* Bytes reserved for the inblock */
int inblock_len;		/* Number of bytes read (<= inblock_size) */
int done = 0;			/* Set to 1 if no more iterations */
char *program;			/* Set to argv[0] in main() */
char *config_file = NULL;	/* Configuration file */
int debug_flag = 0;		/* If debugging */

/* What to call ourselves in case of failure */
#define ERR_NAME                (config_file ? config_file : program)

extern const struct datatype dtypetab[]; /* Table of magic */

/* Location of the mailer */
#ifndef NOMAIL
#ifndef SENDMAIL
#ifdef _PATH_SENDMAIL
/* Path defined in paths.h */
#define SENDMAIL _PATH_SENDMAIL
#else
#ifdef HAVE_SENDMAIL
/* Path defined by autoconf */
#define SENDMAIL PATH_SENDMAIL
#endif
#endif
#endif
#ifndef SENDMAIL
/* We couldn't find a mailer anywhere -- disable mail */
#define NOMAIL 1
#endif
#endif

/* Header of mail message */
#ifndef NOMAIL
const char *mailheader =	/* Header of complaint message */
"To: %s\n\
Subject: Rejected print job\n\

Your print job was automatically rejected as an invalid data type.\n\n";
#endif

/* ------------------------------------------------------------------------- *
 * Fudging of certain functions if we do not have them.  None of these
 * are very good to use, but are better than nothing.
 * ------------------------------------------------------------------------- */

#ifndef HAVE_DUP2
int dup2(int fd1, int fd2)

#ifdef F_DUPFD
{
  close(fd2);
  return fcntl(fildes, F_DUPFD, fd2);
}
#else
{
  int rtn;

  close(fd2);			/* Might return error if unused, is OK */
  rtn = dup(fd1);

  if ( rtn >= 0 && rtn != fd2 )
    {
      fprintf(stderr, "%s: dup() trick failed -- aborting\n", ERR_NAME);
      exit(1);
    }

  return rtn;
}
#endif

#endif

#ifdef HAVE_TMPNAM
/* If we don't have L_tmpnam, we can't use the provided tmpnam */
#ifndef L_tmpnam
#define tmpnam(X) my_tmpnam(X)
#undef HAVE_TMPNAM
#endif
#endif

#ifndef HAVE_TMPNAM
/*
 * Lame implementation of tmpnam which works on most Unices
 */

#define L_tmpnam (15+3*sizeof(pid_t))

char *tmpnam(char *buf)
{
  static char tmp_buf[L_tmpnam];
  static int count = 0;

  if ( !buf ) buf = tmp_buf;

  sprintf(buf, "/tmp/_magf.%u.%02x", (unsigned int) getpid(), count++);
  return buf;
}
#endif

/* ------------------------------------------------------------------------- *
 *  run(command)
 *
 * Similar to system(), but always run a specified shell and does not
 * fork.  We don't want to fork because we are creating a pipe, anyway.
 *
 * The shell is defined in SYSSHELL and is called as
 * ``<SYSSHELL> -c command_line''
 * ------------------------------------------------------------------------- */

#ifndef SYSSHELL
#ifdef _PATH_BSHELL
#define SYSSHELL _PATH_BSHELL
#else
#define SYSSHELL "/bin/sh"
#endif
#endif
#ifndef SHELLFLAG
#define SHELLFLAG "-c"
#endif

void run(char *command)
{
  execl(SYSSHELL,SYSSHELL,SHELLFLAG,command,(char *)NULL);
  exit(255);			/* If we get here, the exec failed */
}


/* ------------------------------------------------------------------------- *
 *  fork_daemon()
 *
 * Fork a process that doesn't have to be wait'ed on.  This it done by
 * forking *twice* and orphanning the grandchild process, leaving it
 * adopted by init.  Do a reasonable job of propagating errors.
 *
 * Returns 0 for the child and 1 for the parent if OK; otherwise -1.
 * (The reason the parent returns 1 is that is *doesn't know* the pid of
 * the spawned child process.)
 * ------------------------------------------------------------------------- */

/* This is for non-POSIX compliant systems */
#ifndef WEXITSTATUS
#define WEXITSTATUS(stat_val) ((unsigned)(stat_val) >> 8)
#endif
#ifndef WIFEXITED
#define WIFEXITED(stat_val) (((stat_val) & 0xff) == 0)
#endif

#ifndef HAVE_WAITPID
/* We don't have waitpid(), we have to emulate it */
#ifdef HAVE_WAIT4
/* With wait4() we should be set */
#define waitpid(p,s,o) wait4(p, s, o, NULL)
#else
/* This is a feeble implementation using wait(); it doesn't implement
   the full waitpid() semantics, but should be close enough for
   what we need.  Use with caution. */
pid_t waitpid(pid_t pid, int *status, int options)
{
  pid_t retval;
  do
    retval = wait(status);
  while ( retval > 0 && retval != pid );
  return retval;
}
#endif
#endif

int fork_daemon(void)
{
  pid_t pid;
  int status;

  pid = fork();
  if ( pid < 0 )
    return -1;			/* Error on first fork */
  else if ( pid == 0 )
    {
      pid = fork();
      if ( pid < 0 )
	_exit(errno);
      else if ( pid == 0 )
	return 0;		/* Child process */
      else
	_exit(0);
    }
  else
    {
      waitpid(pid,&status,0);
      if (status)		/* Did we return error? */
	{
	  errno = WIFEXITED(status) ? WEXITSTATUS(status) : EINTR;
	  return -1;
	}
      return 1;			/* Parent process */
    }
}

/* ------------------------------------------------------------------------- *
 *  cat(file)
 *  act_cat(opt_string)
 *
 * Feed the input to a specified file without any processing whatsoever.
 *
 * act_cat() is the action; it will attempt to extract a prefix and suffix
 * string from the included option string as well.
 * ------------------------------------------------------------------------- */

void cat(FILE *f)
{
  if ( inblock_len > 0 )
    fwrite(inblock,1,inblock_len,f);

  while( ( inblock_len = fread(inblock,1,inblock_size,stdin) ) > 0 )
    {
      fwrite(inblock,1,inblock_len,f);
    }
}

typedef void datafunc(FILE *);
void do_prefix_suffix(char *opt_str, datafunc *convfunc, FILE *f)
{
  char *p, *prefix, *suffix = NULL;
  int n;

  if ( (n = getmagic(opt_str,&p,NULL,NO_WILD)) >= 0 )
    {
      if ( n > 0 )
	{
	  if ( !(prefix = malloc(n)) )
	    {
	      fprintf(stderr, "%s: Out of memory\n", ERR_NAME);
	      exit(1);
	    }
	  getmagic(opt_str,&p,prefix,NO_WILD);
	  fwrite(prefix,1,n,f); /* Write prefix string */
	  free(prefix);
	}
      if ( (n = getmagic(p,NULL,NULL,NO_WILD)) > 0 )
	{
	  if ( !(suffix = malloc(n)) )
	    {
	      fprintf(stderr, "%s: Out of memory\n", ERR_NAME);
	      exit(1);
	    }
	  getmagic(p,NULL,suffix,NO_WILD);
	}
    }
  
  convfunc(f);

  if ( suffix )
    {
      fwrite(suffix,1,n,f);
      free(suffix);
    }
}

void act_cat(char *opt_str)
{
  if ( debug_flag )
    fprintf(stderr, "cat %s\n", opt_str);
  
  do_prefix_suffix(opt_str, cat, stdout);
  done = 1;
}

/* ------------------------------------------------------------------------- *
 *  act_addcr()
 *
 * Feed the input to the output, substituting:
 *    \n -> \r\n     \f -> \r\f
 *
 * It also adds \r\f at the end of the file.
 * 
 * Used for printers which need CR's at line endings.  Note that if your
 * printer supports a different character set (say Roman-8) than your host
 * (say Latin-1) you may be better off using ACT_FILTER and a translating
 * filter such as "lpchar".
 * ------------------------------------------------------------------------- */

void addcr(FILE *f)
{
  char *p;
  int i;

  do
    {
      for ( i = inblock_len, p = inblock ; i ; i--, p++ )
	{
	  switch ( *p )
	    {
	    case '\n':
	    case '\f':
	      putc('\r', f);
	      /* Fall through */

	    default:
	      putc(*p, f);
	      break;
	    }
	}
    
      inblock_len = fread(inblock, 1, inblock_size, stdin);
    }
  while ( inblock_len > 0 );

  putc('\r', f);
  putc('\f', f);
}


void act_addcr(char *opt_str)
{
  if ( debug_flag )
    fprintf(stderr, "text %s\n", opt_str);

  do_prefix_suffix(opt_str, addcr, stdout);
  done = 1;
}

/* ------------------------------------------------------------------------- *
 *  act_ps()
 *
 * Like act_addcr() but add a Ctrl-D to the end (PostScript End of Job).
 * Keeps the printer from running two jobs together.  nenscript in
 * particular is bad with this.
 * ------------------------------------------------------------------------- */

void act_ps(void)
{
  if ( debug_flag )
    fprintf(stderr, "postscript\n");
  
  addcr(stdout);
  putchar('\4');
  done = 1;
}


/* ------------------------------------------------------------------------- *
 *  act_drop()
 *
 * Just sets the done flag.
 * ------------------------------------------------------------------------- */

void act_drop(void)
{
  if ( debug_flag )
    fprintf(stderr, "ignore\n");

  done = 1;
}


/* ------------------------------------------------------------------------- *
 *  act_reject(message,user,host)
 *
 * Sends a rejection message to the user.  A bad file (such as an executable
 * binary) was encountered.
 * ------------------------------------------------------------------------- */

#ifndef NOMAIL
void act_reject(char *message, char *user, char *host)
{
  int fildes[2];		/* File descriptors for pipe */
  pid_t pid;
  FILE *msg;			/* Outgoing pipe */
  char address[BUFSIZ];		/* Buffer to create address */
  int status;

  if ( debug_flag )
    fprintf(stderr, "reject %s\n", message);

#ifdef BANG_ADDRESS
  sprintf(address,"%s!%s", host, user);
#else
  sprintf(address,"%s@%s", user, host);
#endif

  if ( pipe(fildes) )
    {
      perror(ERR_NAME);
      exit(1);
    }

  pid = fork();

  if ( pid < 0 )		/* Fork failed */
    {
      perror(ERR_NAME);
      exit(1);
    }
  else if ( pid == 0 )		/* Child process */
    {
      close(fildes[1]);
      dup2(fildes[0],fileno(stdin)); /* Redirect stdin */
      execl(SENDMAIL,SENDMAIL,address,(char *) NULL);
      exit(255);		/* Exec failed */
    }
  else				/* Parent process */
    {
      close(fildes[0]);
      msg = fdopen(fildes[1],"wb"); /* Make pipe file pointer */

      fprintf(msg, mailheader, address);
      fprintf(msg, "%s\n", message);
      fclose(msg);

      waitpid(pid,&status,0);

      if ( status )
	{
	  fprintf(stderr, "%s: %s failed\n", ERR_NAME, SENDMAIL);
	}
    }

  done = 1;
}
#else
#define act_reject(MSG,USR,HST) act_drop()
#endif    


/* ------------------------------------------------------------------------- *
 *  act_filter(command)
 * 
 * Set up a pipe and filter the input through that command.  Output goes
 * to stdout.
 * ------------------------------------------------------------------------- */

void act_filter(char *command)
{
  int fildes[2];		/* File descriptors for pipe */
  FILE *out;			/* Output file pointer */
  pid_t pid;			/* Child process ID */
  int status;

  if ( debug_flag )
    fprintf(stderr, "filter %s\n", command);

  if ( pipe(fildes) )
    {
      perror(ERR_NAME);
      exit(1);
    }

  pid = fork();

  if ( pid < 0 )		/* Fork failed */
    {
      perror(ERR_NAME);
      exit(1);
    }
  else if ( pid == 0 )		/* Child process */
    {
      close(fildes[1]);
      dup2(fildes[0],fileno(stdin)); /* Redirect stdin */
      run(command);
    }
  else				/* Parent process */
    {
      close(fildes[0]);
      out = fdopen(fildes[1],"wb"); /* Make pipe file pointer */
      cat(out);			/* Send output into pipe */
      fclose(out);

      waitpid(pid,&status,0);

      if ( status )
	{
	  fprintf(stderr, "%s: %s failed\n", ERR_NAME, command);
	}
    }

  done = 1;
}

/* ------------------------------------------------------------------------- *
 *  act_ffilter(command)
 * 
 * Create a temp file; then filter the temp file through that command.
 * Output goes to stdout.
 * ------------------------------------------------------------------------- */

void act_ffilter(char *command)
{
  static char tmpbuf[L_tmpnam+6] = "FILE=";
  FILE *out;			/* Output file pointer */
  int outfd;			/* Output file descriptor */
  pid_t pid;			/* Child process ID */
  int status;

  if ( debug_flag )
    fprintf(stderr, "ffilter %s\n", command);

  if ( !tmpnam(tmpbuf+5) || !(out = fopen(tmpbuf+5, "wb")) )
    {
      perror(ERR_NAME);
      exit(1);
    }

  cat(out);			/* Send output to temp file */
  fclose(out);

  pid = fork();

  if ( pid < 0 )		/* Fork failed */
    {
      perror(ERR_NAME);
      exit(1);
    }
  else if ( pid == 0 )		/* Child process */
    {
      putenv(tmpbuf);		/* Store name in $FILE */
      outfd = open(tmpbuf+5, O_RDONLY);
      dup2(outfd,fileno(stdin)); /* Redirect stdin */
      run(command);
    }
  else				/* Parent process */
    {
      waitpid(pid,&status,0);	/* Wait for process to finish */

      if ( status )
	{
	  fprintf(stderr, "%s: %s failed\n", ERR_NAME, command);
	}

      unlink(tmpbuf+5);		/* Remove temp file */
    }

  done = 1;
}

/* ------------------------------------------------------------------------- *
 *  act_pipethru(command)
 *
 * Pipes the output through a given command, creating a second pipe to
 * fetch the input for re-processing.
 *
 * This is done the "proper" way by creating a sender and a receiver process
 * around the filter process; who knows what the OS does if we overflow a
 * pipe.
 * ------------------------------------------------------------------------- */

void act_pipethru(char *command)
{
  int inpipefd[2];		/* File descriptors for input pipe */
  int outpipefd[2];		/* File descriptors for output pipe */
  FILE *out;			/* Output file pointer */
  pid_t pid, xpid, wpid;
  int null;			/* /dev/null */
  int status;			/* Process exit status */

  if ( debug_flag )
    fprintf(stderr, "pipe %s\n", command);

  if ( pipe(inpipefd) || pipe(outpipefd) )
    {
      perror(ERR_NAME);
      exit(1);
    }

  xpid = fork_daemon();
  
  if ( xpid < 0 )
    {
      perror(ERR_NAME);
      exit(1);
    }
  else if ( xpid == 0 )
    {
       close(outpipefd[0]);

       pid = fork();

       if ( pid < 0 )		/* Fork failed */
	 {
	   perror(ERR_NAME);
	   exit(1);
	 }
       else if ( pid == 0 )		/* Child process */
	 {
	   close(inpipefd[1]);
	   dup2(inpipefd[0],fileno(stdin));
	   dup2(outpipefd[1],fileno(stdout)); /* Redirect stdout */
	   run(command);
	 }
       else
	 {
	   /* Writing process */

	   close(inpipefd[0]);
	   close(outpipefd[1]);
	   
	   out = fdopen(inpipefd[1],"wb");
	   cat(out);		/* Send output into pipe */
	   fclose(out);
	   
	   /* Wait for filter to exit */

	   if ( waitpid(pid,&status,0) < 0 )
	     perror("waitpid");
	   
	   /* If status > 0 we should probably do something, but it is
	      difficult to know what we can do at this stage */
	   
	   exit(0);
	 }
     }
  else
    {
      /* Reading process */
      
      close(inpipefd[0]);
      close(outpipefd[1]);
      close(inpipefd[1]);
      
      /* Flush the stdin buffer */
      null = open("/dev/null", O_RDONLY);
      dup2(null, fileno(stdin));
      close(null);
      while ( fread(inblock,1,inblock_size,stdin) > 0 );
      
      if ( dup2(outpipefd[0],fileno(stdin)) < 0 )
	{
	  perror("dup2");
	  exit(1);
	} /* Redirect stdin */
      
      close(outpipefd[0]);
      
      rewind(stdin);
      inblock_len = fread(inblock,1,inblock_size,stdin);
    }
}


/* ------------------------------------------------------------------------- *
 *  act_fpipe(command)
 *
 * Creates a temp file and sends the output through a given command,
 * creating a pipe to fetch the input for re-processing.
 * ------------------------------------------------------------------------- */

void act_fpipe(char *command)
{
  static char tmpbuf[L_tmpnam+6] = "FILE=";
  int outpipefd[2];		/* File descriptors for output pipe */
  FILE *out;			/* Output file pointer */
  int outfd;			/* Output file descriptor */
  pid_t pid, xpid, wpid;
  int null;			/* /dev/null */
  int status;			/* Exit status */

  if ( debug_flag )
    fprintf(stderr, "fpipe %s\n", command);

  if ( !tmpnam(tmpbuf+5) || !(out = fopen(tmpbuf+5,"w+b")) || pipe(outpipefd) )
    {
#if DEBUG > 2
      fprintf(stderr, "TMPBUF: %s\n", tmpbuf);
#endif
      perror(ERR_NAME);
      exit(1);
    }

  cat(out);			/* Write output to file */
  fclose(out);

  xpid = fork_daemon();
  
  if ( xpid < 0 )
    {
      perror(ERR_NAME);
      exit(1);
    }
  else if ( xpid == 0 )
    {
      close(outpipefd[0]);
      
      pid = fork();
      
      if ( pid < 0 )		/* Fork failed */
	{
	  perror(ERR_NAME);
	  exit(1);
	}
      else if ( pid == 0 )		/* Child process */
	{
	  putenv(tmpbuf);		/* Store name in $FILE */
	  outfd = open(tmpbuf+5, O_RDONLY);
	  dup2(outfd,fileno(stdin)); /* Redirect stdin */
	  dup2(outpipefd[1],fileno(stdout)); /* Redirect stdout */
	  run(command);
	}
      else				/* Parent process */
	{
	  close(outpipefd[1]);
	  
	  /* Wait for filter to finish and then remove temp file */

	  if ( waitpid(pid,&status,0) < 0 )
	    perror("waitpid");
	  
	  /* If status > 0 we should probably do something, but it is
	     difficult to know what we can do at this stage */
	  
	  unlink(tmpbuf+5);
	  exit(0);
	}
    }
  else
    {
      close(outpipefd[1]);
      
      /* Reading process */
      
      /* Flush the stdin buffer */
      null = open("/dev/null", O_RDONLY);
      dup2(null, fileno(stdin));
      close(null);
      while ( fread(inblock,1,inblock_size,stdin) > 0 );
      
      if ( dup2(outpipefd[0],fileno(stdin)) < 0 )
	{
	  perror("dup2");
	  exit(1);
	} /* Redirect stdin */
      
      close(outpipefd[0]);
      
      rewind(stdin);
      inblock_len = fread(inblock,1,inblock_size,stdin);
    }
}


/* ------------------------------------------------------------------------- *
 *  matches(block, pattern, mask, length)
 *
 *  Returns nonzero if block matches pattern.
 * ------------------------------------------------------------------------- */

int matches(char *block, char *pattern, char *mask, int length)
{
  int i;
  char *p, *q, *m;

  q = block;
  p = pattern;
  m = mask;

  for ( i = length ; i ; i-- )
    {
      if ( (*p ^ *q) & *m )
	return 0;
      q++; p++; m++;
    }

#if DEBUG > 3
  fprintf(stderr,"Pattern: ");
  for ( i = 0 ; i < length ; i++ )
    {
      fprintf(stderr,"%03o ", (unsigned char) block[i]);
    }
  fprintf(stderr,"\nMatches: ");
  for ( i = 0 ; i < length ; i++ )
    {
      if ( mask[i] )
	fprintf(stderr,"%03o ", (unsigned char) pattern[i]);
      else
	fprintf(stderr,"??? ");
    }
  fprintf(stderr,"\n");
#endif

  return 1;
}

/* ------------------------------------------------------------------------- *
 *  envset(var,value)
 * 
 * Inserts a key value into the environment
 * ------------------------------------------------------------------------- */

void envset(char *var, char *value)
{
  char *buf;

  if ( !(buf = malloc(strlen(var)+strlen(value)+2)) )
    {
      fprintf(stderr, "%s: out of memory\n", ERR_NAME);
      exit(1);
    }
  sprintf(buf, "%s=%s", var, value);
  if ( putenv(buf) )
    {
      perror(ERR_NAME);
      exit(1);
    }
  if ( debug_flag )
    fprintf(stderr, "%s\n", buf);
}
 
/* ------------------------------------------------------------------------- *
 *  main(argc,argv)
 * 
 * Analyze the input data and choose an appropriate way to handle it.
 * ------------------------------------------------------------------------- */

main(int argc, char *argv[])
{
  int do_cat = 0;		/* True if -c option */
  int i;
  const struct datatype *dtab;	/* Data type table */
  const struct datatype *dt;	/* Data type searched */
  char *user = "unknown";	/* User name */
  char *host = "unknown";	/* Host name */

  program = argv[0];		/* Make available for error messages */

  if ( argc < 2 )
    {
      fprintf(stderr, "%s: No configuration file specified\n", program);
      exit(1);
    }

  /* Clear these strings from environment; if we have unsetenv use it,
     otherwise set to blank strings */

#ifdef HAVE_UNSETENV
  unsetenv("LPUSER");
  unsetenv("LPHOST");
  unsetenv("LPJOB");
  unsetenv("FILE");
  unsetenv("PRINTER");
  unsetenv("LPFORMAT");
  unsetenv("LPCLASS");
  unsetenv("LPACCT");
  unsetenv("LPCOPIES");
  unsetenv("LPQUEUE");
  unsetenv("ZOPT");
  unsetenv("BANNERNAME");
#else
  putenv("LPUSER=");
  putenv("LPHOST=");
  putenv("LPJOB=");
  putenv("FILE=");
  putenv("PRINTER=");
  putenv("LPFORMAT=");
  putenv("LPCLASS=");
  putenv("LPACCT=");
  putenv("LPCOPIES=");
  putenv("LPQUEUE=");
  putenv("ZOPT=");
  putenv("BANNERNAME=");
#endif
  putenv("LPINDENT=0");		/* Default to zero */

  for ( i = 1 ; i < argc ; i++ ) /* Options passed by lpd */
    {
      if ( argv[i][0] == '-' )
	{
	  if ( strcmp(argv[i],"-c") == 0 )
	    do_cat = 1;
	  else if ( strncmp(argv[i],"-n",2) == 0 )
	    {
	      user = (argv[i][2] || i == argc-1) ? argv[i]+2 : argv[++i];
	      envset("LPUSER", user);
	    }
	  else if ( strncmp(argv[i],"-h",2) == 0 )
	    {
	      host = (argv[i][2] || i == argc-1) ? argv[i]+2 : argv[++i];
	      envset("LPHOST", host);
	    }
	  else if ( strncmp(argv[i],"-i",2) == 0 )
	    envset("LPINDENT", argv[i]+2);
	  else if ( strncmp(argv[i],"-C",2) == 0 )
	    envset("LPCLASS", argv[i]+2);
	  else if ( strncmp(argv[i],"-F",2) == 0 )
	    envset("LPFORMAT", argv[i]+2);
	  else if ( strncmp(argv[i],"-J",2) == 0 )
	    envset("LPJOB", argv[i]+2);
	  else if ( strncmp(argv[i],"-K",2) == 0 )
	    envset("LPCOPIES", argv[i]+2);
	  else if ( strncmp(argv[i],"-L",2) == 0 )
	    envset("BANNERNAME", argv[i]+2);
	  else if ( strncmp(argv[i],"-P",2) == 0 )
	    envset("PRINTER", argv[i]+2);
	  else if ( strncmp(argv[i],"-Q",2) == 0 )
	    envset("LPQUEUE", argv[i]+2);
	  else if ( strncmp(argv[i],"-R",2) == 0 )
	    envset("LPACCT", argv[i]+2);
	  else if ( strncmp(argv[i],"-Z",2) == 0 )
	    envset("ZOPT", argv[i]+2);
	  else if ( strcmp(argv[i], "--debug") == 0 )
	    debug_flag = 1;
	  /* Ignore other options passed by lpd */
	}
      else if ( !config_file )
	config_file = argv[i];
    }

  umask(077);			/* Protect the user's privacy */

  if ( do_cat )			/* Literal flag: skip config file */
    {
      if ( debug_flag )
	fprintf(stderr, "(literal)\n");
      inblock_size = BUFSIZ;
    }
  else
    {
      dtab = load_config(config_file, &i); /* Load configuration file */
      if ( !dtab ) exit(1);	/* load_config does the error message */
      /* Round up to nearest multiple of BUFSIZ, for efficiency */
      inblock_size = ((i+BUFSIZ-1)/BUFSIZ)*BUFSIZ;
    }

  if ( !(inblock = malloc(inblock_size)) )
    {
      fprintf(stderr, "%s: Out of memory\n", ERR_NAME);
      exit(1);
    }
  
  inblock_len = fread(inblock,1,inblock_size,stdin);
      
  if ( do_cat )			/* Literal flag -> always ACT_CAT */
    {
      cat(stdout);
      done = 1;
    }
  else
    {
      while ( !done && inblock_len > 0 )
	{
	  dt = dtab;		/* Head of linked list */
	  while ( dt )
	    {
	      if ( dt->length < 1 ||
		  matches(inblock+dt->offset,dt->magic,dt->mask,dt->length) )
		break;
	      
	      dt = dt->next;
	    }
	  
	  if ( !dt )
	    {
	      fprintf(stderr, "%s: Unknown type data and no default; \
job sent by %s@%s)\n", ERR_NAME, user, host);
	      exit(1);
	    }
	  
	  /* Now dt should point to the proper data type record to use */
	  
	  switch( dt->action )
	    {
	    case ACT_CAT:
	      act_cat(dt->command);
	      break;
	      
	    case ACT_DROP:
	      act_drop();
	      break;
	      
	    case ACT_REJECT:
	      act_reject(dt->command, user, host);
	      break;
	      
	    case ACT_FILTER:
	      act_filter(dt->command);
	      break;
	      
	    case ACT_PIPETHRU:
	      act_pipethru(dt->command);
	      break;
	      
	    case ACT_ADDCR:
	      act_addcr(dt->command);
	      break;

	    case ACT_PS:
	      act_ps();
	      break;

	    case ACT_FFILTER:
	      act_ffilter(dt->command);
	      break;

	    case ACT_FPIPE:
	      act_fpipe(dt->command);
	      break;

	    default:
	      fprintf(stderr, "%s(%s): Internal error - invalid parse\n\
Please mail your configuration file to <Peter.Anvin@linux.org>\n",
		      program, config_file);
	      return 1;
	    }
	}

#ifdef REJECT_EMPTY
      if ( !done )
	{
	  /* Send a message to the user informing that his/her print job
	     ended up empty */

	  act_reject(REJECT_EMPTY, user, host);
	}
#endif
    }

  return 0;			/* Error */
}
