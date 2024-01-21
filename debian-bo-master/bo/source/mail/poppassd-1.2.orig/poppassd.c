/*
 * poppassd.c
 *
 * A Eudora and NUPOP change password server.
 *
 * John Norstad
 * Academic Computing and Network Services
 * Northwestern University
 * j-norstad@nwu.edu
 *
 * Based on earlier versions by Roy Smith <roy@nyu.edu> and Daniel
 * L. Leavitt <dll.mitre.org>.
 * 
 * Doesn't actually change any passwords itself.  It simply listens for
 * incoming requests, gathers the required information (user name, old
 * password, new password) and executes /bin/passwd, talking to it over
 * a pseudo-terminal pair.  The advantage of this is that we don't need
 * to have any knowledge of either the password file format (which may
 * include dbx files that need to be rebuilt) or of any file locking
 * protocol /bin/passwd and cohorts may use (and which isn't documented).
 *
 * The current version has been tested at NU under SunOS release 4.1.2 
 * and 4.1.3, and under HP-UX 8.02 and 9.01. We have tested the server 
 * with both Eudora 1.3.1 and NUPOP 2.0.
 *
 * Other sites report that this version also works under AIX and NIS,
 * and with PC Eudora.
 *
 * Note that unencrypted passwords are transmitted over the network.  If
 * this bothers you, think hard about whether you want to implement the
 * password changing feature.  On the other hand, it's no worse than what
 * happens when you run /bin/passwd while connected via telnet or rlogin.
 * Well, maybe it is, since the use of a dedicated port makes it slightly
 * easier for a network snooper to snarf passwords off the wire.
 *
 * NOTE: In addition to the security issue outlined in the above paragraph,
 * you should be aware that this program is going to be run as root by
 * ordinary users and it mucks around with the password file.  This should
 * set alarms off in your head.  I think I've devised a pretty foolproof
 * way to ensure that security is maintained, but I'm no security expert and
 * you would be a fool to install this without first reading the code and
 * ensuring yourself that what I consider safe is good enough for you.  If
 * something goes wrong, it's your fault, not mine.
 *
 * The front-end code (which talks to the client) is directly 
 * descended from Leavitt's original version.  The back-end pseudo-tty stuff 
 * (which talks to /bin/password) is directly descended from Smith's
 * version, with changes for SunOS and HP-UX by Norstad (with help from
 * sample code in "Advanced Programming in the UNIX Environment"
 * by W. Richard Stevens). The code to report /bin/passwd error messages
 * back to the client in the final 500 response, and a new version of the
 * code to find the next free pty, is by Norstad.
 *        
 * Should be owned by root, and executable only by root.  It can be started
 * with an entry in /etc/inetd.conf such as the following:
 *
 * poppassd stream tcp nowait root /usr/local/bin/poppassd poppassd
 * 
 * and in /etc/services:
 * 
 * poppassd	106/tcp
 *
 * Logs to the local2 facility. Should have an entry in /etc/syslog.conf
 * like the following:
 *
 * local2.err	/var/adm/poppassd-log
 */
 
/* Modification history.
 *
 * 06/09/93. Version 1.0.
 *
 * 06/29/93. Version 1.1.
 * Include program name 'poppassd' and version number in initial 
 *    hello message.
 * Case insensitive command keywords (user, pass, newpass, quit).
 *    Fixes problem reported by Raoul Schaffner with PC Eudora.
 * Read 'quit' command from client instead of just terminating after 
 *    password change.
 * Add new code for NIS support (contributed by Max Caines).
 *
 * 08/31/93. Version 1.2.
 * Generalized the expected string matching to solve several problems
 *    with NIS and AIX. The new "*" character in pattern strings
 *    matches any sequence of 0 or more characters.
 * Fix an error in the "getemess" function which could cause the
 *    program to hang if more than one string was defined in the
 *    P2 array.
 */

/* Steve Dorner's description of the simple protocol:
 *
 * The server's responses should be like an FTP server's responses; 
 * 1xx for in progress, 2xx for success, 3xx for more information
 * needed, 4xx for temporary failure, and 5xx for permanent failure.  
 * Putting it all together, here's a sample conversation:
 *
 *   S: 200 hello\r\n
 *   E: user yourloginname\r\n
 *   S: 300 please send your password now\r\n
 *   E: pass yourcurrentpassword\r\n
 *   S: 200 My, that was tasty\r\n
 *   E: newpass yournewpassword\r\n
 *   S: 200 Happy to oblige\r\n
 *   E: quit\r\n
 *   S: 200 Bye-bye\r\n
 *   S: <closes connection>
 *   E: <closes connection>
 */
 
#define VERSION "1.2"

#define SUCCESS 1
#define FAILURE 0
#define BUFSIZE 512

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>
#include <fcntl.h>
#include <syslog.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <strings.h>
#include <errno.h>
#include <stdarg.h>
#include <pwd.h>
#include <string.h>
#include <termios.h>
#include <dirent.h>
#include <getopt.h>

#ifdef HAS_SHADOW
#include <shadow.h>
#include <shadow/pwauth.h>
#ifndef PW_PPP
#define PW_PPP PW_LOGIN
#endif
char *pw_encrypt (char *, char *);	/* To permit long shadow passwords */
#define crypt pw_encrypt		/* for short passwords as well.    */
#endif

/*
 * Prototypes
 */

int main (int argc, char *argv[]);
int dochild (int master, char *slavedev, char *user);
int findpty (char **slave);
void writestring (int fd, char *s);
int talktochild (int master, char *user, char *oldpass, char *newpass,
		 char *emess);
int match (char *str, char *pat);
int expect (int master, char **expected, char *buf);
int getemess (int master, char **expected, char *buf);
void WriteToClient (char *fmt, ...);
void ReadFromClient (char *line);
int chkPass (char *user, char *pass, struct passwd *pw);

/* Prompt strings expected from the "passwd" command. If you want
 * to port this program to yet another flavor of UNIX, you may need to add
 * more prompt strings here.
 *
 * Each prompt is defined as an array of pointers to alternate 
 * strings, terminated by an empty string. In the strings, '*'
 * matches any sequence of 0 or more characters. Pattern matching
 * is forced to lower case so enter only lower case letters.
 */

static char *P1[] =
   {
     "changing password for *\nenter old password: ",
#if 0
     "changing nis password for * on *.\nold password: ",
#endif
     ""
   };

static char *P2[] =
   {
     "enter new password: ",			/* non-shadow passwords */
     "changing password for *\nnew password: ",	/* shadow passwords     */
     ""
   };

static char *P3[] =
   {
     "re-type new password: ",			/* non-shadow passwords	*/
     "re-enter new password: ",			/* shadow passwords	*/
     ""
   };
    
static char *P4[] =
   {
     "password changed. ",
     "nis entry changed on *",
     ""
   };

int verbose = 0;
struct passwd *pw;
char user[BUFSIZE];

int main (int argc, char *argv[])
{
     char line[BUFSIZE];
     char oldpass[BUFSIZE];
     char newpass[BUFSIZE];
     char emess[BUFSIZE];
     char *slavedev;
     int c, master;
     pid_t pid, wpid;
     int wstat, nopt;
     static char options[] = "v";

#ifdef HAS_SHADOW
    struct spwd *spwd;
    struct spwd *getspnam();
#endif

     *user    =
     *oldpass =
     *newpass = 0;

     nopt = getopt (argc, argv, options);
     while (nopt != -1)
       {
	 switch (nopt)
	   {
	   case '?':
	     fprintf (stderr, "invalid option\n");
	     exit (1);
	   case 'v':
	     verbose = 1;
	     break;
	   }
	 nopt = getopt (argc, argv, options);
       }

     openlog ("poppassd", LOG_PID, LOG_LOCAL2);

     gethostname(line, sizeof (line));
     WriteToClient ("200 %s poppassd v%s hello, who are you?", line, VERSION);

     ReadFromClient (line);
     sscanf (line, "user %s", user) ;
     if (strlen (user) == 0)
     {
	  WriteToClient ("500 Username required.");
	  return(1);
     }

     WriteToClient ("200 your password please.");
     ReadFromClient (line);
     sscanf (line, "pass %s", oldpass) ;

     if (strlen (oldpass) == 0)
     {
	  WriteToClient ("500 Password required.");
	  return(1);
     }

     WriteToClient ("200 your new password please.");
     ReadFromClient (line);
     sscanf (line, "newpass %s", newpass);
     
     /* new pass required */
     if (strlen (newpass) == 0)
     {
	  WriteToClient ("500 New password required.");
	  return(1);
     }
     
     if ((pw = getpwnam (user)) == NULL)
     {
	  WriteToClient ("500 Invalid user or password");
	  return(1);
     }

#ifdef HAS_SHADOW
    if ((spwd = getspnam(user)) == NULL)
	  pw->pw_passwd = "";
    else
	  pw->pw_passwd = spwd->sp_pwdp;
#endif

     if (chkPass (user, oldpass, pw) == FAILURE)
     {
	  WriteToClient ("500 Invalid user or password");
	  return(1);
     }

     /* get pty to talk to password program */
     if ((master = findpty (&slavedev)) < 0)
     {
	  syslog (LOG_ERR, "can't find pty");
          WriteToClient("500 Server busy - try again later.");
	  return (1);
     }
	 
     /* fork child process to talk to password program */
     if ((pid = fork()) < 0)     /* Error, can't fork */
     {
	  syslog (LOG_ERR, "can't fork for passwd: %m");
	  WriteToClient ("500 Server error (can't fork passwd), get help!");
	  return (1);
     }

     if (pid)   /* Parent */
     {
	  if (talktochild (master, user, oldpass, newpass, emess) == FAILURE)
	  {
	       syslog (LOG_ERR, "failed attempt by %s", user);
	       if (*emess == '\0') {
	          WriteToClient ("500 Unable to change password." );
               } else {
		  WriteToClient ("500 %s", emess);
               }
	       return(1);
	  }

	  if ((wpid = waitpid (pid, &wstat, 0)) < 0)
	  {
	       syslog (LOG_ERR, "wait for /bin/passwd child failed: %m");
	       WriteToClient ("500 Server error (wait failed), get help!");
	       return (1);
	  }

	  if (pid != wpid)
	  {
	       syslog (LOG_ERR, "wrong child (/bin/passwd waited for!");
	       WriteToClient ("500 Server error (wrong child), get help!");
	       return (1);
	  }

	  if (WIFEXITED (wstat) == 0)
	  {
	       syslog (LOG_ERR, "child (/bin/passwd) killed?");
	       WriteToClient ("500 Server error (funny wstat), get help!");
	       return (1);
	  }

	  if (WEXITSTATUS (wstat) != 0)
	  {
	       syslog (LOG_ERR, "child (/bin/passwd) exited abnormally");
	       WriteToClient ("500 Server error (abnormal exit), get help!");
	       return (1);
	  }

	  syslog (LOG_ERR, "password changed for %s", user);
	  WriteToClient ("200 Password changed, thank-you.");

          ReadFromClient (line);
	  if (strncmp(line, "quit", 4) != 0) {
	  	WriteToClient("500 Quit required.");
		return (1);
	  }
	  
	  WriteToClient("200 Bye.");
	  return (0);
     }
     else      /* Child */
     {
	  dochild (master, slavedev, user);
     }
}

/*
 * dochild
 *
 * Do child stuff - set up slave pty and execl /bin/passwd.
 *
 * Code adapted from "Advanced Programming in the UNIX Environment"
 * by W. Richard Stevens.
 *
 */

int dochild (int master, char *slavedev, char *user)
{
   int slave;
   struct termios stermios;

   /* Start new session - gets rid of controlling terminal. */
   
   if (setsid() < 0) {
      syslog(LOG_ERR, "setsid failed: %m");
      return(0);
   }

   /* Open slave pty and acquire as new controlling terminal. */

   if ((slave = open(slavedev, O_RDWR)) < 0) {
      syslog(LOG_ERR, "can't open slave pty: %m");
      return(0);
   }

   /* Close master. */

   close(master);

   /* Make slave stdin/out/err of child. */

   if (dup2(slave, STDIN_FILENO) != STDIN_FILENO) {
      syslog(LOG_ERR, "dup2 error to stdin: %m");
      return(0);
   }

   if (dup2(slave, STDOUT_FILENO) != STDOUT_FILENO) {
      syslog(LOG_ERR, "dup2 error to stdout: %m");
      return(0);
   }

   if (dup2(slave, STDERR_FILENO) != STDERR_FILENO) {
      syslog(LOG_ERR, "dup2 error to stderr: %m");
      return(0);
   }
   if (slave > 2) close(slave);

   /* Set proper terminal attributes - no echo, canonical input processing,
      no map NL to CR/NL on output. */

   if (tcgetattr(0, &stermios) < 0) {
      syslog(LOG_ERR, "tcgetattr error: %m");
      return(0);
   }

   stermios.c_lflag &= ~(ECHO | ECHOE | ECHOK | ECHONL);
   stermios.c_lflag |= ICANON;
   stermios.c_oflag &= ~(ONLCR);
   if (tcsetattr(0, TCSANOW, &stermios) < 0) {
      syslog(LOG_ERR, "tcsetattr error: %m");
      return(0);
   }

   /* Do some simple changes to ensure that the daemon does not mess */
   /* things up. */

   chdir ("/");
   umask (0);

/*
 * Shadow password suite looks the user in the login database. Since
 * poppassd does not 'login', it will fail. So, cheat. Keep root status
 * and pass the user on the command line.
 */

#ifdef HAS_SHADOW
   execl("/bin/passwd",     "passwd", user, (char*)0);
   execl("/usr/bin/passwd", "passwd", user, (char*)0);
#else

/*
 * Without the shadow password suite, the standard password program
 * looks at the uid for the user. Become the user and don't pass it
 * on the command line.
 */
   setregid (pw->pw_gid, pw->pw_gid);
   setreuid (pw->pw_uid, pw->pw_uid);

   execl("/bin/passwd",     "passwd",       (char*)0);
   execl("/usr/bin/passwd", "passwd",       (char*)0);
#endif

   syslog(LOG_ERR, "can't exec /bin/passwd: %m");
   return(0);
}


/*
 * findpty()
 *
 * Finds the first available pseudo-terminal master/slave pair.  The master
 * side is opened and a fd returned as the function value.  A pointer to the
 * name of the slave side (i.e. "/dev/ttyp0") is returned in the argument,
 * which should be a char**.  The name itself is stored in a static buffer.
 *
 * A negative value is returned on any sort of error.
 *
 * Modified by Norstad to remove assumptions about number of pty's allocated
 * on this UNIX box.
 */

int findpty (char **slave)
{
   int master;
   static char line[] = "/dev/ptyXX";
   DIR *dirp;
   struct dirent *dp;

   dirp = opendir("/dev");
   while ((dp = readdir(dirp)) != NULL) {
      if (strncmp(dp->d_name, "pty", 3) == 0 && strlen(dp->d_name) == 5) {
         line[8] = dp->d_name[3];
         line[9] = dp->d_name[4];
         if ((master = open(line, O_RDWR)) >= 0) {
            line[5] = 't';
            *slave = line;
            closedir(dirp);
            return (master);
         }
      }
   }
   closedir(dirp);
   return (-1);
}

/*
 * writestring()
 *
 * Write a string in a single write() system call.
 */
void writestring (int fd, char *s)
{
     int l;

     l = strlen (s);
     write (fd, s, l);
     if (verbose)
         syslog(LOG_DEBUG, "write: %s", s);
}

/*
 * talktochild()
 *
 * Handles the conversation between the parent and child (password program)
 * processes.
 *
 * Returns SUCCESS is the conversation is completed without any problems,
 * FAILURE if any errors are encountered (in which case, it can be assumed
 * that the password wasn't changed).
 */
int talktochild (int master, char *user, char *oldpass, char *newpass,
		 char *emess)
{
     char buf[BUFSIZE];
     char pswd[BUFSIZE+1];
     int m, n;

     *emess = 0;

#ifndef HAS_SHADOW
     if (!expect(master, P1, buf)) return FAILURE;

     sprintf(pswd, "%s\n", oldpass);
     writestring(master, pswd);
#endif

     if (!expect(master, P2, buf)) return FAILURE;

     sprintf(pswd, "%s\n", newpass);
     writestring(master, pswd);

     if (!expect(master, P3, buf)) {
        getemess(master, P2, buf);
	strcpy(emess, buf);
	return FAILURE;
     }

     writestring(master, pswd);

#ifndef HAS_SHADOW  /* shadow prints no success message :( */
     if (!expect(master, P4, buf)) return FAILURE;
#endif

     return SUCCESS;
}

/*
 * match ()
 *
 * Matches a string against a pattern. Wild-card characters '*' in
 * the pattern match any sequence of 0 or more characters in the string.
 * The match is case-insensitive.
 *
 * Entry: str = string.
 *        pat = pattern.
 *
 * Exit:  function result =
 *		0 if no match.
 *		1 if the string matches some initial segment of
 *		  the pattern.
 *		2 if the string matches the full pattern.
 */
int match (char *str, char *pat)
{
   int result;

   while (*str && *pat) {
     if (*pat == '*')
       break;

     /* ignore multiple space sequences */
     if (*pat == ' ' && isspace (*str)) {
       ++pat;
       do
	 ++str;
       while (isspace (*str) && *str != '\0');
       continue;
     }

     /* ignore multiple newline sequences when found */
     if (*pat == '\n' && (*str == '\n' || *str == '\r')) {
       ++pat;
       do
	 ++str;
       while (isspace (*str) && *str != '\0');
       continue;
     }

     /* Process all other characters as a literal */
     if (tolower(*str) != *pat++)
       return 0;

     ++str;
   }

   if (*str == 0)
     return *pat == 0 ? 2 : 1;

   if (*pat++ == 0)
     return 0;

   while (*str) {
     result = match(str, pat);
     if (result != 0)
       break;
     ++str;
   }

   return result;
}

/*
 * expect ()
 *
 * Reads 'passwd' command output and compares it to expected output.
 *
 * Entry: master = fid of master pty.
 *	  expected = pointer to array of pointers to alternate expected
 *            strings, terminated by an empty string.
 *        buf = pointer to buffer.
 *
 * Exit:  function result = SUCCESS if output matched, FAILURE if not.
 *        buf = the text read from the slave.
 *
 * Text is read from the slave and accumulated in buf. As long as
 * the text accumulated so far is an initial segment of at least 
 * one of the expected strings, the function continues the read.
 * As soon as one of full expected strings has been read, the
 * function returns SUCCESS. As soon as the text accumulated so far
 * is not an initial segment of or exact match for at least one of 
 * the expected strings, the function returns FAILURE.
 */

int expect (int master, char **expected, char *buf)
{
     int n, m;
     char **s;
     char *p;
     int initialSegment;
     
     buf[0] = 0;
     while (1) {
	n = strlen (buf);
     	if (n >= BUFSIZE-1) {
	   syslog(LOG_ERR, "buffer overflow on read from child");
	   return FAILURE;
	}

	m = read (master, &buf[n], BUFSIZ-1-n);
	if (m < 0) {
	   syslog(LOG_ERR, "read error from child: %m");
	   return FAILURE;
	}

	buf[n+m] = '\0';
	strcat (buf, "\n");

	p = strchr (&buf[n], '\r');
	while (p != (char *) 0) {
	    *p = ' ';
	    p = strchr (&buf[n], '\r');
	}

	if (verbose)
	    syslog (LOG_DEBUG, "read: %s\n", &buf[n]);

	/* Ignore leading whitespace. It gets in the way. */
	p = buf;
	while (isspace (*p))
	    ++p;

	if (*p == '\0')
	    continue;

	initialSegment = 0;
        for (s = expected; **s != 0; s++) {
            switch (match(p, *s)) {
	    case 2:
	        if (verbose)
	            syslog (LOG_DEBUG, "expect: succes\n");
	        return SUCCESS;
            case 1:
	        initialSegment = 1;
	    default:
	        break;
	    }
	}

	if (!initialSegment) {
	  if (verbose)
	    syslog (LOG_DEBUG, "expect: failure\n");
	  return FAILURE;
	}
     }
}

/*
 * getemess()
 *
 * This function accumulates a 'passwd' command error message issued
 * after the first copy of the password has been sent.
 *
 * Entry: master = fid of master pty.
 *	  expected = pointer to array of pointers to alternate expected
 *            strings for first password prompt, terminated by an 
 *            empty string.
 *        buf = pointer to buffer containing text read so far.
 *
 * Exit:  buf = the error message read from the slave.
 *
 * Text is read from the slave and accumulated in buf until the text
 * at the end of the buffer is an exact match for one of the expected
 * prompt strings. The expected prompt string is removed from the buffer,
 * returning just the error message text. Newlines in the error message
 * text are replaced by spaces.
 */
int getemess (int master, char **expected, char *buf)
{
   int n, m;
   char **s;
   char *p, *q;

   n = strlen(buf);
   while (1) {
      for (s = expected; **s != 0; s++) {
         for (p = buf; *p; p++) {
            if (match(p, *s) == 2) {
               *p = 0;
               for (q = buf; *q; q++) if (*q == '\n') *q = ' ';
               return;
            }
         }
      }
      if (n >= BUFSIZE-1) {
	 syslog(LOG_ERR, "buffer overflow on read from child");
	 return;
      }
      m = read(master, buf+n, BUFSIZE+1-n);
      if (m < 0) {
	 syslog(LOG_ERR, "read error from child: %m");
	 return;
      }
      n += m;
      buf[n] = 0;

      if (verbose)
	syslog (LOG_DEBUG, "read: %s\n", buf);
   }
}

void WriteToClient (char *fmt, ...)
{
	va_list ap;
	
	va_start (ap, fmt);
	vfprintf (stdout, fmt, ap);
	fputs ("\r\n", stdout );
	fflush (stdout);
	va_end (ap);
}

void ReadFromClient (char *line)
{
	char *sp;
	int i;

	strcpy (line, "");
	fgets (line, BUFSIZE, stdin);
	if ((sp = strchr(line, '\n')) != NULL) *sp = '\0'; 
	if ((sp = strchr(line, '\r')) != NULL) *sp = '\0'; 
	
	/* convert initial keyword on line to lower case. */
	
	for (sp = line; isalpha(*sp); sp++) *sp = tolower(*sp);
}

int chkPass (char *user, char *pass, struct passwd *pw)
{
     /*  Compare the supplied password with the password file entry */
     if (strcmp (crypt (pass, pw->pw_passwd), pw->pw_passwd) != 0)
	  return (FAILURE);
     else 
	  return (SUCCESS);
}
