/*
 *  sliphangup v1.4, Copyright (C) Karl Keyte, 1994-1996
 *
 *  Program to hang-up a dip SLIP line after a specified number of
 *  seconds of inactivity.  This allows the line to be held open
 *  while things like news and mail transfer are going on.
 *
 *  Use: sliphangup -h        to get usage information.
 *
 *  Note that the program goes into the background automatically.
 *
 *  IMPORTANT
 *  ---------
 *  The normal use of this program would be to keep the line open
 *  indefinitely until inactivity is detected.  The author accepts
 *  no responsibility for whatever reason if the line does not hang
 *  up.  It may be best to guard the line with a crontab entry to
 *  hang-up the line anyway so that you can satisfy yourself that
 *  it works before risking any huge phone bills!
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <syslog.h>
#include <fcntl.h>
#include <termios.h>
#include <sys/ioctl.h>
#include <sys/signal.h>


/*
 *  Defaults - the first three may be overridden on the command line
 */
const int   IDLE_TIME  = 60;
const char *INTERFACE  = "sl0";
const char *DIP_EXEC   = "/usr/local/sbin/dip";

const char *DIP_PID    = "/etc/dip.pid";
const char *ROUTE_INFO = "/proc/net/dev";
const char *VERSION    = "1.4";


long getUseCount (char *);
void goDaemon    ();
void logError    (int, char *);
int  pollIdle    (char *, int);
void killDip     (char *);
void forceKill   (void);
int  findFile    (char *, char *);


int main(int argc, char *argv[])
{
   int        idleTime;
   char       dipExec[256],
              interface[10],
              msg[256];

/*
 *  Crude test for help request!
 */
   if (argc > 1 && argv[1][0] == '-' && argv[1][1] == 'h')
      argc = 99;

/*
 *  Display help information
 */
   if (argc > 4)
   {
      fprintf(stderr, "sliphangup - Version %s, Copyright (c) 1995,1996, Karl Keyte\n",
              VERSION);
      fprintf(stderr, "usage: sliphangup [seconds-idle] [interface] [dip-program]\n");
      fprintf(stderr, "       Defaults are:  seconds-idle     = %d\n", IDLE_TIME);
      fprintf(stderr, "       Defaults are:  interface        = %s\n", INTERFACE);
      fprintf(stderr, "       Defaults are:  dip-program      = %s\n", DIP_EXEC);
      return -1;
   }

/*
 *  Set-up operational parameters
 */
   idleTime = (argc>1)? atoi(argv[1]) : IDLE_TIME;
   strcpy(interface, (argc>2)? argv[2] : INTERFACE);
   strcpy(dipExec, (argc>3)? argv[3] : DIP_EXEC);

/*
 *  Zero idle time is meaningless
 */
   if (!idleTime)
   {
      fprintf(stderr, "Idle time must be greater than zero seconds\n");
      return -1;
   }

/*
 *  Check that we can access 'dip' as an executable or we will have
 *  problems killing it when we need to hang up the line.  If it isn't
 *  where we were told, we'll have a go at finding it from the PATH.
 *  If it isn't anywhere there either we'll try to find the PID file
 *  and force kill it right away because we'll have problems otherwise.
 */
   if (access(dipExec, X_OK))
   {
      if (findFile("dip", dipExec))
      {
         sprintf(msg, "Can't find the executable %s - can't continue!",
                 dipExec);
         fprintf(stderr, "sliphangup: %s\n", msg);
         logError(LOG_ERR, msg);
         forceKill();
         return -1;
      } else {
         sprintf(msg, "Warning! 'dip' seems to be located at %s", dipExec);
         fprintf(stderr, "sliphangup: %s\n", msg);
         logError(LOG_WARNING, msg);
      }
   }

/*
 *  Push process into the background
 */
   switch(fork())
   {
      case -1:     logError(LOG_ERR, "Can't fork process");
                   return -1;
      case 0:      break;
      default:     return 0;
   }

/*
 *  Become a daemon!
 */
   goDaemon();

   if (!pollIdle(interface, idleTime))
   {
      sprintf(msg, "line '%s' hanging up after %d seconds inactivity",
              interface, idleTime);
      logError(LOG_NOTICE, msg);
   } else
      logError(LOG_WARNING, "hangup up line - error detecting use count");

/*
 *  We should kill the connection in all cases here - either the interface
 *  hasn't been used for the prescribed time or there's an error in
 *  detecting it.  In the latter case we either kill the connection or we
 *  accept the fact that we can't detect it and we get a never-ending
 *  connection!
 */
   killDip(dipExec);
   return 0;
}


/*
 *  Routine to monitor the interface usage and return if the timeout
 *  period is reached without activity.
 */
int pollIdle(char *iface, int timeout)
{
   long      currentUse,
             newUse;
   int       pauseTime,
             passes = 0;

   if ((currentUse = getUseCount(iface)) == -1)
      return -1;
   pauseTime = (timeout < 5)? timeout : 5;

   do
   {
      sleep(pauseTime);
      if ((newUse = getUseCount(iface)) == -1)
         return -1;
      if (newUse == currentUse)
         passes++;
      else
      {
         currentUse = newUse;
         passes = 0;
      }
   } while (passes*pauseTime < timeout);

   return 0;
}


/*
 *  Kill active dip program using dip itself - Only safe way!  If it
 *  returns, something odd happened and we'll try to force a kill.
 */
void killDip(char *dipExec)
{
   execl(dipExec, "dip", "-k", NULL);
   forceKill();
}


/*
 *  For dip kill based on PID (in emergency only
 */
void forceKill(void)
{
   char buf[128];
   int pid;
   FILE *dip_pid;

   if ((dip_pid = fopen(DIP_PID, "r")) == NULL)
      return;
   fgets(buf, 128, dip_pid);
   pid = atoi(buf);
   kill(pid, SIGTERM);
   fclose(dip_pid);
   unlink(DIP_PID);
}


/*
 *  Calculate the interface usage by looking at the /proc filesystem
 */
long getUseCount(char *iface)
{
   FILE         * route;
   char           buf[256],
                * s = NULL;
   int            foundIface = 0;
   long           pktIn,
                  pktOut;

/*
 *  Open the route information file
 */
   if ((route = fopen(ROUTE_INFO, "r")) == NULL)
   {
      logError(LOG_WARNING, "Cannot locate /proc device file\n");
      return -1;
   }

/*
 *  Calculate which field should be examined
 */
   while (fgets(buf, 256, route) != NULL) {
      if (strchr(buf, ':') == NULL)
         continue;
      s = strtok(buf, ":");
      while (*s == ' ')
         s++;
      if (!strcmp(s, iface)) {
         foundIface = 1;
         break;
      }
   }

/*
 *  If no interface entries were actually found, drop out
 */
   if (!foundIface)
   {
      logError(LOG_WARNING, "Interface does not exist - maybe closed down\n");
      return -1;
   }

   fclose(route);

   sscanf(s+strlen(iface)+1, "%ld %*d %*d %*d %*d %ld", &pktIn, &pktOut);

/*
 *  Return total usage count
 */
   return pktIn+pktOut;
}


/*
 *  Make daemon by closing file descriptors and releasing tty
 */
void goDaemon()
{
   int fd;

   close(0);
   close(1);
   close(2);

   if ((fd = open("/dev/tty", O_RDWR)) >= 0) 
   {
      ioctl(fd, TIOCNOTTY, NULL);
      close(fd);
   }
}


/*
 *  Log message to the syslog
 */
void logError(int severity, char *msg)
{
   openlog("sliphangup", LOG_PID, LOG_USER);
   syslog(severity, msg);
   closelog();
}


/*
 *  Locate a file in the PATH given its name
 */
int findFile(char *name, char *path)
{
   char         nullchar = '\0',
               *pc,
                save,
                test[512];
   int          found,
                len,
                namelen;

   if ((pc = getenv("PATH")) == NULL)
      pc = &nullchar;
   namelen = strlen(name);
   found = 0;
   while (*pc != '\0' && found == 0)
   {
      len = 0;
      while (*pc != ':' && *pc != '\0')
      {
         len++;
         pc++;
      }

      save = *pc;
      *pc = '\0';
      sprintf(test, "%s/%s", pc-len, name);
      *pc = save;
      if (*pc)
         pc++;

      found = (access(test, X_OK) == 0);
      if (found)
         strcpy(path, test);
   }

   if (found == 0)
      return 1;

   return 0;
}
