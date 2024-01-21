#include <stdio.h>
#include <signal.h>
#include <fcntl.h>
#include <pwd.h>
#include "nfs_prot.h"
#include "mp.h"
#include "version.h"
#include "config.h"
#if defined (__SVR4) || defined(__sgi)
#include <stdlib.h>		/* getenv */
#include <string.h>		/* strcmp */
#include <unistd.h>		/* getuid */
#endif

#include <dirent.h>

extern void nfs_program_2();

static char
  *dev  = DDEV,
  *user,
  *dir  = DDIR;
static int
  speed = 0;

int psionfd, gmtoffset, debug, exiting, psion_alive, dowakeup = 0, old_nfsc = 0,
    query_cache = 0, background = 1;
fattr root_fattr = {
  NFDIR, 0040500, 1, 0, 0,
  BLOCKSIZE, BLOCKSIZE, FID, 1, FID, 1,
  {0, 0},
  {0, 0},
  {0, 0}
};

char *disconnprog, *connprog, *shell = 0;

#if defined(hpux) || defined(__SVR4) || defined(__sgi)
void
usleep(usec)
int usec;
{
  struct timeval t;

  t.tv_sec = (long) (usec / 1000000);
  t.tv_usec= (long) (usec % 1000000);
  select(0, (fd_set *)0, (fd_set *)0, (fd_set *)0, &t);
}
#endif /* hpux */

void
main(ac, av)
  int ac;
  char *av[];
{
  struct passwd *pw;
  struct timeval tv;
  struct timezone tz;
  p_inode *rp;
  nfs_fh root_fh;
  DIR *dirp;
  struct dirent *diep;
  int i;


  if(!(user = (char *)getenv("USER")))
    user = (char *)getenv("logname");
    
  while(ac > 1)
    {
      if(!strcmp(av[1], "-v"))
	{
	  debug++;
	}
      else if(!strcmp(av[1], "-dir"))
	{
	  dir = av[2]; ac--;av++;
	}
      else if(!strcmp(av[1], "-user") || !strcmp(av[1], "-u"))
	{
	  user = av[2]; ac--;av++;
	}
      else if(!strcmp(av[1], "-conn"))
	{
	  connprog = av[2]; ac--; av++;
	}
      else if(!strcmp(av[1], "-disconn"))
	{
	  disconnprog = av[2]; ac--; av++;
	}
      else if(!strcmp(av[1], "-wakeup"))
	{
	  dowakeup = 1;
	}
      else if(!strcmp(av[1], "-tty"))
	{
	  dev = av[2]; ac--;av++;
	}
#ifdef PTY
      else if(!strcmp(av[1], "-shell"))
	{
	  shell = av[2]; ac--;av++;
	}
#endif
      else if(!strcmp(av[1], "-oldnfsc"))
	{
	  old_nfsc = 1;
	}
      else if (!strcmp(av[1], "-speed"))
        {
	  speed = atoi(av[2]);
	  ac--; av++;
	}
      else if (!strcmp(av[1], "-"))
        {
	  background = 0;
	}
      else
	{
	  printf("p3nfsd version %s\n", VERSION); 
	  printf("Usage: p3nfsd [-dir directory] [-user username] [-tty ser-dev] [-speed baudrate]\n");
	  printf("              [-conn prog] [-disconn prog] [-wakeup] [-v]\n");
#ifdef PTY
	  printf("              [-shell prog] [-oldnfsc] [-]\n");
#else
	  printf("              [-oldnfsc] [-]\n");
#endif
	  printf("Defaults: -dir %s -user %s -tty %s -speed %d\n", dir, user, dev, DSPEED);
	  exit(1);
	}
      ac--;av++;
    }
  
  if (user && *user)
    {
      if(!(pw = getpwnam(user)))
	{
	  fprintf(stderr, "User %s not found.\n", user);
	  exit(1);
	}
    }
  else if(!(pw = getpwuid(getuid())))
    {
      fprintf(stderr, "You don't exist, go away!\n");
      exit(1);
    }
  if (getuid() && pw->pw_uid != getuid())
    {
      fprintf(stderr, "%s? You must be kidding...\n", user);
      exit(1);
    }
  root_fattr.uid = pw->pw_uid;
  root_fattr.gid = pw->pw_gid;
  endpwent();

  gettimeofday(&tv, &tz);
#ifndef __SVR4
  gmtoffset = -tz.tz_minuteswest * 60;
#else
  tzset();
  gmtoffset = -timezone;
#endif

  if(!background)
    dev = "stdin";
  if(speed == 0 && background)
    speed = DSPEED;
  printf("p3nfsd: version %s, using %s (%d), mounting on %s\n", VERSION, dev, speed, dir);


  /* Check if mountdir is empty (or else you can overmount e.g /etc)
     It is done here, because exit hangs, if hardware flowcontrol is
     not present. Bugreport Nov 28 1996 by Olaf Flebbe */
  if(!(dirp = opendir(dir)))
    {
      perror(dir);
      exit(1);
    }
  i = 0;
  while((diep = readdir(dirp)) != 0)
    if(strcmp(diep->d_name, ".") && strcmp(diep->d_name, ".."))
      i++;
  closedir(dirp);
  if(i)
    {
      fprintf(stderr, "Sorry, directory %s is not empty, exiting.\n", dir);
      exit(1);
    }


  if (!old_nfsc && shell)
    init_pty();

  psionfd = init_serial(dev, speed);

  rp = get_nam("");
  inode2fh(rp->inode, root_fh.data);
  root_fattr.fileid = rp->inode;
  root_fattr.atime.seconds = root_fattr.mtime.seconds = 
  root_fattr.ctime.seconds = tv.tv_sec;

  mount_and_run(dir, dev, nfs_program_2, &root_fh);
  exit(0);
}
