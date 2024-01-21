/*
 *       open.c open a vt to run a new command (or shell).
 *       
 *	 Copyright (c) 1994 by Jon Tombs <jon@gtex02.us.es>
 *
 *       This program is free software; you can redistribute it and/or
 *       modify it under the terms of the GNU General Public License
 *       as published by the Free Software Foundation; either version
 *       2 of the License, or (at your option) any later version.
 */

#include "open.h"

const char *OPENversion = "open: 1.4 (c) Jon Tombs 1994";

#ifndef VTNAME
#error vt device name must be defined in open.h
#endif


int 
main(int argc, char *argv[])
{

   int fd = 0;
   int opt, pid;
   struct vt_stat vt;
   struct passwd *pwnam=NULL;
   int vtno     = -1;
   char show    = FALSE;
   char login   = FALSE;
   char verbose = FALSE;
   char do_wait	= FALSE;
   char as_user= FALSE;
   char vtname[sizeof VTNAME + 2]; /* allow 999 possible VTs */
   char *cmd, *def_cmd = NULL;

   /*
    * I don't like using getopt for this, but otherwise this gets messy.
    * POSIX/Gnu getopt forces the use of -- to separate child/program
    * options. RTFM.
    */
   while ((opt = getopt(argc, argv, "c:lsvuw")) != -1) {
      switch (opt) {
	case 'c':
	  vtno = (int) atol(optarg);
	  if (vtno < 0 || vtno > 99) {	  
	    fprintf(stderr, "open: %s illegal vt number\n", optarg); 
	    return 5;
	  }
	  /* close security holes - until we can do this safely */
	  (void) setuid(getuid());
	  break;
	case 'l':
	  login = TRUE;
	  break;
	case 's':
	  show = TRUE;
	  break;
	case 'v':
	  verbose = TRUE;
	  break;	    	    
	case 'w':
	  do_wait = TRUE;
	  break;
	case 'u':
          /* we'll let 'em get away with the meaningless -ul combo */
          if(getuid()) {
		fprintf(stderr,"%s: only root can use the -u flag.\n",argv[0]);
		exit(1);
          }
	  as_user = TRUE;
	  break;
	default:
	  usage(1);
	
      }
   }
	    


   if (vtno == -1) {
     if ((fd = open("/dev/console",O_WRONLY,0)) < 0) {
	perror("open: Failed to open /dev/console\n");	
	return(2);
     }

     
     if ((ioctl(fd, VT_OPENQRY, &vtno) < 0) || (vtno == -1)) {
        perror("open: Cannot find a free VT\n");
        close(fd);
        return(3);
     }

     if (ioctl(fd, VT_GETSTATE, &vt) < 0) {
	perror("open: can't get VTstate\n");
        close(fd);
        return(4);
     }
   }

   sprintf(vtname, VTNAME, vtno);

/* support for Spawn_Console; running from init
added by Joshua Spoerri, Thu Jul 18 21:13:16 EDT 1996 */
if (as_user) {
	DIR *dp;
	struct dirent *dentp;
	struct stat buf;
	dev_t console_dev;
	ino_t console_ino;
	uid_t console_uid;
	char filename[NAME_MAX+12];

	if (!(dp=opendir("/proc"))) {
		perror("/proc");
		exit(1);
	}
	
	/* get the current tty */
	sprintf(filename,"/dev/tty%d",vt.v_active);
	if (stat(filename,&buf)) {
		perror(filename);
		exit(1);
	}
	console_dev=buf.st_dev;
	console_ino=buf.st_ino;
	console_uid=buf.st_uid;

	/* get the owner of current tty */
	if (!(pwnam=getpwuid(console_uid))) {
		perror("can't getpwuid");
		exit(1);
	}

	/* check to make sure that user has process on that tty */
	while ((dentp=readdir(dp))) {
		sprintf(filename,"/proc/%s/fd/0",dentp->d_name);
		if (stat(filename,&buf)) /*stat will fail if prcs lacks stdin*/
			continue;
		if(buf.st_dev == console_dev && buf.st_ino == console_ino
				&& buf.st_uid == console_uid)
			break;
	}

	if(!(buf.st_dev == console_dev && buf.st_ino == console_ino
				&& buf.st_uid == console_uid)) {
		fprintf(stderr,"couldn't find owner of current tty!\n");
		exit(1);
	}

} else {
   if (!geteuid()) {
      uid_t uid = getuid();
      chown(vtname, uid, getgid());
      setuid(uid);
   }
}

   if (verbose)
	fprintf(stderr,	"open: using VT %s\n", vtname);
	
if(!as_user) {
   if (!(argc > optind)) {
      def_cmd = getenv("SHELL");
      if (def_cmd == NULL)
	usage(0);
      cmd = malloc(strlen(def_cmd + 2));
   } else {
      cmd = malloc(strlen(argv[optind] + 2));
   }

   if (login)
      strcpy(cmd, "-");
   else
      cmd[0] = '\0';

   if (def_cmd)
      strcat(cmd, def_cmd);
   else
      strcat(cmd, argv[optind]);

   if (login) 
      argv[optind] = cmd++;
}

   if((pid=fork()) == 0) {
      /* leave current vt */
#ifdef   ESIX_5_3_2_D
      if (setpgrp() < 0) {
#else
      if (setsid() < 0) {
#endif      
        fprintf(stderr, "open: Unable to set new session (%s)\n",
	strerror(errno));
      }
      close(0);
      close(1);
      close(2);
      close(fd);	 

      /* and grab new one */
      if ((fd = open(vtname, O_RDWR)) == -1) { /* Shouldn't happen */
        _exit (4); /* silently die */
      }
      dup(fd); dup(fd);

      if (show) {
	/* 
         * Can't tell anyone if any of these fail, so throw away
	 * the return values 
         */
        (void) ioctl(fd, VT_ACTIVATE, vtno);
        /* wait to be really sure we have switched */
	(void) ioctl(fd, VT_WAITACTIVE, vtno);
      }
      if(as_user)
	 execlp("login","login","-f",pwnam->pw_name,NULL);
      else if (def_cmd)
         execlp(cmd, def_cmd, NULL);
      else
	 execvp(cmd, &argv[optind]);
   }
   if ( pid < 0 ) {
      perror("open: fork() error");
      return(6);
   }


   if ( do_wait ) {
      wait(NULL);
      if (show) { /* Switch back... */
	 (void) ioctl(fd, VT_ACTIVATE, vt.v_active);
	 /* wait to be really sure we have switched */
	 (void) ioctl(fd, VT_WAITACTIVE, vt.v_active);
      }
   }

   close (fd);
   return 0;
}
      

void usage(int stat)
{
   fprintf(stderr,
      "Usage: open [-c vtnumer ][-l] [-u] [-s] [-v] -- command_line\n");
   exit (stat);
}


