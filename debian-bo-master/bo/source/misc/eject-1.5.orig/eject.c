/*
 *		L I N U X   E J E C T   C O M M A N D
 *
 *             by Jeff Tranter (jeff_tranter@pobox.com)
 *
 * Updated with the help of Dick Streefland, Mark Lord, Doug
 * L. Hoffman (hoffman@cs.unc.edu), Ben Galliart (bgallia@luc.edu),
 * Markus Pilzecker (markus.pilzecker@rhein-neckar.netsurf.de), 
 * Donnie Barnes (djb@redhat.com), Grant Guenther (grant@torque.net),
 * and Erik Troan (ewt@redhat.com).
 *
 ********************************************************************
 *
 * Copyright (C) 1994-97 Jeff Tranter (jeff_tranter@pobox.com)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 ********************************************************************
 *
 * See the man page for a description of what this program does and what
 * the requirements to run it are.
 *
*/

#include <stdlib.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <sys/mount.h>
#include <linux/cdrom.h>
#include <linux/ucdrom.h>
#include <sys/stat.h>
#include <linux/fd.h>
#include <sys/sysmacros.h>
#include <linux/major.h>
#include <sys/wait.h>

/* set the default device here */
char default_device[1024];

/* default options */
int f_option = 0;  /* force eject even if mounted */
int d_option = 0;  /* display default device */
int n_option = 0;  /* display nicknames */
int a_option = 0;  /* auto-eject */
int a_arg    = 0;  /* argument to -a option */
int v_option = 0;  /* verbose mode */
int u_option = 0;  /* unmount first */
int h_option = 0;  /* request help */
int c_option = 0;  /* choose to load a CD slot on a IDE/ATAPI changer */
int c_arg    = 0;  /* argument to -c option */
char device[1024];  /* device name */
struct stat mystat; /* struct to help find out what type device we have */

/* data structure for device nicknames */
typedef struct {
  char *name;
  char *device;
} nickname_t;

/* table of device nicknames */
nickname_t nickname[] = { 
  { "cd0",    "/dev/cd0" },
  { "cd1",    "/dev/cd1" },
  { "cdrom",  "/dev/cdrom" },
  { "cdu31a", "/dev/cdu31a" },
  { "cdu535", "/dev/cdu535" },
  { "fd0",    "/dev/fd0" },
  { "floppy", "/dev/fd0" },
  { "hdb",    "/dev/hdb" },
  { "hdc",    "/dev/hdc" },
  { "hdd",    "/dev/hdd" },
  { "jaz",    "/dev/jaz" },
  { "lmscd",  "/dev/lmscd" },
  { "mcd",    "/dev/mcd" },
  { "sbpcd" , "/dev/sbpcd" },
  { "sbpcd0", "/dev/sbpcd0" },
  { "sbpcd1", "/dev/sbpcd1" },
  { "sbpcd2", "/dev/sbpcd2" },
  { "sbpcd3", "/dev/sbpcd3" },
  { "scd0",   "/dev/scd0" },
  { "scd1",   "/dev/scd1" },
  { "sr0",    "/dev/sr0" },
  { "sr1",    "/dev/sr1" },
  { "sra",    "/dev/sra" },
  { "srb",    "/dev/srb" },
  { "zip",    "/dev/zip" },
  /* next entry indicates end of table */
  { 0, 0 }
};

#define SCSI_IOCTL_SEND_COMMAND 1

struct sdata {
  int  inlen;
  int  outlen;
  char cmd[256];
} scsi_cmd;

/* display command usage on standard error and exit */
void usage()
{
  fprintf(stderr,
"Eject version 1.5 by Jeff Tranter (jeff_tranter@pobox.com)\n"
"\n"
"Usage:\n"
"  eject -h                           -- display this screen and exit\n"
"  eject [-f][-u][-v]                 -- eject default device\n"
"  eject [-f][-u][-v] <nickname>      -- eject device by nickname\n"
"  eject [-f][-u][-v] <device-name>   -- eject device by device name\n"
"  eject -d                           -- display default device\n"
"  eject -n                           -- display device nicknames\n"
"  eject -a [on|off|1|0] [-v]         -- turn auto-eject feature on or off\n"
"  eject -c <slot> [-v]               -- switch CDs on an IDE cdrom changer\n"
"\n"
"Options:\n"
"    -f  force eject even if device is mounted.\n"
"    -u  unmount before forcing an eject.\n"
"    -v  enables verbose output.\n"
);
  exit(1);
}

/* handle command line options */
void parse_args(int argc, char **argv)
{
  const char     *flags = "hfdna:c:uv";
  int             c;

  while ((c = getopt(argc, argv, flags)) != EOF) {
    switch (c) {
    case 'h':
      usage();
      break;
    case 'f':
      f_option = 1;
      break;
    case 'd':
      d_option = 1;
      break;
    case 'n':
      n_option = 1;
      break;
    case 'a':
      a_option = 1;
      /* make some poor guesses at what means on and off */
      if (!strcmp(optarg, "0"))
	a_arg = 0;
      else if (!strcmp(optarg, "off"))
	a_arg = 0;
      else if (!strcmp(optarg, "ferme"))     /* pardon my French */
	a_arg = 0;
      else if (!strcmp(optarg, "aus"))       /* German */
	a_arg = 0;
      else if (!strcmp(optarg, "disattiva")) /* Italian */
	a_arg = 0;
      else if (!strcmp(optarg, "1"))
	a_arg = 1;
      else if (!strcmp(optarg, "on"))
	a_arg = 1;
      else if (!strcmp(optarg, "allumee"))
	a_arg = 1;
      else if (!strcmp(optarg, "auf"))
	a_arg = 1;
      else if (!strcmp(optarg, "attiva"))
	a_arg = 1;
      else
	usage();
      break;
    case 'c':
      c_option = 1;
      c_arg = atoi(optarg);
      break;
    case 'u':
      u_option = 1;
      break;
    case 'v':
      v_option = 1;
      break;
    case '?':
      usage();
      break;
    }
  }

  /* check for a single additional argument */
  if ((argc - optind) > 1)
    usage(); /* too many arguments */
  
  if ((argc - optind) == 1)
    strncpy(device, argv[optind], sizeof(device)-1); /* one argument */
}

/* determine if device is a jaz...returns 1 if true */
int
is_jaz( int fd )
{
  char  id[25];
  int   i;

  scsi_cmd.inlen = 0;
  scsi_cmd.outlen = 40;
  scsi_cmd.cmd[0] = 0x12;               /* inquiry */
  scsi_cmd.cmd[1] = 0;
  scsi_cmd.cmd[2] = 0;
  scsi_cmd.cmd[3] = 0;
  scsi_cmd.cmd[4] = 40;
  scsi_cmd.cmd[5] = 0;

  if (ioctl(fd,SCSI_IOCTL_SEND_COMMAND,(void *)&scsi_cmd))
    return 0;  /* if the SCSI test fails, it is certainly not a JAZ or ZIP */

  for(i=0;i<24;i++) {
    id[i] = scsi_cmd.cmd[i+8];
  }
  id[24] = 0;

  if (!strncasecmp(id,"IOMEGA  JAZ 1GB",15) || !strncasecmp(id,"IOMEGA  ZIP",11)) 
  return(1);
  else return(0);
}

/* send command to SCSI device */
void
motor( int fd, int mode )
{
  scsi_cmd.inlen = 0;
  scsi_cmd.outlen = 0;
  scsi_cmd.cmd[0] = 0x1b;               /* start/stop */
  scsi_cmd.cmd[1] = 0;
  scsi_cmd.cmd[2] = 0;
  scsi_cmd.cmd[3] = 0;
  scsi_cmd.cmd[4] = mode;
  scsi_cmd.cmd[5] = 0;

  if (ioctl(fd,SCSI_IOCTL_SEND_COMMAND,(void *)&scsi_cmd))
    perror("eject: motor control ioctl error");
}

/* unlock the door before ejecting */
void
unlockdoor( int fd )
{
  scsi_cmd.inlen = 0;
  scsi_cmd.outlen = 0;
  scsi_cmd.cmd[0] = 0x1e;               /* prevent/allow media removal */
  scsi_cmd.cmd[1] = 0;
  scsi_cmd.cmd[2] = 0;
  scsi_cmd.cmd[3] = 0;
  scsi_cmd.cmd[4] = 0;
  scsi_cmd.cmd[5] = 0;

  if (ioctl(fd,SCSI_IOCTL_SEND_COMMAND,(void *)&scsi_cmd))
    perror("eject: door unlock ioctl error");
}

int
main(int argc, char **argv)
{
  int fd;                        /* file descriptor for CD-ROM device */
  int status;                    /* return status for system calls */
  int i;                         /* loop counter */
  char s[255];                   /* for error messages */
  char s1[1024] = "";            /* for reading /etc/mtab */
  char s2[1024] = "";            /* "   "       "         */
  char dummy[256];               /* "   "       "         */
  char *s3;                      /* general purpose */
  FILE *fp;                      /* for reading /etc/mtab */
  char * umountDevStack[20];	 /* eww -- a hard limit */
  int umountDevPtr = 0;

  /* get default device from $CDROM environment variable; if not set
     then use compiled in default */
  s3 = getenv("CDROM");
  if (s3 != NULL)
    strncpy(default_device, s3, sizeof(default_device)-1);
  else
    strncpy(default_device, "/dev/cdrom", sizeof(default_device)-1);
  
  /* make default device the default */
  strncpy(device, default_device, sizeof(default_device)-1);
  
  /* parse the command line arguments */
  parse_args(argc, argv);
  
  /* handle -d option */
  if (d_option) {
    printf("eject: default device is `%s'\n", default_device);
    exit(0);
  }

  /* handle -n option */  
  if (n_option) {
    printf("eject: device nicknames:\n");
    printf("Nickname  Device\n");
    printf("--------  ------\n");
    for (i = 0 ; nickname[i].name != 0 ; i++)
      printf("%-8s  %-8s\n", nickname[i].name, nickname[i].device);
    exit(0);
  }

  if (v_option)
    printf("eject: device name is `%s'\n", device);

  /* check if device is a nickname */
  for (i = 0 ; nickname[i].name != 0 ; i++)
    if (!strcmp(device, nickname[i].name)) {
      if (v_option)
	printf("eject: `%s' is a nickname for `%s'\n",
	       device, nickname[i].device);
      strncpy(device, nickname[i].device, sizeof(device)-1);
      break;
    }

  /* check if device is a symbolic link. If so, find out what it points to */
  status = readlink(device, s1, sizeof(s1));
  if (status != -1) {
    s1[status] = 0;
    if (s1[0] == '/') {
      /* absolute link */
      strncpy(s2, s1, sizeof(s2)-1);
    } else {
      /* a relative link */
      strncpy(s2, device, sizeof(s2)-1);
      s3 = strrchr(s2, '/');
      if (s3 != 0) {
	s3[1] = 0;
	strcat(s2, s1);
      }
    }
    if (v_option)
      printf("eject: `%s' is a link to `%s'\n", device, s2);
    strncpy(device, s2, sizeof(s2)-1);
  }

  /* open device */  
  fd = open(device, O_RDONLY);
  if (fd < 0) {
    sprintf(s, "eject: open failed for `%s'", device);
    perror(s);
    exit(1);
  }

  /* handle auto-eject option */
  if (a_option) {
#ifdef CDROMEJECT_SW
    if (v_option)
      if (a_arg)
	printf("eject: enabling auto-eject mode\n");
      else
	printf("eject: disabling auto-eject mode\n");
    status = ioctl(fd, CDROMEJECT_SW, a_arg);
    if (status != 0) {
      sprintf(s, "eject: CDROMEJECT_SW ioctl failed for `%s'", device);
      perror(s);
      exit(1);
    }
#else
    printf("eject: auto-eject not supported by kernel\n");
#endif
    exit(0);
  }

  /* Handle IDE/ATAPI CDROM changer option. CDROM_SELECT_DISC is
   * preferred, older kernels used CDROMLOADFROMSLOT */
  
  if (c_option) {
#ifdef CDROM_SELECT_DISC
    if (v_option)
      printf("eject: switch to CDROM slot #%i\n", c_arg);
    status = ioctl(fd, CDROM_SELECT_DISC, c_arg);
    if (status != 0) {
      sprintf(s, "eject: CDROM_SELECT_DISC ioctl failed for `%s' (slot %i)", 
        device, c_arg);
      perror(s);
      exit(1);
    }
#elif defined CDROMLOADFROMSLOT
    if (v_option)
      printf("eject: switch to CDROM slot #%i\n", c_arg);
    status = ioctl(fd, CDROMLOADFROMSLOT, c_arg);
    if (status != 0) {
      sprintf(s, "eject: CDROMLOADFROMSLOT ioctl failed for `%s' (slot %i)", 
        device, c_arg);
      perror(s);
      exit(1);
    }
#else
    printf("eject: IDE/ATAPI CD-ROM changer not supported by this kernel\n");
#endif
    exit(0);
  }
  
  /* see if device has been mounted by looking in /etc/mtab */
  #ifdef __linux__
    fp = fopen("/proc/mounts", "r");
  #else
    fp = fopen("/etc/mtab", "r");
  #endif
  while (fscanf(fp, "%s %s %s %s %s %s", s1, s2, dummy, dummy, dummy, dummy) >= 4) {
    if (!strncmp(s1, device,8)) {
      if (!f_option && !u_option) {
	printf("eject: `%s' is mounted at `%s', not ejected\n", s1, s2);
	exit(1);
      } else {
	if (v_option)
	  printf("eject: `%s' is mounted at `%s'\n", s1, s2);
	if (u_option) {
	  if (v_option)
	    printf("eject: will unmount `%s' a bit later\n", s1);
	  umountDevStack[umountDevPtr++] = strdup(s1);
	}
     }
   }
  }

  fclose(fp);

  while (umountDevPtr--) {
    if (v_option)
      printf("eject: trying to unmount `%s'\n", umountDevStack[umountDevPtr]);
    if (!fork()) {
	  execl("/bin/umount", "/bin/umount", umountDevStack[umountDevPtr], 
			NULL);
	  fprintf(stderr, "unable to exec /bin/umount of %s: %s",
		  umountDevStack[umountDevPtr], strerror(errno));
	  exit(1);
    }
    wait(&status);

    if (!WIFEXITED(status)) {
      sprintf(s, "eject: unmount of `%s' failed", umountDevStack[umountDevPtr]);
      perror(s);
    }
  }
  
  fstat(fd, &mystat);
  if (major(mystat.st_rdev) == FLOPPY_MAJOR) {
    /* if we're a floppy, use the FDEJECT ioctl */
    status = ioctl(fd, FDEJECT);
    if (status != 0) {
      sprintf(s, "eject: FDEJECT ioctl failed for `%s'", device);
      perror(s);
      exit(1);
    }
  }  else if (is_jaz(fd))  {
        unlockdoor(fd);
        motor(fd,1);
        motor(fd,2);
  }  else {
    /* if we're not a floppy or a jaz, we must be a CD */
    status = ioctl(fd, CDROMEJECT);
    if (status != 0) {
      sprintf(s, "eject: CDROMEJECT ioctl failed for `%s'", device);
      perror(s);
      exit(1);
    }
  }

  /* close device */
  status = close(fd);
  if (status != 0) {
    sprintf(s, "eject: close failed for `%s'", device);
    perror(s);
    exit(1);
  }
  
  exit(0);
}
