/* Various utilities - NT versions
   Copyright (C) 1994, 1995, 1996 the Free Software Foundation.

   Written 1994, 1995, 1996 by:
   Juan Grigera, Miguel de Icaza, Janne Kukonlehto, Dugan Porter,
   Jakub Jelinek, Mauricio Plaza.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <windows.h>
#include <io.h>
#include <fcntl.h>
#include <signal.h>		/* my_system */
#include <limits.h>		/* INT_MAX */
#include <sys/time.h>		/* select: timeout */
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdarg.h>
#include <process.h>
#include <fs.h>
#include <util.h>

char *get_owner (int uid)
{
    return "none";
}

char *get_group (int gid)
{
    return "none";
}

/* Pipes are guaranteed to be able to hold at least 4096 bytes */
/* More than that would be unportable */
#define MAX_PIPE_SIZE 4096

static int error_pipe[2];	/* File descriptors of error pipe */
static int old_error;		/* File descriptor of old standard error */

/* Creates a pipe to hold standard error for a later analysis. */
/* The pipe can hold 4096 bytes. Make sure no more is written */
/* or a deadlock might occur. */
void open_error_pipe (void)
{
    if (pipe (error_pipe) < 0){
	message (0, " Warning ", " Pipe failed ");
    }
    old_error = dup (2);
    if(old_error < 0 || close(2) || dup (error_pipe[1]) != 2){
	message (0, " Warning ", " Dup failed ");
	close (error_pipe[0]);
	close (error_pipe[1]);
    }
    close (error_pipe[1]);
}

void close_error_pipe (int error, char *text)
{
    char *title;
    char msg[MAX_PIPE_SIZE];
    int len = 0;

    if (error)
	title = " Error ";
    else
	title = " Warning ";
    if (old_error >= 0){
	close (2);
	dup (old_error);
	close (old_error);
	len = read (error_pipe[0], msg, MAX_PIPE_SIZE);

	if (len >= 0)
	    msg[len] = 0;
	close (error_pipe[0]);
    }
    if (error < 0)
	return;		/* Just ignore error message */
    if (text == NULL){
	if (len == 0) return;	/* Nothing to show */

	/* Show message from pipe */
	message (error, title, msg);
    } else {
	/* Show given text and possible message from pipe */
	message (error, title, " %s \n %s ", text, msg);
    }
}

void check_error_pipe (void)
{
    char error[MAX_PIPE_SIZE];
    int len = 0;
    if (old_error >= 0){
	while (len < MAX_PIPE_SIZE)
	{
	    int rvalue;

	    rvalue = read (error_pipe[0], error + len, 1);
	    len ++;
	    if (rvalue <= 0)
		break;
	}
	error[len] = 0;
	close (error_pipe[0]);
    }
    if (len > 0)
        message (0, " Warning ", error);
}

int my_system (int as_shell_command, const char *shell, const char *command)
{
    int status = 0;

    if (as_shell_command)
       spawnlp (_P_WAIT, shell, shell, "/c", command, (char *) 0);
    else
       spawnl (_P_WAIT, shell, shell, command, (char *) 0);

    return status;
}

char *tilde_expand (char *directory)
{
    return strdup (directory);
}

/* Canonicalize path, and return a new path. Do everything in situ.
 * [call OS API]
 */
char *canonicalize_pathname (char *path)
{
/* This holds an unused pointer to the start of file name in path */
    char *pName; 
    char pCanonical[MC_MAXPATHLEN];

    GetFullPathName (path, MC_MAXPATHLEN, pCanonical, &pName);

    /* FIXME: buffer large enough? */
    strcpy (path, pCanonical);
}


void my_statfs (struct my_statfs *myfs_stats, char *path)
{
    int i, len = 0;
    DWORD lpSectorsPerCluster, lpBytesPerSector, lpFreeClusters, lpClusters;
       DWORD           lpMaximumComponentLength, dw, lpFileSystemFlags;
       static char     lpVolumeNameBuffer[256], lpFileSystemNameBuffer[30];

       GetDiskFreeSpace(NULL, &lpSectorsPerCluster, &lpBytesPerSector,
			&lpFreeClusters, &lpClusters);

       /* KBytes available */
       myfs_stats->avail = lpSectorsPerCluster * lpBytesPerSector * lpFreeClusters / 1024;
       
       /* KBytes total */
       myfs_stats->total = lpSectorsPerCluster * lpBytesPerSector * lpClusters / 1024; 
       myfs_stats->nfree = lpFreeClusters;
       myfs_stats->nodes = lpClusters;

       GetVolumeInformation(NULL, lpVolumeNameBuffer, 255, NULL,
			    &lpMaximumComponentLength, &lpFileSystemFlags,
			    lpFileSystemNameBuffer, 30);

       myfs_stats->mpoint = lpFileSystemNameBuffer;
       myfs_stats->device = lpVolumeNameBuffer;


       myfs_stats->type = GetDriveType(NULL);
       switch (myfs_stats->type) {
	   /*
	    * mmm. DeviceIoControl may fail if you are not root case
	    * F5_1Pt2_512,            5.25", 1.2MB,  512 bytes/sector
	    * myfs_stats->typename = "5.25\" 1.2MB"; break; case
	    * F3_1Pt44_512,           3.5",  1.44MB, 512 bytes/sector
	    * myfs_stats->typename = "3.5\" 1.44MB"; break; case
	    * F3_2Pt88_512,           3.5",  2.88MB, 512 bytes/sector
	    * myfs_stats->typename = "3.5\" 2.88MB"; break; case
	    * F3_20Pt8_512,           3.5",  20.8MB, 512 bytes/sector
	    * myfs_stats->typename = "3.5\" 20.8MB"; break; case
	    * F3_720_512,             3.5",  720KB,  512 bytes/sector
	    * myfs_stats->typename = "3.5\" 720MB"; break; case
	    * F5_360_512,             5.25", 360KB,  512 bytes/sector
	    * myfs_stats->typename = "5.25\" 360KB"; break; case
	    * F5_320_512,             5.25", 320KB,  512 bytes/sector
	    * case F5_320_1024,       5.25", 320KB,  1024
	    * bytes/sector myfs_stats->typename = "5.25\" 320KB"; break;
	    * case F5_180_512,        5.25", 180KB,  512
	    * bytes/sector myfs_stats->typename = "5.25\" 180KB"; break;
	    * case F5_160_512,        5.25", 160KB,  512
	    * bytes/sector myfs_stats->typename = "5.25\" 160KB"; break;
	    * case RemovableMedia,    Removable media other than
	    * floppy myfs_stats->typename = "Removable"; break; case
	    * FixedMedia              Fixed hard disk media
	    * myfs_stats->typename = "Hard Disk"; break; case Unknown:
	    * Format is unknown
	    */
       case DRIVE_REMOVABLE:
               myfs_stats->typename = "Removable";
               break;
       case DRIVE_FIXED:
               myfs_stats->typename = "Hard Disk";
               break;
       case DRIVE_REMOTE:
               myfs_stats->typename = "Networked";
               break;
       case DRIVE_CDROM:
               myfs_stats->typename = "CD-ROM";
               break;
       case DRIVE_RAMDISK:
               myfs_stats->typename = "RAM disk";
               break;
       default:
               myfs_stats->typename = "unknown";
               break;
       };
}

int gettimeofday (struct timeval* tvp, void *p)
{
	if (p != NULL)		// what is "p"?
		return 0;	
	
	// Since MC only calls this func from get_random_hint we return 
	// some value, not exactly the "correct" one
	tvp->tv_sec = GetTickCount()/1000; 	// Number of milliseconds since Windows started
	tvp->tv_usec = GetTickCount();
}

// FAKE funcs

/* lstat - Because of symlinks in Unix, stat will give info 
	   on the file pointed to and lstat on the symlink itself.
	   We have no such a difference/trouble.
 */
int lstat (const char* pathname, struct _stat *buffer)
{
	return stat (pathname, buffer);
}

int getuid ()	      
{
/*    SID sid;
    LookupAccountName (NULL, &sid...
    return 0;
*/
    return 0;
}

int getgid ()	      
{
    return 0;
}

int readlink (char* path, char* buf, int size)
{
    return -1;
}
int symlink (char *n1, char *n2)
{
    return -1;
}
int link (char *p1, char *p2)
{
    return -1;
}
int chown (char *path, int owner, int group)
{
    return -1;
}
int mknod (char *path, int mode, int dev)
{
    return -1;
}

void init_uid_gid_cache (void)
{
    return 0;
}

/*
#define __SECURITY_DEFAULT   64
int _stat (const char *pathname, struct _stat *buffer)
{
	SECURITY_DESCRIPTOR  sd;
	BOOL				 flag;
	DWORD			     dw;
	SID					 sid;

 	stat (pathname, buffer)

	sd = malloc (__SECURITY_DEFAULT);
	GetFileSecurity (pathname, OWNER_SECURITY_INFORMATION, &sd, __SECURITY_DEFAULT, &dw);

	// Buffer too small, try again	
	if (dw) {
		free (sd);
		sd = malloc (dw);
		GetFileSecurity (pathname, OWNER_SECURITY_INFORMATION, &sd, __SECURITY_DEFAULT, &dw);
	}

	GetSecurityDescriptorGroup (&sd, &sid, &flag);
	unsigned short st_mode;
	short st_uid;
	short st_gid;
}
*/ 