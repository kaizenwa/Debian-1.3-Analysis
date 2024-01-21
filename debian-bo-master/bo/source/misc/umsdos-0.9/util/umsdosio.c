#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <errno.h>
#include <fcntl.h>
//#include <limits.h>
#include <unistd.h>
#include <sys/stat.h>
#include <linux/umsdos_fs.h>
#include "umsdos_progs.h"

/*
	Open a directory. Don't return if error.
*/
PUBLIC UMSDOS_IOCTL::UMSDOS_IOCTL (
	const char *dpath,	/* Directory path */
	int _verbose,		/* Report error with perror() ? */
	int quit)			/* exit(-1) if directory can't be open */
						/* if quit == 0, isok() should be used */
{
	fd = -1;
	char buf[300];
	sprintf (buf,"Can't open directory %s",dpath);
	struct stat dir_stat;
	if (stat(dpath,&dir_stat)==-1){
		if (_verbose) perror (buf);
	}else if (!S_ISDIR(dir_stat.st_mode)){
		if (_verbose) fprintf (stderr,"%s is not a directory\n",dpath);
	}else{
		fd = open (dpath,O_RDONLY);
		if (fd == -1){
			if (_verbose){
				perror (buf);
			}
			if (quit) exit (-1);
		}else{
			int version,release;
			if (getversion (version,release) != 0){
				fprintf (stderr,"This utility must operate on a directory\n"
					"of a umsdos mounted file system. Even an msdos\n"
					"file system won't do. If this is the case\n"
					"umount the file system and remount it as a umsdos one.\n");
				exit (-1);
			}else if (version != UMSDOS_VERSION || release != UMSDOS_RELEASE){
				fprintf (stderr,"This utility is incompatible with the\n"
					"current release of the UMSDOS file system.\n"
					"This utility was issued for version %d.%d\n"
					"and the UMSDOS drivers currently running is %d.%d.\n"
					"Can't continue\n"
					,UMSDOS_VERSION,UMSDOS_RELEASE,version,release);
				exit (-1);
			}
		}
	}
	verbose = _verbose;
}

PUBLIC UMSDOS_IOCTL::~UMSDOS_IOCTL()
{
	if (fd != -1) close (fd);
}

/*
	Return != 0 if the directory was successfully opened
*/
PUBLIC int UMSDOS_IOCTL::isok()
{
	return fd != -1;
}
/*
	Perform ioctl and (conditionnally) reports error.
*/
PRIVATE int UMSDOS_IOCTL::ioctl (
	int cmd,
	struct umsdos_ioctl *data)
{
	int ret = ::ioctl (fd,cmd,data);
	if (ret < 0 && verbose){
		// This table must be kept in sync with linux/umsdos_fs.h
		static char *tb[]={
			"UMSDOS_READDIR_DOS",
			"UMSDOS_UNLINK_DOS",
			"UMSDOS_RMDIR_DOS",
			"UMSDOS_STAT_DOS",
			"UMSDOS_CREAT_EMD",
			"UMSDOS_UNLINK_EMD",
			"UMSDOS_READDIR_EMD",
			"UMSDOS_GETVERSION",
			"UMSDOS_INITEMD",
			"UMSDOS_DOS_SETUP",
			"UMSDOS_RENAME_DOS",
		};
		char buf[100];
		sprintf (buf,"umsdos_ioctl: %s ",tb[cmd-UMSDOS_READDIR_DOS]);
		perror (buf);
	}
	return ret;
}

/*
	Rewind to beginning of dir (for readdir()).
*/
PUBLIC void UMSDOS_IOCTL::rewind()
{
	if (lseek (fd,0,SEEK_SET)==-1) perror ("UMSDOS_IOCTL::rewind");
}
	
/*
	Gets the version and release number of the umsdos fs driver.
	Return -1 if any error.
*/
PUBLIC int UMSDOS_IOCTL::getversion(int &version, int &release)
{
	int ret = -1;
	struct umsdos_ioctl data;
	version = release = -1;
	if (ioctl(UMSDOS_GETVERSION,&data)==0){
		version = data.version;
		release = data.release;
		ret = 0;
	}
	return ret;
}
/*
	Make sure the EMD file exist in a directory. This automaticly
	promote the directory to UMSDOS semantic (long name, links, etc).
	Return -1 if any error.
*/
PUBLIC int UMSDOS_IOCTL::initemd()
{
	int ret = -1;
	struct umsdos_ioctl data;
	if (ioctl(UMSDOS_INIT_EMD,&data)==0){
		ret = 0;
	}
	return ret;
}

/*
	Read an entry in the DOS directory.
	Return -1 if error.
*/
PUBLIC int UMSDOS_IOCTL::readdir (struct dirent &dirent)
{
	int ret = -1;
	struct umsdos_ioctl data;
	if (ioctl(UMSDOS_READDIR_DOS,&data)>0){
		dirent = data.dos_dirent;
		ret = 0;
	}
	return ret;
}
/*
	Read an entry in the EMD directory.
	Return -1 if error.
*/
PUBLIC int UMSDOS_IOCTL::ureaddir (
	struct umsdos_dirent &udirent,
	struct dirent &dirent)
{
	int ret = -1;
	struct umsdos_ioctl data;
	if (ioctl(UMSDOS_READDIR_EMD,&data)>0){
		udirent = data.umsdos_dirent;
		dirent = data.dos_dirent;
		ret = 0;
	}
	return ret;
}

/*
	Initialise the name fields in struct umsdos_ioctl.
	Does both name and d_name initialisation even if unnecessary.
*/
PRIVATE void UMSDOS_IOCTL::fname_copy (
	struct umsdos_ioctl &data,
	const char *fname)
{
	strcpy (data.dos_dirent.d_name,fname);
	data.dos_dirent.d_reclen = strlen(fname);
	strcpy (data.umsdos_dirent.name,fname);
	data.umsdos_dirent.name_len = data.dos_dirent.d_reclen;
}
/*
	Create a file or a directory in the EMD file (or whatever in fact).
	This function simply create an entry with the supplied parameter.
	It is expected that the caller know what he is doing.
	Return -1 if error.
*/
PUBLIC int UMSDOS_IOCTL::create (
	const char *fname,
	mode_t mode,
	time_t atime,
	time_t ctime,
	time_t mtime,
	int uid,
	int gid,
	int rdev)
{ 
	struct umsdos_ioctl data;
	fname_copy (data,fname);
	data.umsdos_dirent.mode = mode;
	data.umsdos_dirent.nlink = 1;
	data.umsdos_dirent.ctime = ctime;
	data.umsdos_dirent.atime = atime;
	data.umsdos_dirent.mtime = mtime;
	data.umsdos_dirent.flags = 0;
	data.umsdos_dirent.gid = gid;
	data.umsdos_dirent.uid = uid;
	data.umsdos_dirent.rdev = rdev;
	return ioctl (UMSDOS_CREAT_EMD,&data);
}
/*
	Remove a file from the DOS directory.
	Return -1 if error.
*/
PUBLIC int UMSDOS_IOCTL::dosunlink(const char *fname)
{
	struct umsdos_ioctl data;
	fname_copy (data,fname);
	return ioctl (UMSDOS_UNLINK_DOS,&data);
}
/*
	Remove a directory from the DOS directory.
	Return -1 if error.
*/
PUBLIC int UMSDOS_IOCTL::dosrmdir(const char *fname)
{
	struct umsdos_ioctl data;
	fname_copy (data,fname);
	return ioctl (UMSDOS_RMDIR_DOS,&data);
}
/*
	Rename a file or directory in the DOS directory.
	Return -1 if error.
*/
PUBLIC int UMSDOS_IOCTL::dosrename(
	const char *src,
	const char *dst)
{
	struct umsdos_ioctl data;
	fname_copy (data,src);
	strcpy (data.umsdos_dirent.name,dst);
	data.umsdos_dirent.name_len = strlen(dst);
	return ioctl (UMSDOS_RENAME_DOS,&data);
}
/*
	Remove a directory from the EMD file.
	Return -1 if error.
*/
PUBLIC int UMSDOS_IOCTL::urmdir(const char *fname)
{
	struct umsdos_ioctl data;
	fname_copy (data,fname);
	data.umsdos_dirent.mode = S_IFDIR|0777;
	return ioctl (UMSDOS_UNLINK_EMD,&data);
}
/*
	Remove a file from the EMD file.
	Return -1 if error.
*/
PUBLIC int UMSDOS_IOCTL::uunlink(const char *fname)
{
	struct umsdos_ioctl data;
	fname_copy (data,fname);
	data.umsdos_dirent.mode = S_IFREG|0777;
	return ioctl (UMSDOS_UNLINK_EMD,&data);
}

/*
	Do a stat of the file using DOS directory info only
	Return -1 if error.	 
*/
PUBLIC int UMSDOS_IOCTL::dosstat (const char *fname, struct stat &fstat)
{
	struct umsdos_ioctl data;
	fname_copy (data,fname);
	data.umsdos_dirent.mode = S_IFREG|0777;
	int ret = ioctl (UMSDOS_STAT_DOS,&data);
	fstat.st_ino  = data.stat.st_ino;
	fstat.st_mode = data.stat.st_mode;
	fstat.st_size = data.stat.st_size;
	fstat.st_atime = data.stat.st_atime;
	fstat.st_ctime = data.stat.st_ctime;
	fstat.st_mtime = data.stat.st_mtime;
	return ret;
}

/*
	Set the default permissions and owner for DOS directories.
	Return -1 if error.	 
*/
PUBLIC int UMSDOS_IOCTL::dossetup (
	int uid,
	int gid,
	mode_t mode)
{
	struct umsdos_ioctl data;
	data.umsdos_dirent.uid  = uid;
	data.umsdos_dirent.gid  = gid;
	data.umsdos_dirent.mode = mode;
	return ioctl (UMSDOS_DOS_SETUP,&data);
}

/*
	Return != 0 if the directory is promoted (Has a --linux-.--- file)
	A promoted directory behave like a Unix directory. A non promoted
	behave like DOS.
*/
PUBLIC int UMSDOS_IOCTL::ispromoted()
{
	struct stat fstat;
	int cur_verbose = verbose;
	verbose = 0;
	int ret = dosstat(UMSDOS_EMD_FILE,fstat) != -1;
	verbose = cur_verbose;
	return ret;
}

