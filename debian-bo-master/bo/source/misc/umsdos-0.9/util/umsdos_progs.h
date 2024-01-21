#ifndef UMSDOS_PROGS_H
#define UMSDOS_PROGS_H

#define UPROG_VERSION	0
#define UPROG_RELEASE	9
#define PROG_ID(n)	fprintf(stderr,n " %d.%d "\
			"(Umsdos compatibility level %d.%d)\n"\
			,UPROG_VERSION,UPROG_RELEASE\
			,UMSDOS_VERSION,UMSDOS_RELEASE);

class UMSDOS_IOCTL{
	int fd;
	int verbose;
	/*~PROTOBEG~ UMSDOS_IOCTL */
public:
	UMSDOS_IOCTL (const char *dpath,
		 int _verbose,
		 int quit);
	int create (const char *fname,
		 mode_t mode,
		 time_t atime,
		 time_t ctime,
		 time_t mtime,
		 int uid,
		 int gid,
		 int rdev);
	int dosrename (const char *src, const char *dst);
	int dosrmdir (const char *fname);
	int dossetup (int uid, int gid, mode_t mode);
	int dosstat (const char *fname,
		 struct stat&stat);
	int dosunlink (const char *fname);
private:
	void fname_copy (struct umsdos_ioctl&data,
		 const char *fname);
public:
	int getversion (int &version, int &release);
	int initemd (void);
private:
	int ioctl (int cmd, struct umsdos_ioctl *data);
public:
	int isok (void);
	int ispromoted (void);
	int readdir (struct dirent&dirent);
	void rewind (void);
	int ureaddir (struct umsdos_dirent&udirent,
		 struct dirent&dirent);
	int urmdir (const char *fname);
	int uunlink (const char *fname);
	~UMSDOS_IOCTL (void);
	/*~PROTOEND~ UMSDOS_IOCTL */
};

#include "umsdos_progs.p"

#endif

