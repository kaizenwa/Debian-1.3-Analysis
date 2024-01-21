/* main.c 13/01/95 01.20.46 */
/* numconst.c 20/01/94 01.31.18 */
/* printk.c 12/05/94 07.13.08 */
/* udosctl.c 13/01/95 01.11.46 */
/* udump.c 28/08/93 00.25.02 */
/* umsdosio.c 19/01/95 23.30.52 */
PUBLIC UMSDOS_IOCTL::UMSDOS_IOCTL (const char *dpath,
	 int _verbose,
	 int quit);
PUBLIC UMSDOS_IOCTL::~UMSDOS_IOCTL (void);
PUBLIC int UMSDOS_IOCTL::isok (void);
PRIVATE int UMSDOS_IOCTL::ioctl (int cmd, struct umsdos_ioctl *data);
PUBLIC void UMSDOS_IOCTL::rewind (void);
PUBLIC int UMSDOS_IOCTL::getversion (int &version, int &release);
PUBLIC int UMSDOS_IOCTL::initemd (void);
PUBLIC int UMSDOS_IOCTL::readdir (struct dirent&dirent);
PUBLIC int UMSDOS_IOCTL::ureaddir (struct umsdos_dirent&udirent,
	 struct dirent&dirent);
PRIVATE void UMSDOS_IOCTL::fname_copy (struct umsdos_ioctl&data,
	 const char *fname);
PUBLIC int UMSDOS_IOCTL::create (const char *fname,
	 mode_t mode,
	 time_t atime,
	 time_t ctime,
	 time_t mtime,
	 int uid,
	 int gid,
	 int rdev);
PUBLIC int UMSDOS_IOCTL::dosunlink (const char *fname);
PUBLIC int UMSDOS_IOCTL::dosrmdir (const char *fname);
PUBLIC int UMSDOS_IOCTL::dosrename (const char *src, const char *dst);
PUBLIC int UMSDOS_IOCTL::urmdir (const char *fname);
PUBLIC int UMSDOS_IOCTL::uunlink (const char *fname);
PUBLIC int UMSDOS_IOCTL::dosstat (const char *fname,
	 struct stat&stat);
PUBLIC int UMSDOS_IOCTL::dossetup (int uid, int gid, mode_t mode);
PUBLIC int UMSDOS_IOCTL::ispromoted (void);
/* umssetup.c 13/01/95 01.11.32 */
/* umssync.c 24/01/95 23.05.22 */
