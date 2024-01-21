/*
 * fsync(2) emulation for systems lacking it
 *
 * Also strongly recommended if you are not doing processing over NFS,
 * as fsync() is a serious efficiency hit on many systems.  When NFS
 * enters the picture, though, it's probably necessary.
 */

/* ARGSUSED */
int
fsync(fd)
int fd;
{
	return 0;
}
