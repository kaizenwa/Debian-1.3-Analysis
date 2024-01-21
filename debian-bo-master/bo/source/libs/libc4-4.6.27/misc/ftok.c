#include <sys/stat.h>
#include <sys/ipc.h>

key_t
ftok (char *path, char id)
{
    struct stat buf;
    key_t key;

    if (__stat (path, &buf)) {
	return (key_t) -1;
    }
#if 0
    key = (buf.st_ino & 0xFFFF) | ((buf.st_uid & 0xFF) << 16) | (id << 24);
#else
    key = (buf.st_ino & 0xFFFF) | ((buf.st_dev & 0xFF) << 16) | (id << 24);
#endif
    return key;
}
