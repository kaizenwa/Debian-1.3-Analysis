#ifdef __FreeBSD__
#include <unistd.h>
#endif

main()
{
#ifdef __FreeBSD__
	return lseek(STDIN_FILENO, (off_t)0, SEEK_SET) < 0;
#else
	return lseek(0,0L,0) < 0;
#endif
}
