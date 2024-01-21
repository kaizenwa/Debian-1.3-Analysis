#include "internal.h"
#include <sys/ioctl.h>
#include <linux/fd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

const char			fdflush_usage[] = "fdflush device";

int
fdflush_fn(const struct FileInfo * i)
{
	int	status;
	int	fd = open(i->source, 0);

	if ( fd < 0 ) {
		name_and_error(i->source);
		return 1;
	}

	status = ioctl(fd, FDFLUSH, 0);
	close(fd);

	if ( status != 0 ) {
		name_and_error(i->source);
		return 1;
	}
	return 0;
}
