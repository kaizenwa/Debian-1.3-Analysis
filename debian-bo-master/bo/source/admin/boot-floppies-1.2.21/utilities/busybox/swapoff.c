#include "internal.h"

const char	swapoff_usage[] = "swapoff block-device\n"
"\n"
"\tStop swapping virtual memory pages on the given device.\n";

extern int
swapoff_fn(const struct FileInfo * i)
{
	return swapoff(i->source);
}
