#include "internal.h"

const char	swapon_usage[] = "swapon block-device\n"
"\n"
"\tSwap virtual memory pages on the given device.\n";

extern int
swapon_fn(const struct FileInfo * i)
{
#ifdef sparc
        return swapon(i->source);
#else
	return swapon(i->source, 0);
#endif
}

