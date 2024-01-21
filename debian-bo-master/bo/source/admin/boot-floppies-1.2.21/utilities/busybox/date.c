#include "internal.h"
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <stdio.h>

const char			date_usage[] = "date";

int
date_main(struct FileInfo * i, int argc, char * * argv)
{
	time_t		t;

	time(&t);
	printf("%s\n", ctime(&t));
	return 0;
}
