#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <getopt.h>
#include <string.h>
#include <fcntl.h>
#include <dirent.h>
#include <pwd.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/vt.h>
#include <sys/types.h>
#include <sys/wait.h>



void usage(int);

/*
 * There must be a universal way to find these!
 */

#define TRUE (1)
#define FALSE (0)


/*
 * Where your VTs are hidden
 */
#ifdef __linux__
#define VTNAME "/dev/tty%d"
#endif

#ifdef ESIX_5_3_2_D
#define	VTBASE		"/dev/vt%02d"
#endif
