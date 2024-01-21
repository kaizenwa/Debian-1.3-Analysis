/*
   Time-stamp: <96/07/19 20:18:46 yusuf>

   $Id: taper.h,v 1.9 1996/07/27 20:42:17 yusuf Exp $
*/


/* Functions in tapeio.h */

#define __FROM_MAIN__


#include "version.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <utime.h>
#include <time.h>
#include <signal.h>
#include <sys/stat.h>
#include <sys/mtio.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/sem.h>
#include <sys/mount.h>
#include <sys/wait.h>
#include <curses.h>
#include <form.h>

#include "select_box.h"
#include "compress/lzrw3.h"
#include "compress/gzip.h"
#include "non-ansi.h"
#include "defaults.h"
#include "structs.h"
#include "vars.h"
#include "common.h"
#include "tapeio.h"
#include "endianize.h"
