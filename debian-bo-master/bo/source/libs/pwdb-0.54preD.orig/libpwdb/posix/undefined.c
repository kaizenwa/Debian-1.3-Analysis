/*
 * This is an abort function. It is used to help stop an application
 * making mistakes when it tries to put all of its posix calls through
 * libpwdb... Eventually, it should not be needed.
 */

#include <stdio.h>
#include <pwdb/pwdb_public.h>

void _posix_undefined(void)
{
    fprintf(stderr,
	    "Sorry, you have called a POSIX function.\n"
	    "However, it has not been implemented within libpwdb yet!\n");
    exit(1);
}
