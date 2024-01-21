#ifndef lint
static char *RCSid = "$Header: /src/common/usc/lib/libgen/RCS/strerror.c,v 1.1 1992/03/21 02:48:11 mcooper Exp mcooper $";
#endif

/*
 * $Log: strerror.c,v $
 * Revision 1.1  1992/03/21  02:48:11  mcooper
 * Initial revision
 *
 */

#include <stdio.h>
#include <sys/errno.h>

/*
 * Return string for system error number "Num".
 */
char *strerror(Num)
     int			Num;
{
    extern int 			sys_nerr;
    extern char 	       *sys_errlist[];
    static char 		Unknown[100];

    if (Num < 0 || Num > sys_nerr) {
	(void) sprintf(Unknown, "Error %d", Num);
	return(Unknown);
    } else
	return(sys_errlist[Num]);
}
