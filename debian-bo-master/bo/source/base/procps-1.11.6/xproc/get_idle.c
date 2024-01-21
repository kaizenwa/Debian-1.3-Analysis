/*
 * get_idle.c
 * get idle, from get_load.c derived
 *
 * Author:  Hans-Helmut Buehmann 20. Jan. 1996
 * 
 */

#include <X11/Xos.h>
#include <X11/Intrinsic.h>
#include <stdio.h>
#include <unistd.h>
#include "get_idle.h"

#ifdef linux
#include "../proc/sysinfo.h"	/* possibly <proc/sysinfo.h> instead */

void InitIdlePoint()
{
}

/* ARGSUSED */
void GetIdlePoint( w, closure, call_data )	/* Linux version */
     Widget	w;		/* unused */
     caddr_t	closure;	/* unused */
     caddr_t	call_data;	/* pointer to (double) return value */
{
	/* tracked ratio of idle time to uptime */
	static double last_idl = 0, last_tim = 0;
	double idl, tim, idl_diff, tim_diff;
	if (!uptime(&tim, &idl)) {
	    perror("xidle : trouble with /proc/uptime ");
	    exit(1);
	}
	idl_diff = idl - last_idl;
	tim_diff = tim - last_tim;
	last_idl = idl;
	last_tim = tim;
	*(double*)call_data = 0.95*((tim_diff>0) ? (1 - idl_diff/tim_diff) : 1);
	
}

#endif /* linux */
