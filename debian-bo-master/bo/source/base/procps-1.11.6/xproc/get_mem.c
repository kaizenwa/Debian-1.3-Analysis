/*
 * get_mem.c
 * get memory usage, from get_load.c derived
 *
 * Author:  Hans-Helmut Buehmann 20. Jan. 1996
 * 
 * Modified for more recent kernels Helmut Geyer Oct. 1996
 */

#include <X11/Xos.h>
#include <X11/Intrinsic.h>
#include <stdio.h>
#include <unistd.h>
#include "MemStripChart.h"

#ifdef linux
#include "../proc/sysinfo.h"	/* possibly <proc/sysinfo.h> instead */

void InitMemLoadPoint()
{
}


void GetMemLoadPoint( w, closure, call_data )	/* Linux version */
     Widget	w;		/* unused */
     caddr_t	closure;	/* unused */
     caddr_t	call_data;	/* pointer to (MemStripChartCallbackData) return value */
{
	MemStripChartCallbackData ret;
	unsigned** mem = meminfo();
	if (!mem) {
	    perror("xmem : trouble with /proc/meminfo ");
	    exit(1);
	} 
        /* code + cached + buffer + free == 1.0 */
	/* the old formula using shared doesn't work any longer */
	ret.code
	    = (double)(  mem[meminfo_main][meminfo_used] 
		       - mem[meminfo_main][meminfo_buffers]
		       - mem[meminfo_main][meminfo_cached])
	    / (double)mem[meminfo_main][meminfo_total];
	ret.cached 
	    = (double)mem[meminfo_main][meminfo_cached] 
	    / (double)mem[meminfo_main][meminfo_total];
	ret.buffer 
	    = (double)mem[meminfo_main][meminfo_buffers] 
	    / (double)mem[meminfo_main][meminfo_total];
	ret.free
	    = (double)mem[meminfo_main][meminfo_free] 
	    / (double)mem[meminfo_main][meminfo_total];
	ret.swap
	    = (double)mem[meminfo_swap][meminfo_used] 
	    / (double)mem[meminfo_main][meminfo_total];
	memcpy(call_data, &ret, sizeof(MemStripChartCallbackData));
}

#endif /* linux */
