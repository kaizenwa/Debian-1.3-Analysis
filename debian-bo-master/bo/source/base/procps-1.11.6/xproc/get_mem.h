/***********************************************************************
 *
 * function prototypes for get_mem.c
 *
 * Author:  Hans-Helmut B"uhmann 20. Jan. 1996
 *
 ***********************************************************************/
#ifndef __get_mem_h
#define __get_mem_h

#include "MemStripChart.h"

void InitMemLoadPoint();

void GetMemLoadPoint(   	/* Linux version */
     Widget	w,		/* unused */
     caddr_t	closure,	/* unused */
     caddr_t	call_data	/* pointer to (MemStripChartCallbackData) return value */
);
#endif /* __get_mem_h */
