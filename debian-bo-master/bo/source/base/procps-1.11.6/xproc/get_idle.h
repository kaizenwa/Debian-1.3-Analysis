/***********************************************************************
 *
 * function prototypes for get_idle.c
 *
 * Author:  Hans-Helmut B"uhmann 20. Jan. 1996
 *
 ***********************************************************************/
#ifndef __get_idle_h
#define __get_idle_h

void InitIdlePoint();

void GetIdlePoint(        	/* Linux version */
     Widget	w,		/* unused */
     caddr_t	closure,	/* unused */
     caddr_t	call_data);	/* pointer to (double) return value */

#endif /* __get_idle_h */
