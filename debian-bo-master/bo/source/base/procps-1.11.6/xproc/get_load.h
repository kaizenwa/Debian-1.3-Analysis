/***********************************************************************
 *
 * function prototypes for get_load.c
 *
 * Author:  Hans-Helmut B"uhmann 20. Jan. 1996
 *
 ***********************************************************************/
#ifndef __get_load_h
#define __get_load_h

void InitLoadPoint();

void GetLoadPoint(      	/* Linux version */
     Widget	w,		/* unused */
     caddr_t	closure,	/* unused */
     caddr_t	call_data	/* pointer to (double) return value */
);
#endif /* __get_load_h */
