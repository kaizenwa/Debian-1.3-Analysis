/* Copyright 1989 GROUPE BULL -- See license conditions in file COPYRIGHT
 * Copyright 1989 Massachusetts Institute of Technology
 */
/*******************************\
* 			        *
*  Machine - dependent patches  *
* 			        *
\*******************************/

#if defined SYSV || defined SVR4

/* getpagesize just returns "pagesize" which is 8192 on most machines? */

int 
getpagesize()
{
#ifdef PAGE_SIZE
    return PAGE_SIZE;
#else /* PAGE_SIZE */
    return 8192;
#endif /* PAGE_SIZE */
}

#endif /* SYSV || SVR4 */
