/* @(#) rtlib.h,v 1.5 1992/07/11 11:50:44 tron Exp */

/*
 * rtlib.h - interface file for routines in rtlib.c
 */

/* flag values passed from rt[dv]_standard to the driver routines */
#define RT_VERIFY	0x0001		/* Verify only */

/* external functions in the rtlib.c file */
void rtd_standard();
void rtv_standard();
