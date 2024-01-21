/*
 * CFINGERD
 * Global defines
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 1, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 */

/* Error handler...  Or what seems to be.  :) */
#define	CF_ERROR(x)	{ \
			    printf("\n"); \
			    printf(errors[x].error); \
			    printf("\n"); \
			    fflush(stdout); \
			    exit(PROGRAM_SYSLOG); \
			}

/* Some defines to make adding in carriage returns easier... */
#define	SEND_RAW_RETURN			{ printf("\n"); fflush(stdout); }
#define	SEND_RAW_RETURN_NO_FLUSH	printf("\n");
