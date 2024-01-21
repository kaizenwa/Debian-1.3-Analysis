/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

static char RCSid[] = "$Id: ftwx.c,v 1.6 1995/09/10 18:32:56 chuck Exp $" ;

#include <sys/param.h>
#ifdef linux
#include <unistd.h>
#endif	/* linux */
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#ifndef OLD_DIR
#include <dirent.h>
#else
#include <sys/dir.h>
#define dirent direct
#endif

extern int errno ;

#include "misc.h"
#include "ftwx.h"

#define PRIVATE			static

#define NUL					'\0'

#if defined(linux) || defined(BSD)
PRIVATE int ftwx_traverse( char *path , int depth ) ;
#endif

typedef enum { NO, YES } boolean_e ;

static struct
{
	int (*stat_func)() ;
	int (*user_func)() ;
} ftwx_data ;

/*
 * ftwx is an extension to ftw, that optionally follows symlinks (the
 * default is NOT to follow them).
 *
 * Possible flag values:
 *		FTWX_FOLLOW: 		follow symlinks
 *
 * Possible depth values:
 *		0			: means only the specified path
 *		positive : means go as deep as specified
 *		FTWX_ALL : no depth limitation
 *
 * User function return value:
 *		negative : means an error occured and the traversal should stop
 *		0			: OK
 *		positive : means that if the current object is a directory it
 *					  should not be traversed.
 *
 * Return value:
 *		-1 		: if an error occurs
 *  frv			: frv is the function return value if it is negative (it
 *					  should not be -1).
 *		0			: if successful
 */
int ftwx( path, func, depth, flags )
	char *path ;
	int (*func)() ;
	int depth ;
	int flags ;
{
	int stat(), lstat() ;

	/*
	 * Initialize the data structure
	 */
	ftwx_data.stat_func = ( flags & FTWX_FOLLOW ) ? stat : lstat ;
	ftwx_data.user_func = func ;

	return( ftwx_traverse( path, depth ) ) ;
}




/*
 * ftwx_traverse works in two phases:
 *
 * Phase 1: process the current path
 *
 * Phase 2: if the current path is a directory, it invokes ftwx_traverse
 *				for each directory entry
 */
PRIVATE int ftwx_traverse( path, depth )
	char *path ;
	int depth ;
{
	DIR *dirp ;
	struct stat st ;
	int ftw_flag = 0 ;
	boolean_e traverse = YES ;
	int retval ;
	int save_errno ;

	if ( (*ftwx_data.stat_func)( path, &st ) == -1 )
		ftw_flag = FTW_NS ;
	else
	{
		/*
		 * If it is a directory and determine if it is readable
		 * (if it is not readable, we don't traverse it
		 */
		if ( S_ISDIR( st.st_mode ) )
			if ( access( path, R_OK ) == 0 )
				ftw_flag = FTW_D ;
			else
				ftw_flag = FTW_DNR ;
		else
			ftw_flag = FTW_F ;
	}
	retval = (*ftwx_data.user_func)( path, &st, ftw_flag ) ;
	if ( retval < 0 )
		return( retval ) ;
	else if ( retval > 0 && ftw_flag == FTW_D )
		traverse = NO ;

	/*
	 * Stop traversal if:
	 *		a. depth reached 0
	 *		b. the current path is not a readable directory
	 *		c. the user doesn't want us to traverse this directory tree
	 */
	if ( depth == 0 || ftw_flag != FTW_D || traverse == NO )
		return( 0 ) ;

	if ( depth != FTWX_ALL )
		depth-- ;

	if ( ( dirp = opendir( path ) ) == NULL )
		return( -1 ) ;

	for ( ;; )
	{
		struct dirent *dp ;
		char *filename ;

		errno = 0 ;			/* to detect readdir errors */
		dp = readdir( dirp ) ;
		if ( dp == NULL )
		{
			retval = ( errno == 0 ) ? 0 : -1 ;
			break ;
		}

		/*
		 * The special names: "." and ".." are skipped
		 */
		if ( dp->d_name[ 0 ] == '.' )
			if ( dp->d_name[ 1 ] == NUL ||
						dp->d_name[ 1 ] == '.' && dp->d_name[ 2 ] == NUL )
				continue ;

		filename = make_pathname( 2, path, dp->d_name ) ;
		if ( filename == NULL )
		{
			retval = -1 ;
			break ;
		}
		
		retval = ftwx_traverse( filename, depth ) ;
		free( filename ) ;

		/*
		 * Check for a negative value instead of -1 because the
		 * user function may use any negative value
		 */
		if ( retval < 0 )
			break ;
	}
	/*
	 * Make sure we don't trash errno; we should only do this if
	 * retval is negative, but we are lazy...
	 */
	save_errno = errno ;
	(void) closedir( dirp ) ;
	errno = save_errno ;
	return( retval ) ;
}

