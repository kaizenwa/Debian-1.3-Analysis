/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Header: /upc/share/CVS/netcdf/ncgen/genlib.h,v 1.8 1995/12/05 00:08:58 russ Exp $
 *********************************************************************/

extern const char *progname;	/* for error messages */
extern const char *cdlname;	/* for error messages */

#define FORT_MAX_LINES	20	/* max lines in FORTRAN statement */
#define	FORT_MAX_STMNT	66*FORT_MAX_LINES /* max chars in FORTRAN statement */
#define C_MAX_STMNT	FORT_MAX_STMNT /* until we fix to break up C lines */


#undef PROTO
#ifndef NO_HAVE_PROTOTYPES 
#   define	PROTO(x)	x
#else
#   define	PROTO(x)	()
#endif

#ifdef __cplusplus
extern "C" {
#endif

void cline	PROTO((
    char* stmnt
    ));
void fline	PROTO((
    char* stmnt
    ));
char* nctype	PROTO((
    nc_type  type
    ));
char* ncctype	PROTO((
    nc_type  type
    ));
char* fstring	PROTO((
    nc_type  type, void* valp, int num
    ));
char* cstrstr	PROTO((
    char* valp, long len
    ));
char* fstrstr	PROTO((
    char* str, long ilen
    ));
void define_netcdf	PROTO((
    char* netcdfname
    ));

extern void	derror		PROTO((
				       char *fmt,
				       ...
				       ));
extern void	*emalloc	PROTO((
				       int size
				       ));
extern void	*erealloc	PROTO((
				       void *ptr,
				       int size
				       ));
extern void	expe2d		PROTO((
				       char *ptr
				       ));
extern void	grow_iarray	PROTO((
				       int narray,
				       int **array
				       ));
extern void	grow_varray	PROTO((
				       int narray,
				       struct vars **array
				       ));
extern void	grow_darray	PROTO((
				       int narray,
				       struct dims **array
				       ));
extern void	grow_aarray	PROTO((
				       int narray,
				       struct atts **array
				       ));

#ifdef __cplusplus
}
#endif
