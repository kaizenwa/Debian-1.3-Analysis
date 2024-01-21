/* libsrc/local_nc.h.  Generated automatically by configure.  */
/*
 *	Copyright 1993, University Corporation for Atmospheric Research
 *      See netcdf/COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: local_nc.h,v 1.58 1996/02/22 17:23:23 steve Exp $ */
#ifndef _LOCAL_NC_
#define _LOCAL_NC_

/*
 *	netcdf library 'private' data structures, objects and interfaces
 */

#include	<stddef.h> /* size_t */
#include	<stdio.h> /* FILENAME_MAX */

/*
 * Arrgh. HP thinks FILENAME_MAX is _POSIX_NAME_MAX and autoconf(1)
 * comments-out all #undef's.
 */
#if !defined(FILENAME_MAX) || FILENAME_MAX < 255
#define NC_FILENAME_MAX	255
#else
#define NC_FILENAME_MAX	FILENAME_MAX
#endif

#ifndef	NO_SYSTEM_XDR_INCLUDES
#include	<rpc/types.h>
#include	<rpc/xdr.h>
#else
#ifndef WINNT
#include	<types.h> /* "../xdr/types.h" */
#include	<xdr.h>	 /* "../xdr/xdr.h" */
#else
#include        "types.h"
#include        "xdr.h"
#endif
#endif /* !NO_SYSTEM_XDR_INCLUDES */

#include	"netcdf.h" /* needed for defs of nc_type, ncvoid, ... */

/* ptr argument type in internal functions */
#define Void    char

/*
 * The following are determined by the configure script on UNIX platforms:
 */
#if !defined(SIZEOF_INT)
#   define SIZEOF_INT		4	/* Byte size of an `int' */
#endif
#if !defined(SIZEOF_LONG)
#   define SIZEOF_LONG		4	/* Byte size of a `long' */
#endif
#if !defined(SIZEOF_NCLONG)
#   define SIZEOF_NCLONG	SIZEOF_LONG	/* Byte size of an `nclong' */
#endif
#if !defined(SWAP)
#   define SWAP			0	/* Whether to swap bytes because the */
					/* machine is little-endian */
#endif
#if !defined(NETLONG)
#   define NETLONG		long	/* Type corresponding to a 32-bit */
					/* external integer */
#endif
#if !defined(SIZEOF_NETLONG)
#   define SIZEOF_NETLONG	4
					/* Byte size of the type */
					/* corresponding to a 32-bit external */
					/* integer */
#endif
#if !defined(INTERNAL_NETLONG)
#   define INTERNAL_NETLONG	long	/* Type used by the XDR */
					/* implementation to hold a 32-bit */
					/* external integer */
#endif
#if !defined(SIZEOF_INTERNAL_NETLONG)
#   define SIZEOF_INTERNAL_NETLONG	4
					/* Byte size of the type used */
					/* by the XDR implementation to hold */
					/* a 32-bit external integer */
#endif
#if !defined(INLINE)
#   define INLINE		netlong	/* Type pointed-to by the `inline' */
					/* function of the XDR implementation */
#endif
#if !defined(BIOBUFSIZ)
#   define BIOBUFSIZ		8192	/* Byte size of a block I/O buffer */
#endif
#if !defined(HAS_XDR_GETINT)
#   define HAS_XDR_GETINT	0	/* Whether the XDR implementation */
					/* uses xdr_getint() */
#endif

/* like, a discriminated union in the sense of xdr */
typedef struct {
	nc_type type ;		/* the discriminant */
	size_t len ;		/* the total length originally allocated */
	size_t szof ;		/* sizeof each value */
	unsigned count ;	/* length of the array */
	Void *values ;		/* the actual data */
} NC_array ;

/* Counted string for names and such */
typedef struct {
	unsigned count ;
	char *values ;
} NC_string ;

/* Counted array of ints for assoc list */
typedef struct {
	unsigned count ;
	int *values ;
} NC_iarray ;

/* NC dimension stucture */
typedef struct {
	NC_string *name ;
	long size ;
} NC_dim ;

/* NC attribute */
typedef struct {
	NC_string	*name ;
	NC_array	*data ;
} NC_attr ;

/* NC variable: description and data */
typedef struct {
	NC_string *name ;
	NC_iarray *assoc ; /* user definition */
	unsigned long *shape ; /* compiled info */
	unsigned long *dsizes ; /* compiled info */
	NC_array *attrs;
	nc_type type ;		/* the discriminant */
	unsigned long len ;		/* the total length originally allocated */
	size_t szof ;		/* sizeof each value */
	long begin ;  /* seek index, often an off_t */
} NC_var ;

#define IS_RECVAR(vp) \
	((vp)->shape != NULL ? (*(vp)->shape == NC_UNLIMITED) : 0 )

typedef struct {
	char path[NC_FILENAME_MAX + 1] ;
	unsigned flags ;
	XDR *xdrs ;
	long begin_rec ; /* (off_t) postion of the first 'record' */
	unsigned long recsize ; /* length of 'record' */
	int redefid ;
	/* below gets xdr'd */
	unsigned long numrecs ; /* number of 'records' allocated */
	NC_array *dims ;
	NC_array *attrs ;
	NC_array *vars ;
} NC ;

extern char *cdf_routine_name ; /* defined in lerror.c */

               /*  C D F 1 */
#define	NCMAGIC	0x43444601
                       /*  C D L 1 */
#define	NCLINKMAGIC	0x43444c01

/* #undef PROTO */
#ifndef NO_HAVE_PROTOTYPES 
#   define	PROTOTYPE(x)	x
#else
#   define	PROTOTYPE(x)	()
#endif

#ifdef __cplusplus
extern "C" {
#endif

extern void		nc_serror	PROTOTYPE((
	char *fmt,
	...
)) ;
extern void		NCadvise	PROTOTYPE((
	int err,
	char *fmt,
	...
)) ;

extern int        NC_computeshapes	PROTOTYPE((
    NC		*handle
));
extern int        NC_xtypelen		PROTOTYPE((
    nc_type	type
));
extern int        NC_xlen_array		PROTOTYPE((
    NC_array	*array
));
extern int        NC_xlen_attr		PROTOTYPE((
    NC_attr	**app
));
extern int        NC_xlen_cdf		PROTOTYPE((
    NC		*cdf
));
extern int        NC_xlen_dim		PROTOTYPE((
    NC_dim	**dpp
));
extern int        NC_xlen_iarray	PROTOTYPE((
    NC_iarray	*iarray
));
extern int        NC_xlen_string	PROTOTYPE((
    NC_string	*cdfstr
));
extern int        NC_xlen_var		PROTOTYPE((
    NC_var	**vpp
));
extern int	  NCxdrfile_create	PROTOTYPE((
    XDR		*xdrs,
    const char  *name,
    int		 mode
));
extern int	  NCxdrfile_sync	PROTOTYPE((
    XDR		*xdrs
));

extern char       *NCmemset		PROTOTYPE((
    char	*s,
    int		c,
    int		n
));

extern void       NC_arrayfill		PROTOTYPE((
    Void	*lo,
    size_t	len,
    nc_type	type
));
extern void       NC_copy_arrayvals	PROTOTYPE((
    char	*target,
    NC_array	*array
));
extern void       NC_free_array		PROTOTYPE((
    NC_array	*array
));
extern void       NC_free_attr		PROTOTYPE((
    NC_attr	*attr
));
extern void       NC_free_cdf		PROTOTYPE((
    NC		*handle
));
extern void       NC_free_dim		PROTOTYPE((
    NC_dim	*dim
));
extern void       NC_free_iarray	PROTOTYPE((
    NC_iarray	*iarray
));
extern void       NC_free_string	PROTOTYPE((
    NC_string	*cdfstr
));
extern void       NC_free_var		PROTOTYPE((
    NC_var	*var
));

extern Void      *NC_incr_array		PROTOTYPE((
    NC_array	*array,
    Void	*tail
));

extern bool_t     NCcktype		PROTOTYPE((
    nc_type	datatype
));
extern bool_t     NC_indefine		PROTOTYPE((
    int		cdfid,
    bool_t	iserr
));
extern bool_t     xdr_cdf		PROTOTYPE((
    XDR		*xdrs,
    NC		**handlep
));
extern bool_t	 xdr_doubles		PROTOTYPE((
    XDR		*xdrs,
    double	*values,
    u_int	 cnt
));
extern bool_t	 xdr_floats		PROTOTYPE((
    XDR		*xdrs,
    float	*values,
    u_int	 cnt
));
extern bool_t	  xdr_nclongs		PROTOTYPE((
    XDR		*xdrs,
    nclong	*values,
    u_int	 cnt
));
extern bool_t     xdr_numrecs		PROTOTYPE((
    XDR		*xdrs,
    NC		*handle
));
extern bool_t     xdr_shorts		PROTOTYPE((
    XDR		*xdrs,
    short	*sp,
    u_int	cnt
));
#if SIZEOF_NCLONG <= SIZEOF_INT
#   define xdr_nclong	xdr_int
#elif SIZEOF_NCLONG == SIZEOF_LONG && SIZEOF_LONG == 4
#   define xdr_nclong	xdr_long
#else
#	include error Unhandled nclong type in file __FILE__, line __LINE__
#endif
extern bool_t     xdr_NC_array		PROTOTYPE((
    XDR		*xdrs,
    NC_array	**app
));
extern bool_t     xdr_NC_attr		PROTOTYPE((
    XDR		*xdrs,
    NC_attr	**app
));
extern bool_t     xdr_NC_dim		PROTOTYPE((
    XDR		*xdrs,
    NC_dim	**dpp
));
extern bool_t     xdr_NC_fill		PROTOTYPE((
    XDR		*xdrs,
    NC_var	*vp
));
extern bool_t     xdr_NC_iarray		PROTOTYPE((
    XDR		*xdrs,
    NC_iarray	**ipp
));
extern bool_t     xdr_NC_string		PROTOTYPE((
    XDR		*xdrs,
    NC_string	**spp
));
extern bool_t     xdr_NC_var		PROTOTYPE((
    XDR		*xdrs,
    NC_var	**vpp
));

extern size_t     NC_typelen		PROTOTYPE((
    nc_type	type
));

extern NC        *NC_check_id		PROTOTYPE((
    int		cdfid
));
extern NC        *NC_dup_cdf		PROTOTYPE((
    const char *name,
	int     mode,
    NC		*old
));
extern NC        *NC_new_cdf		PROTOTYPE((
    const char *name,
    int		mode
));
extern NC_array  *NC_new_array		PROTOTYPE((
    nc_type	type,
    unsigned	count,
    const void	*values
));
extern NC_array  *NC_re_array		PROTOTYPE((
    NC_array	*old,
    nc_type	type,
    unsigned	count,
    const void	*values
));
extern NC_attr  **NC_findattr		PROTOTYPE((
    NC_array	**ap,
    const char	*name
));
extern NC_dim    *NC_new_dim		PROTOTYPE((
    const char	*name,
    long	size
));
extern NC_iarray *NC_new_iarray		PROTOTYPE((
    unsigned	count,
    const int		values[]
));
extern NC_string *NC_new_string		PROTOTYPE((
    unsigned	count,
    const char	*str
));
extern NC_string *NC_re_string		PROTOTYPE((
    NC_string	*old,
    unsigned	count,
    const char	*str
));
extern NC_var    *NC_hlookupvar		PROTOTYPE((
    NC		*handle,
    int		varid
));
extern NC_var    *NC_new_var		PROTOTYPE((
    const char	*name,
    nc_type	type,
    int		ndims,
    const int		*dims
));
extern int	NCvario			PROTOTYPE((
	NC *handle,
	int varid,
	const long *start,
	const long *edges,
	Void *values
));
extern bool_t	NCcoordck		PROTOTYPE((
	NC *handle,
	NC_var *vp, 
	const long *coords
));
extern bool_t xdr_NCvshort		PROTOTYPE((
	XDR *xdrs,
	unsigned which,
	short *values
));
extern bool_t	NC_dcpy			PROTOTYPE((
	XDR *target,
	XDR *source,
	long nbytes
));


#ifdef __cplusplus
}
#endif

#endif /* _LOCAL_NC_ */
