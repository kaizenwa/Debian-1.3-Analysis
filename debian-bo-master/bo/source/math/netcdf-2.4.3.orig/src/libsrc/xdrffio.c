#if !defined(lint)
static char rcsid[] = "$Id: xdrffio.c,v 2.7 1996/06/27 19:38:48 steve Exp $";
#endif

/*
 * XDR implementation on CRI FFIO file interface
 *
 * Copyright (C) 1995, University Corp for Atmospheric Research
 *
 * These routines implement an XDR on a Cray Research FFIO descriptor.
 * XDR_ENCODE serializes onto the descriptor, XDR_DECODE de-serializes
 * from the descriptor. The interface presented is roughly based on 
 * Glenn Davis's xdrposix layer, but the internal buffering scheme has
 * been replaced with the FFIO libraries to correct performance problems
 * caused by the single buffer scheme used in xdrposix.c.  The FFIO
 * XDR will allow transparent configuration of the system interface below
 * the XDR layer.  The most likely canidate being a multilevel cache with
 * a synchronous layer in memory and and asynchronous layer on the SSD.
 * The default is a single layer synchronous cache in memory so we don't
 * surprise the user with unexpected SSD usage.
 *
 * --jeff
 *
 * For T3D/T3E systems, you can't use FFIO if you would like multiple PEs
 * to do I/O to shared files.  This is because the FFIO library maintains
 * a local cache - it doesn't know about I/O being done by other PEs.  The
 * "par_io" library uses a globally coherent cache and provides FFIO-like
 * behavior, and should be used instead.  
 *
 * Steve Luzmoor
 */


#include <stdio.h>

#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <foreign.h>
#include <sys/types.h>
#include <string.h>
#include "netcdf.h"	/* NC_ */
#include "local_nc.h"	/* prototypes for NCadvis, nc_error 	*/
		     	/* also obtains <stdio.h>, <rpc/types.h>*/
		      	/*  and <rpc/xdr.h>			*/
#ifdef _CRAYMPP
#include "par_io.h"
#endif

typedef	long	netlong;
typedef u_int	ncpos_t;
			
static bool_t	 xdrcrayffio_getlong();
static bool_t	 xdrcrayffio_putlong();
static bool_t	 xdrcrayffio_getbytes();
static bool_t	 xdrcrayffio_putbytes();
static bool_t	 xdrcrayffio_setpos();
static ncpos_t	 xdrcrayffio_getpos();
static inline_t *xdrcrayffio_inline();
static void	 xdrcrayffio_destroy();

/* Ops vector for FFIO type XDR */
static struct xdr_ops	xdrcrayffio_ops = {
	xdrcrayffio_getlong,	/* deserialize a 32-bit int */
	xdrcrayffio_putlong,	/* serialize a 32-bit int */
	xdrcrayffio_getbytes,	/* deserialize counted bytes */
	xdrcrayffio_putbytes,	/* serialize counted bytes */
	xdrcrayffio_getpos,		/* get offset in the stream */
	xdrcrayffio_setpos,		/* set offset in the stream */
	xdrcrayffio_inline,		/* prime stream for inline macros */
	xdrcrayffio_destroy		/* destroy stream */
};




/*
 * Initialize a FFIO xdr stream.
 * puts xdrs in a sane state after an ffopen()
 */
static int
xdrcrayffio_create(xdrs, fd, fmode, op)
	XDR *xdrs;
	int fd;
	int fmode;
	enum xdr_op op;
{
        xdrs->x_op      = op;
	xdrs->x_ops     = & xdrcrayffio_ops;
	xdrs->x_private	= (caddr_t) fd ;
	xdrs->x_handy   = 0;	/* unused */
	xdrs->x_base    = 0;	/* unused */

	return( 1 );	/* interface compatibility only */
}

/*
 * "sync" a FFIO xdr stream.
 */
static int
xdrcrayffio_sync(xdrs)
	XDR *xdrs;
{
#ifdef _CRAYMPP
	if ( par_sync( (int)(xdrs->x_private) ) < 0 ) {
#else
	if ( ffflush( (int)(xdrs->x_private) ) < 0 ) {
#endif
		return (-1); 
	}
	return (1);
}


/*
 * Destroy a FFIO xdr stream.
 * Cleans up the xdr stream handle xdrs previously set up by xdrcrayffio_create.
 */
static void
xdrcrayffio_destroy(xdrs)
	XDR *xdrs;
{
#ifdef _CRAYMPP
	barrier(); /* for compatibility with proposed FFIO implementation */
	(void) par_close( (int)(xdrs->x_private) );
#else
	(void) ffclose( (int)(xdrs->x_private) );
#endif
}

/*
 * extract a long from the FFIO xdr stream
 */
static bool_t
xdrcrayffio_getlong(xdrs, lp)
	XDR *xdrs;
	netlong *lp;
{
	char *up = (char *)lp ;
	*lp = 0;
	up += 4;	/* dump it into the bottom of the word */
#ifdef _CRAYMPP
	if( par_read( (int)(xdrs->x_private), up, 4) < 4 )
#else
	if( ffread( (int)(xdrs->x_private), up, 4) < 4 )
#endif
		return (FALSE);
	return (TRUE);
}

/*
 * put a long on the FFIO xdr stream
 */
static bool_t
xdrcrayffio_putlong(xdrs, lp)
	XDR *xdrs;
	netlong *lp;
{
	char *up = (char *)lp ;
	up += 4;	/* grab it from the bottom of the word */
#ifdef _CRAYMPP
	if ( par_write((int)(xdrs->x_private), up, 4) < 4 )
#else
	if ( ffwrite((int)(xdrs->x_private), up, 4) < 4 )
#endif
		return (FALSE);
	return (TRUE);
}

/*
 * extract (len) bytes from the FFIO xdr stream
 */
static bool_t
xdrcrayffio_getbytes(xdrs, addr, len)
	XDR *xdrs;
	caddr_t addr;
	u_int len;
{
#ifdef _CRAYMPP
	if ((len != 0) && (par_read((int)(xdrs->x_private), (char *)addr, (int)len) != len))
#else
	if ((len != 0) && (ffread((int)(xdrs->x_private), (char *)addr, (int)len) != len))
#endif
		return (FALSE);
	return (TRUE);
}

/*
 * put (len) bytes on the FFIO xdr stream
 */
static bool_t
xdrcrayffio_putbytes(xdrs, addr, len)
	XDR *xdrs;
	caddr_t addr;
	u_int len;
{
#ifdef _CRAYMPP
	if ((len != 0) && (par_write((int)(xdrs->x_private), (char *)addr, (int)len) != len))
#else
	if ((len != 0) && (ffwrite((int)(xdrs->x_private), (char *)addr, (int)len) != len))
#endif
		return (FALSE);
	return (TRUE);
}

/*
 * get the current offset of the FFIO xdr stream
 */
static ncpos_t
xdrcrayffio_getpos(xdrs)
	XDR *xdrs;
{
#ifdef _CRAYMPP
	return ( (ncpos_t) par_seek((int)(xdrs->x_private), 0, SEEK_CUR) );
#else
	return ( (ncpos_t) ffseek((int)(xdrs->x_private), 0, SEEK_CUR) );
#endif
}

/*
 * set the current offset of the FFIO xdr stream
 */
static bool_t
xdrcrayffio_setpos(xdrs, pos) 
	XDR *xdrs;
	ncpos_t pos;
{ 
#ifdef _CRAYMPP
	if ( (ncpos_t)par_seek((int)(xdrs->x_private), pos, SEEK_SET) != pos )
#else
	if ( (ncpos_t)ffseek((int)(xdrs->x_private), pos, SEEK_SET) != pos )
#endif
		return FALSE ;
	return TRUE ;
}

/*
 * not with FFIO... stub to hold a place in the ops array
 */
static inline_t *
xdrcrayffio_inline(xdrs, len)
	XDR *xdrs;
	u_int len;
{
	/*
	 * can't inline with ffio... sorry
	 */
	return (NULL);
}


/*
 * synchronize an FFIO xdr stream
 */
int
NCxdrfile_sync(xdrs)
	XDR *xdrs ;
{
	return xdrcrayffio_sync(xdrs) ;
}


/*
 * create an FFIO xdr stream.
 * opens the file. sets the xdrs to a sane state by calling
 * xdrcrayffio_create().
 */

/* SWANSON
 * for error message processing, we need some fixed
 * strings
 */

static char	*mess1 = "Unable to open file: ";
static char	*mess2 = "\n";
static char	*mess3 = "Error Explanation:";

int
NCxdrfile_create(xdrs, path, ncmode)
	XDR *xdrs ;
	const char *path ;
	int ncmode ;
{
#ifdef _CRAYMPP
	extern _MPP_N_PES;
	static bool_t first_time = TRUE;
#endif
	int fmode ;
	int	 fd ;
	enum xdr_op op ;
	char *ControlString;
	struct ffsw stat;

	/* SWANSON
	 * for error message processing, we need some errors
	 * already described for us
	 */
	extern char	*_fdc_errlist[];

	switch(ncmode & 0x0f) {
	case NC_NOCLOBBER:
		fmode = O_RDWR | O_CREAT | O_EXCL;
		break;
	case NC_CLOBBER:
		fmode = O_RDWR | O_CREAT | O_TRUNC;
		break;
	case NC_WRITE:
		fmode = O_RDWR;
		break;
	case NC_NOWRITE:
		fmode = O_RDONLY;
		break;
	default:
		NCadvise(NC_EINVAL, "Bad flag %0x", ncmode & 0x0f);
		return (-1);
	}

	ControlString = getenv("NETCDF_FFIOSPEC");
	if ( ControlString == 0 ) {
		ControlString="bufa:336:2";
	}
#ifdef _CRAYMPP
	if (first_time == TRUE) {
		/* 2 pages per PE, each 512 Kbytes in size */
		par_io_init(2 * _MPP_N_PES, 128);
		first_time = FALSE;
	}
	/*
	 * Note that when par_io functionality is added to FFIO for the
	 * T3D/T3E, "par_io_init()" arguments will probably be incorporated
	 * into the "par_open" parameters.
	 */

	fd = par_open(path, fmode, 0666);
	/* wait for all PEs to complete open, as the FFIO layer will */
	barrier();
#else
	fd = ffopens(path, fmode, 0666, 0, &stat, ControlString) ;
#endif
	if( fd == -1 ) {
	/* SWANSON
	 * we have an error on the open. There are many possible
	 * reasons, but the primary cause we've seen is being out of
	 * memory. If we try to use the nc_serror routine
	 * issue this error message, it too will run out
	 * of memory, and the message will be garbled, if
	 * issued at all.
	 *
 	 * we will try to issue the message directly to
	 * file handle 2 (stderr)
	 *
	 */
		write(2,mess1,strlen(mess1));
		write(2,path,strlen(path));
		write(2,mess2,1); /* end of line */
		write(2,mess3,strlen(mess3)); /* start of line */
		write(2,_fdc_errlist[stat.sw_error - 5000],
			strlen(_fdc_errlist[stat.sw_error - 5000]));
		write(2,mess2,1); /* end of line */
		/* nc_serror("filename \"%s\"", path) ; */
		return (-1);
	}

	if( ncmode & NC_CREAT ) {
		op = XDR_ENCODE;
	} else {
		op = XDR_DECODE;
	}
	
	if(xdrcrayffio_create(xdrs, fd, fmode, op) < 0) {
		return (-1);
	}

	return (fd);
}
