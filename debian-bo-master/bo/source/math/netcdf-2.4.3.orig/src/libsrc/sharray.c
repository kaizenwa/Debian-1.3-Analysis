/*
 *	Copyright 1993, University Corporation for Atmospheric Research
 *      See netcdf/COPYRIGHT file for copying and redistribution conditions.
 */
/*	$Id: sharray.c,v 1.26 1996/02/28 16:11:50 steve Exp $ */

/*
 *      modified Nov 17, 1994 By Jeff Kuehn, NCAR/SCD
 *      changed: libsrc/putget.c:xdr_NCvdata()
 *                      Now vectors of longs, floats and doubles are handled
 *                      in a manner more consistent with vectors of shorts.
 *                      However, all of the control and error handling is
 *                      done in the xdr conversion layer now.  New conversion
 *                      layer routines added to libsrc/sharray.c
 *               libsrc/sharray.c:xdr_shorts()  (*this file*) 
 *      created: libsrc/sharray.c:xdr_nclongs() (*this file*) 
 *               libsrc/sharray.c:xdr_floats()  (*this file*)
 *               libsrc/sharray.c:xdr_doubles() (*this file*)
 *                      New xdr conversion layer routine to support changes
 *                      to libsrc/putget.c:xdr_NCvdata().  These routines
 *                      function by calling the appropriate xdr element
 *                      processor on everything except Cray machines, in
 *                      case they use CRI's IEG2CRAY() and CRAY2IEG()
 *                      routines to convert blocks of data using a 32KW
 *                      buffer.  Coded as three seperate routines for 
 *			consistency with existing xdr_shorts() usage in
 *			libsrc/putget.c:xdr_NCvdata(). xdr_shorts() modified
 *                      for faster conversion.
 *      modifications for Cray IEEE and MPP hardware due to Steve Luzmoor, CRI
 */

#include	"local_nc.h"
#include	"alloc.h"

/* you may wish to tune this: big on a cray, small on a PC? */
#define NC_SHRT_BUFSIZ 8192
#define NC_NSHRTS_PER (NC_SHRT_BUFSIZ/2) /* number of netshorts the buffer holds */

/*
 * Foreign dataset buffer for fast conversions on the cray.  This also
 * to guarantees word alignment for the conversion routines IEG2CRAY() and
 * CRAY2IEG().  Allocate as static because 32K*8bytes is small on a Cray but
 * memory allocations are expensive.  Reuse this  buffer where applicable. 
 * ie. for xdr_longs(), xdr_floats(), xdr_doubles().
 */
#ifdef _CRAY1		/* CRAY PVP systems */
#define CrayBuffSize 32768			/* size in 8 byte words   */
static long IEEEBuff[CrayBuffSize];    		/* word aligned buffer    */
#endif


/*
 * internal function, bulk xdr of an even number of shorts, less than 
 * NC_NSHRTS_PER
 */
static
bool_t
NCxdr_shortsb(xdrs, sp, nshorts)
	XDR *xdrs;
	short *sp;
	u_int nshorts ;
{
	unsigned char buf[NC_SHRT_BUFSIZ] ;
	unsigned char *cp ;
	unsigned int nbytes = nshorts * 2;

	/* assert(nshorts <= NC_NSHRTS_PER) ; */
	/* assert(nshorts > 0) ; */

	if(xdrs->x_op == XDR_ENCODE)
	{
		for(cp = buf ; cp < &buf[nbytes] ; sp++, cp += 2 )
		{
			*(cp +1) = *sp % 256 ;
			*cp = (*sp >> 8) ;
		}
	}

	if(!xdr_opaque(xdrs, (caddr_t)buf, nbytes))
		return FALSE ;
	
	if(xdrs->x_op == XDR_DECODE)
	{
		for(cp = buf ; cp < &buf[nbytes] ; sp++, cp += 2 )
		{
			*sp = ((*cp & 0x7f) << 8) + *(cp +1) ;
			if(*cp & 0x80)
			{
				/* extern is neg */
				*sp -= 0x8000 ;
			}
		}
	}

	return TRUE ;
}


/*
 * Translate an array of cnt short integers at sp.
 */
bool_t
xdr_shorts(xdrs, sp, cnt)
	XDR *xdrs;
	short *sp;
	u_int cnt ;
{
#if !defined(_CRAY) || defined(_CRAYIEEE) || defined(_CRAYMPP)

	/*
	 * Non-CRAY or CRAY IEEE or MPP systems:
	 */

	/*
         * We don't use xdr_vector() in this case because `shorts' in
         * a netCDF dataset are packed into contiguous two-byte words
         * and having xdr_vector() call a special routine that knows
         * about this packing (e.g. xdr_NCvshort()) for every element is
         * expensive and unnecessary.
	 */

	int odd ; /* 1 if cnt is odd, 0 otherwise */

	if(cnt == 0)
		return TRUE ;	/* ? */

	odd = cnt % 2 ;
	if(odd) 
		cnt-- ;
	/* cnt is even, odd is set if apropos */

	while(cnt > NC_NSHRTS_PER)
	{
		if(!NCxdr_shortsb(xdrs, sp, NC_NSHRTS_PER))
			return FALSE ;
		/* else */
		sp += NC_NSHRTS_PER ;
		cnt -= NC_NSHRTS_PER ;
	}

	/* we know cnt <= NC_NSHRTS_PER at this point */

	if(cnt != 0)
	{
		if(!NCxdr_shortsb(xdrs, sp, cnt))
			return FALSE ;
		/* else */
		sp += cnt ;
		cnt = 0 ;
	}

	if(odd)
		if(!xdr_NCvshort(xdrs, 0, sp))
			return FALSE ;

	return TRUE ;

#else	/* CRAY non-IEEE PVP systems below */

	u_int chnksz;
	int Zero  = 0;	/* because buffer is Word Addressed */
	int UnitStride = 1;	/* no stride for xdr      */
	int CrayShort  = 7;	/* 64bit 2's compliment to/from 32bit 2s comp */
	int IEG2CRAY(), CRAY2IEG();
	bool_t stat;
	int odd ; /* 1 if cnt is odd, 0 otherwise */

	if(cnt == 0)
		return TRUE ;	/* ? */

	odd = cnt % 2 ;
	if(odd) 
		cnt-- ;
	/* cnt is even, odd is set if apropos */

	stat = TRUE;
	while (cnt>0)
	{
		/* chnksz is the number of elements to convert */
		/* on this pass.  it must be the lesser of the    */
		/* number of items left or the buffer size        */

		chnksz = ( cnt<4*CrayBuffSize ? cnt : 4*CrayBuffSize );

		if (xdrs->x_op == XDR_DECODE)		/* read operation */
		{
			/* fill the conversion buffer with the next chunk */
			/* of data -- word aligned                        */

			stat = XDR_GETBYTES(xdrs, (char *)IEEEBuff, 2*chnksz);
			if ( stat == FALSE ) break;

			/* convert the word aligned input data, writing   */
			/* the result to the chunk of memory at sp        */
			stat = ! IEG2CRAY(&CrayShort, &chnksz, IEEEBuff,
					&Zero, sp, &UnitStride, 0);
			if ( stat == FALSE ) break;
		}
		else if (xdrs->x_op == XDR_ENCODE)	/* write operation */
		{
			/* convert the output data at sp, writing the     */
			/* results into the word aligned buffer           */
			stat = ! CRAY2IEG(&CrayShort, &chnksz, IEEEBuff,
					&Zero, sp, &UnitStride, 0);
			if ( stat == FALSE ) break;

			/* dump the chunk of output data in the word      */
			/* aligned buffer into the output stream          */

			stat = XDR_PUTBYTES(xdrs, (char *)IEEEBuff, 2*chnksz);
			if ( stat == FALSE ) break;
		}
		else /* should not have been called for this operation */
		{
			stat=FALSE;
			break;
		}
		sp += chnksz;
		cnt -= chnksz;
	}

	if(odd)  /* write the oddball the slow way.... */
		if(!xdr_NCvshort(xdrs, 0, sp))
			stat = FALSE ;

	return (stat);

#endif	/* CRAY non-IEEE PVP systems above */
}


/*
 * Translate an array of cnt long integers at lp.
 * Cray Optimization by Jeff Kuehn, NCAR/SCD.
 * (see also xdr_floats() & xdr_doubles() in this file)
 */
bool_t
xdr_nclongs(xdrs, lp, cnt)
	XDR *xdrs;
	nclong *lp;
	u_int cnt;
{
#if !defined(_CRAY) || defined(_CRAYMPP) || defined(_CRAYIEEE)

	/*
	 * Non-CRAY or CRAY IEEE or MPP systems:
	 */
	return xdr_vector(xdrs, (void *) lp, cnt, sizeof(nclong), 
			  (xdrproc_t)xdr_nclong);

#else

	/*
	 * CRAY non-IEEE PVP systems:
	 */
	bool_t stat;
	u_int chnksz;
	int Zero  = 0;	/* because buffer is W.A. */
	int UnitStride = 1;	/* no stride for xdr      */
	int CrayLong   = 1;	/* 64bit 2's compliment to/from 32bit 2s comp */
	int IEG2CRAY(), CRAY2IEG();

	stat = TRUE;
	while (cnt>0)
	{
		/* chnksz is the number of elements to convert */
		/* on this pass.  it must be the lesser of the    */
		/* number of items left or the buffer size        */

		chnksz = ( cnt<2*CrayBuffSize ? cnt : 2*CrayBuffSize );

		if (xdrs->x_op == XDR_DECODE)		/* read operation */
		{
			/* fill the conversion buffer with the next chunk */
			/* of data -- word aligned                        */

			stat = XDR_GETBYTES(xdrs, (char *)IEEEBuff, 4*chnksz);
			if ( stat == FALSE ) break;

			/* convert the word aligned input data, writing   */
			/* the result to the chunk of memory at lp        */
			stat = ! IEG2CRAY(&CrayLong, &chnksz, IEEEBuff,
					&Zero, lp, &UnitStride, 0);
			if ( stat == FALSE ) break;
		}
		else if (xdrs->x_op == XDR_ENCODE)	/* write operation */
		{
			/* convert the output data at lp, writing the     */
			/* results into the word aligned buffer           */
			stat = ! CRAY2IEG(&CrayLong, &chnksz, IEEEBuff,
					&Zero, lp, &UnitStride, 0);
			if ( stat == FALSE ) break;

			/* dump the chunk of output data in the word      */
			/* aligned buffer into the output stream          */

			stat = XDR_PUTBYTES(xdrs, (char *)IEEEBuff, 4*chnksz);
			if ( stat == FALSE ) break;
		}
		else /* should not have been called for this operation */
		{
			stat=FALSE;
			break;
		}
		lp += chnksz;
		cnt -= chnksz;
	}
	return stat;

#endif	/* CRAY non-IEEE PVP systems above */
}


/*
 * Translate an array of cnt floats at fp.
 * Cray optimization by Jeff Kuehn, NCAR/SCD.
 * (see also xdr_longs() & xdr_doubles() in this file)
 */
bool_t
xdr_floats(xdrs, fp, cnt)
	XDR *xdrs;
	float *fp;
	u_int cnt;
{
#if !defined(_CRAY) || defined(_CRAYMPP) || defined(_CRAYIEEE)

	/*
	 * Non-CRAY or CRAY IEEE or MPP systems:
	 */
	return xdr_vector(xdrs, (void *) fp, cnt, sizeof(float), 
			  (xdrproc_t)xdr_float);

#else

	/*
	 * CRAY non-IEEE PVP systems:
	 */
	bool_t stat;
	u_int chnksz;
	int Zero  = 0;	/* because buffer is W.A. */
	int UnitStride = 1;	/* no stride for xdr      */
	int CrayFloat  = 2;	/* 64bit Cray float to/from 32bit IEEE float  */
	int IEG2CRAY(), CRAY2IEG();

	stat = TRUE;
	while (cnt>0)
	{
		/* chnksz is the number of elements to convert */
		/* on this pass.  it must be the lesser of the    */
		/* number of items left or the buffer size        */

		chnksz = ( cnt<2*CrayBuffSize ? cnt : 2*CrayBuffSize );

		if (xdrs->x_op == XDR_DECODE)		/* read operation */
		{
			/* fill the conversion buffer with the next chunk */
			/* of data -- word aligned                        */

			stat = XDR_GETBYTES(xdrs, (char *)IEEEBuff, 4*chnksz);
			if ( stat == FALSE ) break;

			/* convert the word aligned input data, writing   */
			/* the result to the chunk of memory at fp        */
			stat = ! IEG2CRAY(&CrayFloat, &chnksz, IEEEBuff,
				&Zero, fp, &UnitStride, 0);
			if ( stat == FALSE ) break;
		}
		else if (xdrs->x_op == XDR_ENCODE)	/* write operation */
		{
			/* convert the output data at fp, writing the     */
			/* results into the word aligned buffer           */
			stat = ! CRAY2IEG(&CrayFloat, &chnksz, IEEEBuff,
				&Zero, fp, &UnitStride, 0);
			if ( stat == FALSE ) break;

			/* dump the chunk of output data in the word      */
			/* aligned buffer into the output stream          */

			stat = XDR_PUTBYTES(xdrs, (char *)IEEEBuff, 4*chnksz);
			if ( stat == FALSE ) break;
		}
		else /* should not have been called for this operation */
		{
			stat=FALSE;
			break;
		}
		fp += chnksz;
		cnt -= chnksz;
	}

	return(stat);

#endif	/* CRAY non-IEEE PVP systems above */
}


/*
 * Translate an array of cnt doubles at dp.
 * Cray optimization by Jeff Kuehn, NCAR/SCD.
 * (see also xdr_longs() & xdr_floats() in this file)
 */
bool_t
xdr_doubles(xdrs, dp, cnt)
	XDR *xdrs;
	double *dp;
	u_int cnt;
{
#if !defined(_CRAY) || defined(_CRAYMPP) || defined(_CRAYIEEE)

	/*
	 * Non-CRAY or CRAY IEEE or MPP systems:
	 */
	return xdr_vector(xdrs, (void *) dp, cnt, sizeof(double), 
			  (xdrproc_t)xdr_double);

#else
	/*
	 * CRAY non-IEEE PVP systems:
	 */
	bool_t stat;
	u_int chnksz;
	int Zero  = 0;	/* because buffer is W.A. */
	int UnitStride = 1;	/* no stride for xdr      */
	int CrayDouble = 8;	/* 64bit Cray float to/from 64bit IEEE float */
	int IEG2CRAY(), CRAY2IEG();

	stat = TRUE;
	while( (cnt>0) && stat )
	{
		/* chnksz is the number of elements to convert */
		/* on this pass.  it must be the lesser of the    */
		/* number of items left or the buffer size        */

		chnksz = ( cnt<CrayBuffSize ? cnt : CrayBuffSize );

		if (xdrs->x_op == XDR_DECODE)		/* read operation */
		{
			/* fill the conversion buffer with the next chunk */
			/* of data -- word aligned                        */

			stat = XDR_GETBYTES(xdrs, (char *)IEEEBuff, 8*chnksz);
			if ( stat == FALSE ) break;

			/* convert the word aligned input data, writing   */
			/* the result to the chunk of memory at dp        */
			stat = ! IEG2CRAY(&CrayDouble, &chnksz, IEEEBuff,
					&Zero, dp, &UnitStride, 0);
			if ( stat == FALSE ) break;
		}
		else if (xdrs->x_op == XDR_ENCODE)	/* write operation */
		{
			/* convert the output data at dp, writing the     */
			/* results into the word aligned buffer           */
			stat = ! CRAY2IEG(&CrayDouble, &chnksz, IEEEBuff,
					&Zero, dp, &UnitStride, 0);
			if ( stat == FALSE ) break;

			/* dump the chunk of output data in the word      */
			/* aligned buffer into the output stream          */

			stat = XDR_PUTBYTES(xdrs, (char *)IEEEBuff, 8*chnksz);
			if ( stat == FALSE ) break;
		}
		else /* should not have been called for this operation */
		{
			stat=FALSE;
			break;
		}
		dp += chnksz;
		cnt -= chnksz;
	}

	return(stat);

#endif	/* CRAY non-IEEE PVP systems above */
}
