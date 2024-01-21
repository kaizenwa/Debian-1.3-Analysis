/*
 *	Copyright 1993, University Corporation for Atmospheric Research
 *      See netcdf/COPYRIGHT file for copying and redistribution conditions.
 */
/*	$Id: iarray.c,v 1.19 1995/02/09 22:23:53 steve Exp $ */

#include	"local_nc.h"
#include	"alloc.h"


NC_iarray *
NC_new_iarray(count, values)
unsigned count ;
const int *values ;		/* VAX C doesn't like values[] */
{
	NC_iarray *ret ;
	int *ip ;
	size_t memlen ;

	ret = (NC_iarray *)malloc(sizeof(NC_iarray)) ;
	if( ret == NULL )
		goto alloc_err ;
	ret->count = count ;
	if(count != 0 ) /* allocate */
	{
		memlen = count * sizeof(int) ;
		ret->values = (int *)malloc(memlen) ;
		if(ret->values == NULL)
			goto alloc_err ;
		if(values != NULL) /* copy them in */
		{
			for(ip = ret->values ; count > 0; count--)
				*ip++ = *values++ ;
		}
	} else {
		ret->values = NULL ;
	}
	
	return(ret) ;
alloc_err :
	if (ret)
	    Free(ret);
	nc_serror("NC_new_iarray") ;
	return(NULL) ;
}


/*
 * Free iarray, and, if needed, its values.
 */
void
NC_free_iarray(iarray)
NC_iarray *iarray ;
{
	if(iarray == NULL)
		return ;
	if(iarray->values != NULL)
		Free(iarray->values) ;
	Free(iarray) ;
}


bool_t
xdr_NC_iarray(xdrs, ipp)
	XDR *xdrs;
	NC_iarray **ipp;
{
	int *ip ;
	u_long count ;
	bool_t stat = TRUE ;

	switch (xdrs->x_op) {
	case XDR_FREE:
		NC_free_iarray((*ipp)) ;
		return(TRUE) ;
	case XDR_DECODE:
		/* need the length to pass to new */
		if (! xdr_u_long(xdrs, &count)) {
			return (FALSE);
		}
		(*ipp) = NC_new_iarray((unsigned)count, (int *)NULL) ;
		if((*ipp) == NULL)
			return(FALSE) ;
		/* then deal with the array */
		for( ip = (*ipp)->values ; (count > 0 ) && stat ; count-- )
			stat = xdr_int(xdrs, ip++ ) ;
		return(stat) ;
	case XDR_ENCODE:
		/* first deal with the length */
		count = (*ipp)->count ;
		if (! xdr_u_long(xdrs, &count) ) {
			return (FALSE);
		}
		/* then deal with the array */
		for(ip = (*ipp)->values  ; (count > 0 ) && stat ; count--)
			stat = xdr_int(xdrs, ip++ ) ;
		return(stat) ;
	}
	return(FALSE) ;
}


/*
 * How much space will the xdr'd iarray take.
 */
NC_xlen_iarray(iarray)
NC_iarray *iarray ;
{
	int len = 4 ;
	if(iarray!=NULL)
	{
		len += iarray->count * 4 ;
	}
	return(len) ;
}
