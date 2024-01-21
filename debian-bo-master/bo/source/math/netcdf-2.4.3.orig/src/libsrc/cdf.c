/*
 *	Copyright 1993, University Corporation for Atmospheric Research
 *      See netcdf/COPYRIGHT file for copying and redistribution conditions.
 */
/*	$Id: cdf.c,v 1.54 1996/06/27 19:38:48 steve Exp $ */

#include	"local_nc.h"
#include	"alloc.h"


/*
 * free the stuff that xdr_cdf allocates
 */
static void
NC_free_xcdf(handle)
NC *handle ;
{
	if(handle == NULL)
		return ;
	NC_free_array(handle->dims) ;
	NC_free_array(handle->attrs) ;
	NC_free_array(handle->vars) ;
}


void
NC_free_cdf(handle)
NC *handle ;
{
	if(handle == NULL)
		return ;

	NC_free_xcdf(handle) ;

	xdr_destroy(handle->xdrs) ;
	Free(handle->xdrs) ;
	Free(handle) ;
}


NC *
NC_new_cdf(name, mode)
const char *name ;
int mode ;
{
	NC *cdf ;

	cdf = (NC *)malloc(sizeof(NC)) ;
	if( cdf == NULL )
	{
		nc_serror("NC_new_cdf") ;
		return(NULL) ;
	}

	cdf->flags = mode ;

	cdf->xdrs = (XDR *)malloc(sizeof(XDR)) ;
	if( cdf->xdrs == NULL)
	{
		nc_serror("NC_new_cdf: xdrs") ;
		Free(cdf) ;
		return(NULL) ;
	} /* else */	

	if( NCxdrfile_create( cdf->xdrs, name, mode ) < 0) 
	{
	        Free(cdf->xdrs) ;
		Free(cdf) ;
		return(NULL) ;
	} /* else */	

	cdf->dims = NULL ;
	cdf->attrs = NULL ;
	cdf->vars = NULL ;
	cdf->begin_rec = 0 ;
	cdf->recsize = 0 ;
	cdf->numrecs = 0 ;
	cdf->redefid = -1 ;

	if(cdf->xdrs->x_op == XDR_DECODE) /* Not NC_CREAT */
	{
		if(!xdr_cdf(cdf->xdrs, &cdf) )
		{
			NC_free_cdf(cdf) ;
			return(NULL) ;
		}
		if( NC_computeshapes(cdf) == -1)
			/* MEMORY: should NC_free_cdf(cdf) be called? */
			return(NULL) ;
	}

	return(cdf) ;
}


/* 
 * Duplicate a description structure.
 * Can only be called for 'old' extant on disk, eg, old in DATA mode.
 */
NC *
NC_dup_cdf(name, mode, old)
const char *name ;
int mode ;
NC *old ;
{
	NC *cdf ;

	cdf = (NC *)malloc(sizeof(NC)) ;
	if( cdf == NULL )
	{
		nc_serror("NC_dup_cdf") ;
		return(NULL) ;
	}

	cdf->flags = old->flags | NC_INDEF ;

	cdf->xdrs = (XDR *)malloc(sizeof(XDR)) ;
	if( cdf->xdrs == NULL)
	{
		nc_serror("NC_dup_cdf: xdrs") ;
		Free(cdf) ;
		return(NULL) ;
	} /* else */	

	cdf->dims = NULL ;
	cdf->attrs = NULL ;
	cdf->vars = NULL ;
	cdf->begin_rec = 0 ;
	cdf->recsize = 0 ;
	cdf->numrecs = 0 ;

	if(NCxdrfile_create( cdf->xdrs, name, mode) < 0)
	{
		Free(cdf) ;
		/* MEMORY: should Free(cdf->xdrs) be called? */
		return(NULL) ;
	} /* else */	

	old->xdrs->x_op = XDR_DECODE ;
	if(!xdr_cdf(old->xdrs, &cdf) )
	{
		NC_free_cdf(cdf) ;
		return(NULL) ;
	}
	if( NC_computeshapes(cdf) == -1)
		/* MEMORY: should NC_free_cdf(cdf) be called? */
		return(NULL) ;

	return(cdf) ;
}


ncinquire(cdfid, ndimsp, nvarsp, nattrsp, xtendimp)
int cdfid ;
int *ndimsp ;
int *nvarsp ;
int *nattrsp ;
int *xtendimp ;
{
	NC *handle ;

	cdf_routine_name = "ncinquire" ;

	handle = NC_check_id(cdfid) ;
	if(handle == NULL)
		return(-1) ;
	
	if(nvarsp != NULL)
		*nvarsp = (handle->vars != NULL) ? handle->vars->count : 0 ;
	if(nattrsp != NULL)
		*nattrsp = (handle->attrs != NULL) ? handle->attrs->count : 0 ;
	if(handle->dims != NULL)
	{
		NC_dim **dp ;
		int ii ;

		if(ndimsp != NULL)
			*ndimsp = handle->dims->count ;
		if(xtendimp != NULL) {
			*xtendimp = -1 ;

			dp = (NC_dim**)handle->dims->values ;
			for(ii = 0 ; ii < handle->dims->count ; ii++, dp++)
			{
				if((*dp)->size == NC_UNLIMITED)
				{
					*xtendimp = ii ;
				}
			}
		}
	} else {
		if(ndimsp != NULL)
			*ndimsp = 0 ;
		if(xtendimp != NULL)
			*xtendimp = -1 ;
	}

	return(cdfid) ;
}


bool_t
xdr_cdf(xdrs, handlep)
	XDR *xdrs;
	NC **handlep;
{

	u_long	magic = NCMAGIC ;

	if( xdrs->x_op == XDR_FREE)
	{
		NC_free_xcdf(*handlep) ;
		return(TRUE) ;
	}
	
	if( xdr_getpos(xdrs) != 0)
	{
		if( !xdr_setpos(xdrs, 0L) )
		{
			nc_serror("Can't set position to begin") ;
			return(FALSE) ;
		}
	}

	/* magic number */
	if( !xdr_u_long(xdrs, &magic) )
	{
		if( xdrs->x_op == XDR_DECODE)
		{
			NCadvise(NC_ENOTNC,
				"Not a netcdf file (Can't read magic number)") ;
		}
		else
		{
			/* write error */
			nc_serror("xdr_cdf: xdr_u_long") ;
		}
		return(FALSE) ;
	}

	if( xdrs->x_op == XDR_DECODE && magic != NCMAGIC )
	{
		if(magic == NCLINKMAGIC)
		{
			NCadvise(NC_NOERR, "link file not handled yet") ;
			return(FALSE) ;
		} /* else */
		NCadvise(NC_ENOTNC, "Not a netcdf file") ;
		return(FALSE) ;
	}

	if( !xdr_numrecs(xdrs, *handlep))
	{
		NCadvise(NC_EXDR, "xdr_numrecs") ;
		return(FALSE) ;
	}
	if( !xdr_NC_array(xdrs, &((*handlep)->dims)))
	{
		NCadvise(NC_EXDR, "xdr_cdf:dims") ;
		return(FALSE) ;
	}
	if( !xdr_NC_array(xdrs, &((*handlep)->attrs)))
	{
		NCadvise(NC_EXDR, "xdr_cdf:attrs") ;
		return(FALSE) ;
	}
	if( !xdr_NC_array(xdrs, &((*handlep)->vars)))
	{
		NCadvise(NC_EXDR, "xdr_cdf:vars") ;
		return(FALSE) ;
	}	
	return(TRUE) ;
}


/*
 * How much space will the xdr'd NC description take.
 *
 */
NC_xlen_cdf(cdf)
NC *cdf ;
{
	int len = 8 ;

	if(cdf == NULL)
		return(0) ;

	len += NC_xlen_array(cdf->dims) ;
	len += NC_xlen_array(cdf->attrs) ;
	len += NC_xlen_array(cdf->vars) ;

	return(len) ;
}


#define RECPOS	4L 	/* seek index of numrecs value */
bool_t
xdr_numrecs(xdrs, handle)
	XDR *xdrs;
	NC *handle;
{
	if( (handle->flags & NC_NOFILL)
		&& xdrs->x_op == XDR_ENCODE
		&& handle->begin_rec > 0)
	{
		/*
		 * we need to write something just beyond the last 
		 * record so we can successfully read back the 
		 * entire last record.
		 */
		if( !xdr_setpos(xdrs, handle->begin_rec
				+  handle->numrecs * handle->recsize) )
		{
			nc_serror("Can't set position to EOF") ;
			return(FALSE) ;
		}
#ifdef RDEBUG
		fprintf(stderr,"\txdr_numrecs %ld = %d + %ld * %d\n",
			xdr_getpos(xdrs), 
			handle->begin_rec, handle->numrecs, handle->recsize) ;
#endif /*  RDEBUG */
		if( !xdr_u_long(xdrs, &(handle->numrecs)) )
			return(FALSE) ;
	}

	if( !xdr_setpos(xdrs, RECPOS) )
	{
		nc_serror("Can't set position to RECPOS") ;
		return(FALSE) ;
	}
	return( xdr_u_long(xdrs, &(handle->numrecs)) ) ;
}


static bool_t
xdr_4bytes(xdrs, cp)
XDR *xdrs ;
char *cp ; /* at least 4 valid bytes */
{
	return xdr_opaque(xdrs, cp, 4) ;
}

static bool_t
xdr_2shorts(xdrs, sp)
XDR *xdrs ;
short *sp ; /* at least 2 valid shorts */
{
	return xdr_shorts(xdrs, sp, 2) ;
}

/* SWANSON
 *
 * make the 'fill' process as efficient as the 'write' process
 * by using vectorized code on the Cray machine
 */

#ifdef _CRAY1		/* CRAY PVP systems */
#define CrayBuffSize 32768			/* size in 8 byte words   */
static float FillBuff[CrayBuffSize];    	/* word aligned buffer    */
static int cray_first_time = 1;
#endif

bool_t
xdr_NC_fill(xdrs, vp)
XDR *xdrs ;
NC_var *vp ;
{
	char fillp[2*sizeof(double)] ;
	bool_t stat ;
	bool_t (*xdr_NC_fnct)() ;
	u_long alen = vp->len ;
	NC_attr **attr = NULL ;
	/* SWANSON
	 * needed for vectorized fill
	 */
#ifdef _CRAY1		/* CRAY PVP systems */
	u_int chnksz;
	int	i;
#endif


	/*
	 * set up fill value
	 */
	/* start with the default */
	NC_arrayfill((Void *)fillp, (size_t)2*sizeof(double),
		vp->type) ;

	/* 
	 * if there is a valid user defined value, use it instead
	 */
	attr = NC_findattr(&vp->attrs, _FillValue) ;
	if( attr != NULL )
	{
		if( (*attr)->data->type != vp->type || (*attr)->data->count != 1 )
			NCadvise(NC_EBADTYPE, "var %s: _FillValue type mismatch",
				vp->name->values) ;
		else
		{
			int len = NC_typelen(vp->type) ;
			char *cp = fillp ;
			while(cp < &fillp[sizeof(fillp) -1])
			{
				NC_copy_arrayvals(cp, (*attr)->data) ;
				cp += len ;
			}
		}
	}

		switch(vp->type){
		case NC_BYTE :
		case NC_CHAR :
			alen /= 4 ;
			xdr_NC_fnct = xdr_4bytes ;
			break ;
		case NC_SHORT :
			alen /= 4 ;
			xdr_NC_fnct = xdr_2shorts ;
			break ;
		case NC_LONG :
			alen /= 4 ;
			xdr_NC_fnct = xdr_nclong ;
			break ;	
		case NC_FLOAT :
			alen /= 4 ;
#if !defined(_CRAY) || defined(_CRAYMPP) || defined(_CRAYIEEE)

			/*
			 * Non-CRAY or CRAY IEEE or MPP systems:
			 */
			xdr_NC_fnct = xdr_float ;
			/* SWANSON
			 * track usage of the xdr_float
			 * routine.
			printf("--CDFfill %d\n",alen);
			printf("--CDFfillf %f\n",*((float *)fillp));
			 */

#else
			/*
			 * CRAY non-IEEE PVP systems:
			 */

			/* SWANSON
			 * we will propogate the fill float
			 * value into an array and pass
			 * that on to the xdr_floats() function.
			 *
			 * of course, it might be nicer to not
			 * have to reconvert and recalculate
			 * every time, but first cut at this
			 * will brute-force it and we'll see
			 * how fast it is
			 */
			stat = TRUE;
			while (alen>0) {
			  chnksz = (alen<CrayBuffSize ? alen : CrayBuffSize);
			  if (cray_first_time) { 
			    /* fill it up first time */
			    cray_first_time = 0;
			    for (i = 0 ; i < chnksz ; i++) {
			      FillBuff[i] = *((float *)fillp);
			    }
			  } else {
			    /* only refill buffer if it's changed */
		            if (FillBuff[0] != *((float *)fillp)) {
			      for (i = 0 ; i < chnksz ; i++) {
			        FillBuff[i] = *((float *)fillp);
			      }
			    }
			  }
			  stat = xdr_floats(xdrs,FillBuff,chnksz);
			  if (stat == FALSE) {
			    break;
			  }
			  alen -= chnksz;
			}
			if(!stat)
			{
				NCadvise(NC_EXDR, "xdr_NC_fill") ;
				return(FALSE) ;
			}
			return(TRUE) ;
#endif
			break ;
		case NC_DOUBLE : 
			alen /= 8 ;
			xdr_NC_fnct = xdr_double ;
			break ;
		default :
			NCadvise(NC_EBADTYPE, "bad type %d", vp->type) ;
			return(FALSE) ;
		}

		/* write out fill values */
		for(stat = TRUE ; stat && (alen > 0) ; alen--)
		{
			stat = (*xdr_NC_fnct)(xdrs,fillp) ;	
		}

		if(!stat)
		{
			NCadvise(NC_EXDR, "xdr_NC_fill") ;
			return(FALSE) ;
		}
		
		return(TRUE) ;
}
