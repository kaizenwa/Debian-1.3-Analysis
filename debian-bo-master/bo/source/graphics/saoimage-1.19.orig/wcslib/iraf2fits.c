/*** File iraf2fits.c
 *** February 9, 1996
 *** By Doug Mink, Harvard-Smithsonian Center for Astrophysics

 * Module:      iraf2fits.c (Convert IRAF header to FITS header)
 * Purpose:     Convert IRAF header string to FITS header string
 * Subroutine:  iraf2fits(irafheader,nbiraf,fitsheader,nbfits) returns FITS string length

 * Copyright:   1995, 1996 Smithsonian Astrophysical Observatory
 *              You may do anything you like with this file except remove
 *              this copyright.  The Smithsonian Astrophysical Observatory
 *              makes no representations about the suitability of this
 *              software for any purpose.  It is provided "as is" without
 *              express or implied warranty.

*/

#include <stdio.h>

#define	IM_PHYSLEN	13	/* physical length (as stored) */
#define	IM_TITLE	183	/* image name string */
#define IM_PIXTYPE	4	/* datatype of the pixels */

/* Codes from iraf/unix/hlib/iraf.h */
#define TY_CHAR		2
#define TY_SHORT	3
#define TY_INT		4
#define TY_LONG		5
#define TY_REAL		6
#define TY_DOUBLE	7
#define TY_COMPLEX	8

int iraf2fits (irafheader,nbiraf,fitsheader,nbfits)

int 	*irafheader;	/* IRAF image header */
int	nbiraf;		/* Number of bytes in IRAF image */
char	*fitsheader;	/* FITS image header (returned) */
int	nbfits;		/* Maximum length of FITS header */
{
	int lfhead;		/* Actual length of FITS header (returned) */
	char objname[64];	/* object name from fits file */
	int	nbits;		/* Number of bits per pixel value */
	int i, j, nax;
	short *irafobj;

	int ncr, nblock;
	char *fhead, *fhead1, *fp, *fhmax, endline[81];
	short *irafline;
	char fitsline[81];

	(void)strncpy (endline,"END                                     ",40);
	(void)strncpy (endline+40,"                                        ",40);

/*  Initialize FITS header */
	fhead = fitsheader;
	lfhead = 0;
	fhmax = fhead + nbfits;
	if (fhead+400 > fhmax)
	    return 0;
	(void)strncpy (fitsheader, endline, 80);
	hputl (fitsheader, "SIMPLE", 1);
	fhead = fhead + 80;
	lfhead = lfhead + 80;

/*  Set pixel size in FITS header */
	switch (irafheader[IM_PIXTYPE] ) {
	case TY_CHAR:
	    nbits = 8;
	    break;
	case TY_SHORT:
	    nbits = 16;
	    break;
	case TY_INT:
	case TY_LONG:
	    nbits = 32;
	    break;
	case TY_REAL:
	    nbits = -32;
	    break;
	case TY_DOUBLE:
	    nbits = -64;
	    break;
	default:
	    (void)fprintf(stderr,"Unsupported data type: %d\n",
	      irafheader[IM_PIXTYPE]);
	    return (0);
	}
	hputi4 (fitsheader,"BITPIX",nbits);
	fhead = fhead + 80;
	lfhead = lfhead + 80;

/*  Set image dimensions in fits header */
	nax = irafheader[11];
	hputi4 (fitsheader,"NAXIS",nax);
	fhead = fhead + 80;
	lfhead = lfhead + 80;
	hputi4 (fitsheader,"NAXIS1",irafheader[IM_PHYSLEN]);
	fhead = fhead + 80;
	lfhead = lfhead + 80;
	hputi4 (fitsheader,"NAXIS2",irafheader[IM_PHYSLEN+1]);
	fhead = fhead + 80;
	if (fhead > fhmax) {
	    (void)strncpy (fhead-80, endline,80);
	    return (lfhead);
	    }
	lfhead = lfhead + 80;
	if (nax > 2) {
	    hputi4 (fitsheader,"NAXIS3",irafheader[IM_PHYSLEN+2]);
	    fhead = fhead + 80;
	    if (fhead > fhmax) {
		(void)strncpy (fhead-80, endline,80);
		return (lfhead);
		}
	    lfhead = lfhead + 80;
	    }
	if (nax > 3) {
	    hputi4 (fitsheader,"NAXIS4",irafheader[IM_PHYSLEN+3]);
	    fhead = fhead + 80;
	    if (fhead > fhmax) {
		(void)strncpy (fhead-80, endline,80);
		return (lfhead);
		}
	    lfhead = lfhead + 80;
	    }

/*  Set object name in fits header */
	irafobj = (short *) (irafheader + IM_TITLE);
	i = 0;
	for (i = 0; i < 64; i++) {
	    objname[i] = (char) irafobj[i];
	    if (irafobj[i] == 0)
		break;
	    }
	hputs (fitsheader,"OBJECT",objname);
	fhead = fhead + 80;
	if (fhead > fhmax) {
	    (void)strncpy (fhead-80, endline,80);
	    return (lfhead);
	    }
	if (fhead > fhmax) {
	    (void)strncpy (fhead-80, endline,80);
	    return (lfhead);
	    }
	lfhead = lfhead + 80;

/*  Add user portion of iraf header to fits header */
	ncr = nbiraf / 2;
	fitsline[81] = 0;
	for (i = 1026; i < ncr; i = i + 81) {
	    irafline = ((short *) irafheader) + i;
	    if (irafline[0] == 0) break;
	    for (j = 0; j < 80; j++) {
		if (irafline[j] < 32)
		    irafline[j] = 32;
		fitsline[j] = (char) irafline[j];
		}
	    (void)strncpy (fhead, fitsline, 80);
	    fhead = fhead + 80;
	    if (fhead > fhmax) {
		(void)strncpy (fhead-80, endline,80);
		return (lfhead);
		}
	    /* printf ("%80s\n",fitsline); */
	    lfhead = lfhead + 80;
	    }

/* Add END to last line */
	(void)strncpy (fhead, endline, 80);
	lfhead = lfhead + 80;

/* Find end of last 2880-byte block of header */
	nblock = lfhead / 2880;
	if (nblock*2880 < lfhead)
	    nblock = nblock + 1;
	fhead1 = fitsheader + (nblock * 2880);

/* Pad rest of header with spaces */
	strncpy (endline,"   ",3);
	for (fp = fhead+80; fp < fhead1; fp = fp + 80) {
	    (void)strncpy (fp, endline,80);
	    lfhead = lfhead + 80;
	    }

	return lfhead;
}
