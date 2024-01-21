/*
 * FIG : Facility for Interactive Generation of figures
 * Copyright (c) 1992 by Brian Boyter
 * Parts Copyright (c) 1991 by Paul King
 * Parts Copyright (c) 1994 by Brian V. Smith
 *
 * The X Consortium, and any party obtaining a copy of these files from
 * the X Consortium, directly or indirectly, is granted, free of charge, a
 * full and unrestricted irrevocable, world-wide, paid up, royalty-free,
 * nonexclusive right and license to deal in this software and
 * documentation files (the "Software"), including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software subject to the restriction stated
 * below, and to permit persons who receive copies from any such party to
 * do so, with the only requirement being that this copyright notice remain
 * intact.
 * This license includes without limitation a license to do the foregoing
 * actions under any patents of the party supplying this software to the 
 * X Consortium.
 *
 * Restriction: The GIF encoding routine "GIFencode" in f_wrgif.c may NOT
 * be included if xfig is to be sold, due to the patent held by Unisys Corp.
 * on the LZW compression algorithm.
 *
 */

#include "fig.h"
#include "resources.h"
#include "object.h"
#include "w_setup.h"

extern FILE	*open_picfile();
extern void	 close_picfile();

int	_read_pcx();

/* attempt to read an EPS file */

/* return codes:  PicSuccess (1) : success
		  FileInvalid (-2) : invalid file
*/

int
read_epsf(file,filetype,pic)
    FILE	   *file;
    int		    filetype;
    F_pic	   *pic;
{
    int		    nbitmap;
    Boolean	    bitmapz;
    Boolean	    foundbbx;
    Boolean	    nested;
    char	   *cp;
    unsigned char   *mp;
    unsigned int    hexnib;
    int		    flag;
    char	    buf[300];
    int		    llx, lly, urx, ury, bad_bbox;
    unsigned char  *last;
    float	    scale;
#ifdef GSBIT
    int		    status;
    FILE	   *pixfile, *gsfile;
    char	    pixnam[PATH_MAX],errnam[PATH_MAX],gscom[PATH_MAX];
#endif /* GSBIT */
    Boolean	    useGS;
    useGS = False;

    /* check if empty or no valid PostScript start */
    if (fgets(buf, 300, file) == NULL || strncasecmp(buf,"%!",2))
	return FileInvalid;

    llx=lly=urx=ury=0;
    foundbbx = nested = False;
    while (fgets(buf, 300, file) != NULL) {

	if (!nested && !strncmp(buf, "%%BoundingBox:", 14)) {	/* look for the bounding box */
	   if (!strstr(buf,"(atend)")) {		/* make sure doesn't say (atend) */
		float rllx, rlly, rurx, rury;

		if (sscanf(strchr(buf,':')+1,"%f %f %f %f",&rllx,&rlly,&rurx,&rury) < 4) {
		    file_msg("Bad EPS file: %s", pic->file);
		    return FileInvalid;
	    	}
		foundbbx = True;
	   	llx = round(rllx);
	    	lly = round(rlly);
	    	urx = round(rurx);
		ury = round(rury);
#ifdef EPS_ROT_FIXED
		/* swap notion of lower-left/upper-right when in landscape mode */
		if (appres.landscape) {
		    register int tmp;
		    tmp = llx; llx = lly ; lly = tmp;
		    tmp = urx; urx = ury ; ury = tmp;
		}
#endif
		break;
	    }
	} else if (!strncmp(buf, "%%Begin", 7)) {
	    ++nested;
	} else if (nested && !strncmp(buf, "%%End", 5)) {
	    --nested;
	}
    }
    if (!foundbbx) {
	file_msg("No bounding box found in EPS file");
	return FileInvalid;
    }

    if ((urx - llx) == 0) {
	file_msg("Bad or missing EPS bounding box in file: %s", pic->file);
	return FileInvalid;
    }
    pic->hw_ratio = (float) (ury - lly) / (float) (urx - llx);
    /* make scale factor larger for metric */
    scale = (appres.INCHES ? (float)PIX_PER_INCH : 2.54*PIX_PER_CM)/72.0;
    pic->size_x = round((urx - llx) * scale);
    pic->size_y = round((ury - lly) * scale);
    /* make 2-entry colormap here if we use monochrome */
    pic->cmap[0].red = pic->cmap[0].green = pic->cmap[0].blue = 0;
    pic->cmap[1].red = pic->cmap[1].green = pic->cmap[1].blue = 255;
    pic->numcols = 0;

    if ( bad_bbox = ( urx <= llx || ury <= lly ) ) {
	file_msg("Bad values in EPS bounding box");
	return FileInvalid;
    }
    bitmapz = False;

    /* look for a preview bitmap */
    while (fgets(buf, 300, file) != NULL) {
	lower(buf);
	if (!strncmp(buf, "%%beginpreview", 14)) {
	    sscanf(buf, "%%%%beginpreview: %d %d %*d",
		   &pic->bit_size.x, &pic->bit_size.y);
	    bitmapz = True;
	    break;
	}
    }
#ifdef GSBIT
	/* if monochrome and a preview bitmap exists, don't use gs */
	if ((!appres.monochrome || !bitmapz) && !bad_bbox) {
	    int   wid,ht;
	    char *driver;

	    wid = urx - llx + 1;
	    ht  = ury - lly + 1;

/* See the Imakefile about PCXBUG */

#ifdef PCXBUG
	    /* width must be even for pcx format with Aladdin Ghostscript < 3.32 */
	    if ((wid & 1) != 0)
		wid++;
#endif
	    /* make name /TMPDIR/xfig-pic.pix */
	    sprintf(pixnam, "%s/%s%06d.pix", TMPDIR, "xfig-pic", getpid());
	    /* and file name for any error messages from gs */
	    sprintf(errnam, "%s/%s%06d.err", TMPDIR, "xfig-pic", getpid());
	    /* generate gs command line */
	    /* for monchrome, use pbm */
	    if (tool_cells <= 2 || appres.monochrome) {
		/* monochrome output */
		driver = "pbmraw";
	    } else {
		/* for color, use pcx */
		driver = "pcx256";
	    }
	    sprintf(gscom,
		   "gs -r72x72 -dSAFER -sDEVICE=%s -g%dx%d -sOutputFile=%s -q - > %s 2>&1",
		    driver, wid, ht, pixnam, errnam);
	    if ((gsfile = popen(gscom,"w" )) != 0) {
		pic->bit_size.x = wid;
		pic->bit_size.y = ht;
		bitmapz = True;
		useGS = True;
	    } else {
		file_msg("Cannot open pipe with command: %s\n",gscom);
	    }
	}
#endif /* GSBIT */
    if (!bitmapz) {
	file_msg("EPS object read OK, but no preview bitmap found/generated");
#ifdef GSBIT
	if ( useGS )
	    pclose( gsfile );
#endif /* GSBIT */
	return PicSuccess;
    } else if ( pic->bit_size.x <= 0 || pic->bit_size.y <= 0 ) {
	file_msg("Strange bounding-box/bitmap-size error, no bitmap found/generated");
#ifdef GSBIT
	if ( useGS )
	    pclose( gsfile );
#endif /* GSBIT */
	return FileInvalid;
    } else {
	nbitmap = (pic->bit_size.x + 7) / 8 * pic->bit_size.y;
	pic->bitmap = (unsigned char *) malloc(nbitmap);
	if (pic->bitmap == NULL) {
	    file_msg("Could not allocate %d bytes of memory for EPS bitmap\n",
		     nbitmap);
#ifdef GSBIT
	    if ( useGS )
		pclose( gsfile );
#endif /* GSBIT */
	    return PicSuccess;
	}
    }
#ifdef GSBIT
    /* if GhostScript */
    if ( useGS ) {
	char	    tmpfile[PATH_MAX];
	FILE	   *tmpfp;
	char	   *psnam;

	psnam = pic->file;
	/* is the file a pipe? (This would mean that it is compressed) */
	tmpfile[0] = '\0';
	if (filetype == 1) {	/* yes, now we have to uncompress the file into a temp file */
	    /* re-open the pipe */
	    close_picfile(file, filetype);
	    file = open_picfile(psnam, &filetype);
	    sprintf(tmpfile, "%s/%s%06d", TMPDIR, "xfig-eps", getpid());
	    if ((tmpfp = fopen(tmpfile, "w")) == NULL) {
		file_msg("Couldn't open tmp file %s, %s", tmpfile, sys_errlist[errno]);
		return PicSuccess;
	    }
	    while (fgets(buf, 300, file) != NULL)
		fputs(buf, tmpfp);
	    fclose(tmpfp);
	    /* and use the tmp file */
	    psnam = tmpfile;
	}

	/*********************************************
	gs commands (New method)

	W is the width in pixels and H is the height
	gs -dSAFER -sDEVICE=pbmraw(or pcx256) -gWxH -sOutputFile=/tmp/xfig-pic%%%.pix -q -

	-llx -lly translate 
	/oldshowpage {showpage} bind def
	/showpage {} def
	(psfile) run
	oldshowpage
	quit
	*********************************************/

	fprintf(gsfile, "%d %d translate\n",-llx,-lly);
	fprintf(gsfile, "/oldshowpage {showpage} bind def\n");
	fprintf(gsfile, "/showpage {} def\n");
	fprintf(gsfile, "(%s) run\n", psnam);
	fprintf(gsfile, "oldshowpage\n");
	fprintf(gsfile, "quit\n");

	status = pclose(gsfile);
	if (tmpfile[0])
	    unlink(tmpfile);
	/* error return from ghostscript, look in error file */
	if (status != 0 || ( pixfile = fopen(pixnam,"r") ) == NULL ) {
	    FILE *errfile = fopen(errnam,"r");
	    file_msg("Could not parse EPS file with ghostscript: %s", pic->file);
	    if (errfile) {
		file_msg("ERROR from ghostscript:");
		while (fgets(buf, 300, errfile) != NULL) {
		    buf[strlen(buf)-1]='\0';	/* strip newlines */
		    file_msg("%s",buf);
		}
		fclose(errfile);
		unlink(errnam);
	    }
	    free((char *) pic->bitmap);
	    pic->bitmap = NULL;
	    unlink(pixnam);
	    return FileInvalid;
	}
	if (tool_cells <= 2 || appres.monochrome) {
		pic->numcols = 0;
		fgets(buf, 300, pixfile);
		/* skip any comments */
		/* the last line read is the image size */
		do
		    fgets(buf, 300, pixfile);
		while (buf[0] == '#');
		if ( fread(pic->bitmap,nbitmap,1,pixfile) != 1 ) {
		    file_msg("Error reading output (EPS problems?): %s", pixnam);
		    fclose(pixfile);
		    unlink(pixnam);
		    free((char *) pic->bitmap);
		    pic->bitmap = NULL;
		    return FileInvalid;
		}
	} else {
		FILE	*pcxfile;
		int	 filtyp;
		int	 wid,ht;

		/* now read the pcx file just produced by gs */
		/* don't need bitmap - _read_pcx() will allocate a new one */
		free((char *) pic->bitmap);
		pic->bitmap = NULL;
		/* save picture width/height because read_pcx will overwrite it */
		wid = pic->size_x;
		ht  = pic->size_y;
		pcxfile = open_picfile(pixnam, &filtyp);
		status = _read_pcx(pcxfile,filtyp,pic);
		close_picfile(pcxfile, filtyp);
		/* restore width/height */
		pic->size_x = wid;
		pic->size_y = ht;
		/* and type */
		pic->subtype = T_PIC_EPS;
		if (status != 1) {
		    file_msg("Error reading output from ghostscript (EPS problems?): %s",
			pixnam);
		    unlink(pixnam);
		    if (pic->bitmap)
			free((char *) pic->bitmap);
		    pic->bitmap = NULL;
		    return FileInvalid;
		}
	}
	fclose(pixfile);
	unlink(pixnam);
    }
#endif /* GSBIT */

    /* for whatever reason, ghostscript wasn't available or didn't work but there 
       is a preview bitmap - use that */
    if ( !useGS )
{
	mp = pic->bitmap;
	bzero((char*)mp, nbitmap);	/* init bitmap to zero */
	last = pic->bitmap + nbitmap;
	flag = True;
	while (fgets(buf, 300, file) != NULL && mp < last) {
	    lower(buf);
	    if (!strncmp(buf, "%%endpreview", 12) ||
		!strncmp(buf, "%%endimage", 10))
		    break;
	    cp = buf;
	    if (*cp != '%')
		break;
	    cp++;
	    while (*cp != '\0') {
		if (isxdigit(*cp)) {
		    hexnib = hex(*cp);
		    if (flag) {
			flag = False;
			*mp = hexnib << 4;
		    } else {
			flag = True;
			*mp = *mp + hexnib;
			mp++;
			if (mp >= last)
			    break;
		    }
		}
		cp++;
	    }
	}
    }
    return PicSuccess;
}

int
hex(c)
    char	    c;
{
    if (isdigit(c))
	return (c - 48);
    else
	return (c - 87);
}

lower(buf)
    char	   *buf;
{
    while (*buf) {
	if (isupper(*buf))
	    *buf = (char) tolower(*buf);
	buf++;
    }
}
