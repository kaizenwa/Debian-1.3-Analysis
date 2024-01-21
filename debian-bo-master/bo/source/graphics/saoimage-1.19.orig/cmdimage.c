#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	cmdimage.c (Command Image File)
 * Purpose:	Set options from command line
 * Subroutine:	parse_filename()			returns: int
 * Subroutine:	parse_filetype()			returns: int
 * Subroutione:	parse_fileread()			returns: int
 * Subroutione:	parse_connection()			returns: int
 * Xlib calls:	none
 * Copyright:	1994 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version	       	5  Jan 1989
 *              {1} MVH BSDonly strings.h compatability         19 Feb 1990
 *		{2} John Add Socket connection			8  May 1990
 *		{3} MVH removed isalpha test for file names	1  Jan 1991
 *		{4} MVH added + type switches and support	21 Jun 1991
 *		{5} Doug Mink add nimage			21 Oct 1994
 *		{6} Doug Mink add plane as synonym of nimage	18 Oct 1995
 *		{7} Doug Mink add zf and zoom for zoom factor    2 Jan 1996
 *		{8} Doug Mink close pipe if image display        9 Feb 1996
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* stderr, NULL, etc. */
#include <ctype.h>

#ifndef VMS
#ifdef SYSV
#include <string.h>
#else
#include <strings.h>		/* strlen, etc. for unenlightened BSD's */
#endif
#else
#include <string.h>
#endif

#include <math.h>		/* for atof (see Ultrx atof man page) */
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/constant.h"	/* define codes */
#include "hfiles/struct.h"	/* declare structure types */
#include "hfiles/extern.h"	/* extern main parameter structures */
#include "hfiles/cmdparse.h"	/* define parse status bits */

/*
 * Subroutine:	parse_filename
 * Purpose:	Parse commandline to find an image file name
 */
int parse_filename ( argc, argv, argi, got_name )
     int argc;		/* total number of arg tokens */
     char *argv[];	/* array of arg tokens */
     int *argi;		/* first arg to check, (returned as last arg used) */
     int *got_name;	/* status word (use bit for found a file name) */
{
  int i;
  int usage();
  void close_imtool_connection();

  i = *argi;
  /* --- check for the filename as an unidentified string --- */
  if( (argv[i][0] != '-') ) {
    if( *got_name & CMD_FNAME ) {
      (void)fprintf(stderr, "More than 1 possible file name: %s %s.\n",
		    img.filename, argv[i]);
      return( usage("filename", argc, argv, i, i) );
    } else {
      img.filename = argv[i];
      *got_name |= CMD_FNAME;
    }
    /* --- Close imtool pipe --- */
    control.IRAF_in.open = 0;
    control.IRAF_in.func = NULL;
    return( 1 );
  }
  /* --- check for filename as an identified string --- */
  if( (strcmp(argv[i], "-name") == 0) ||
      (strcmp(argv[i], "-") == 0) ) {
    if( ++i >= argc )
      return( usage("filename", argc, argv, i-1, i) );
    if( *got_name & CMD_FNAME ) {
      (void)fprintf(stderr, "More than 1 possible file name: %s %s.\n",
		    img.filename, argv[i]);
      return( usage("filename", argc, argv, i, i) );
    } else {
      img.filename = argv[i];
      *got_name |= CMD_FNAME;
    }
    *argi = i;
    return( 1 );
  }
  return( 0 );
}

/*
 * Subroutine:	parse_filetype
 * Purpose:	Parse command line for image file type
 */
int parse_filetype ( argc, argv, argi, got_type )
     int argc;		/* total number of arg tokens */
     char *argv[];	/* array of arg tokens */
     int *argi;		/* first arg to check, (returned as last arg used) */
     int *got_type;	/* status word (bit for got an image type switch) */
{
  int i;

  i = *argi;
  /* --- IMAGE FILE TYPE --- */
  if( strcmp(argv[i], "-iraf") == 0 ) {
    /* --- IRAF image --- */
    img.file_type = SOP_IRAF;
  } else if( strcmp(argv[i], "-oif") == 0 ) {
    /* --- Iraf OIF .imh headed image file --- */
    img.file_type = SOP_IRAF;
  } else if( strcmp(argv[i], "-fits") == 0 ) {
    /* --- FITS image --- */
    img.file_type = SOP_FITS;
  } else if( strcmp(argv[i], "-dfits") == 0 ) {
    /* --- FITS image in non-standard byte swap order --- */
    img.file_type = SOP_FITS;
    img.byte_swap = !img.byte_swap;
  } else if( strcmp(argv[i], "-saoccd") == 0 ) {
    /* --- SAOCCD image --- */
    img.file_type = SOP_SAOCCD;
  } else if( strcmp(argv[i], "-ros") == 0 ) {
    /* --- ROSAT HOPR HRI image --- */
    img.file_type = SOP_ROSAT;
  } else {
    /* --- check array type specification --- */
    if( (strcmp(argv[i], "-chararray") == 0) ||
        (strcmp(argv[i], "-u1" ) == 0) ) {
      img.file_type = SOP_Array;
      img.storage_type = ARR_U1;
      img.bytepix = 1;
    } else if( (strcmp(argv[i], "-shortarray") == 0) ||
	       (strcmp(argv[i], "-i2" ) == 0) ) {
      img.file_type = SOP_Array;
      img.storage_type = ARR_I2;
      img.bytepix = 2;
    } else if( (strcmp(argv[i], "-ushortarray") == 0) ||
	       (strcmp(argv[i], "-u2" ) == 0) ) {
      img.file_type = SOP_Array;
      img.storage_type = ARR_U2;
      img.bytepix = 2;
    } else if( (strcmp(argv[i], "-longarray") == 0) ||
	       (strcmp(argv[i], "-i4" ) == 0) ) {
      img.file_type = SOP_Array;
      img.storage_type = ARR_I4;
      img.bytepix = 4;
    } else if( (strcmp(argv[i], "-floatarray") == 0) ||
	       (strcmp(argv[i], "-r4" ) == 0) ) {
      img.file_type = SOP_Array;
      img.storage_type = ARR_R4;
      img.bytepix = 4;
    } else if( (strcmp(argv[i], "-doublearray") == 0) ||
	       (strcmp(argv[i], "-r8" ) == 0) ) {
      img.file_type = SOP_Array;
      img.storage_type = ARR_R8;
      img.bytepix = 8;
    } else {
      /* --- got here if no match of any type --- */
      return( 0 );
    }
    /* --- got here by finding an array type identifier --- */
    /* if there are more arguments and the first does not start with '-' */
    if( (argc > (i+2)) && (argv[i+1][0] != '-') && isdigit(argv[i+1][0]) ) {
      /* --- if giving args like this, 2 must be given --- */
      if( ((i+2) >= argc) ||
	  ((img.filecols = atoi (argv[++i])) <= 0) ||
	  ((img.filerows = atoi (argv[++i])) <= 0) )
	return( usage("array size", argc, argv, i-3, i) );
    }
  }
  /* --- got here after finding a match --- */
  *got_type |= CMD_FTYPE;
  *argi = i;
  return( 1 );
}

/*
 * Subroutine:	parse_fileread
 * Purpose:	Parse command line for image file reading parameters
 */
int parse_fileread ( argc, argv, argi, got_read )
     int argc;		/* total number of arg tokens */
     char *argv[];	/* array of arg tokens */
     int *argi;		/* first arg to check, (returned as last arg used) */
     int *got_read;	/* status word (bit that there were read parameters) */
{
  int i;
  float zoom;
  void set_tdisp();

  i = *argi;
  if( (strcmp(argv[i], "-sk") == 0) ||
      (strcmp(argv[i], "-skip") == 0) ||
      (strcmp(argv[i], "-header") == 0) ) {
    /* --- skip header of given size --- */
    if( (++i >= argc) || ((img.headersize = atoi(argv[i])) < 0) )
      return( usage("header size", argc, argv, i-1, i) );
  } else if( (strcmp(argv[i], "-bswap") == 0) ||
	     (strcmp(argv[i], "-byteswap") == 0) ) {
    /* --- selecting byteswap gets reverse byteswap of whatever it was --- */
    img.byte_swap = !img.byte_swap;
  } else if( (strcmp(argv[i], "-sb") == 0) ||
	     (strcmp(argv[i], "-scalebias") == 0) ) {
    /* --- scale and bias for image file data --- */
    double scale;
    float bias;

    if( (i + 2) >= argc )
      return( usage("args for scale and bias", argc, argv, i, i) );
    if( (scale = atof(argv[++i])) <= 0.0 )
      return( usage("value for scale", argc, argv, i-1, i) );
    if( sscanf(argv[++i], "%f", &bias) != 1 )
      return( usage("value for bias", argc, argv, i-2, i) );
    /* store values and flag if they represent remapping of the values */
    if( ((img.fscale = scale) != 1.0) ||
        ((img.fbias = (double)bias) != 0.0) )
      img.fscaled = 1;
    else
      img.fscaled = 0;
  } else if( (strcmp(argv[i], "-dc") == 0) ||
	     (strcmp(argv[i], "-dispcen") == 0) ) {
    /* --- initial display location --- */
    if( ((i+2) >= argc) ||
        (img.fdcenX = atof(argv[++i]) <= 0.0) ||
        (img.fdcenY = atof(argv[++i]) <= 0.0) )
      return( usage("display center", argc, argv, i-3, i) );
    if( (argc >= (i+1)) && (argv[i+1][0] != '-') && isdigit(argv[i+1][0]) ) {
      if( (img.fdblock = atoi(argv[++i]) == 0) )
	return( usage("display blocking", argc, argv, i-1, i) );
    }

  } else if( (strcmp(argv[i], "-zf") == 0) ||
	     (strcmp(argv[i], "-zoom") == 0) ) {

    /* --- initial zoom factor --- */
    if( (++i >= argc) || ((zoom = atof(argv[i])) <= 0) )
      return( usage("zoom factor", argc, argv, i-1, i) );

    /* check against the limits before proceeding */
    if( (0.01 > zoom) || (zoom > 400.0) ) {
      (void)fprintf(stderr, "WARNING: Attempt to exceed zooming limits!\n");
      img.zoom = 0.0;
    }

     /* tid.zoom = zoom but force exact integer alignment (int or 1/int) */
    else if( zoom > 0.75 )
      img.zoom = (double)((int)(zoom + 0.5));
    else
      img.zoom = 1.0 / (double)((int)((1.0 / zoom) + 0.5));

  } else if( (strcmp(argv[i], "-nim") == 0) ||
	     (strcmp(argv[i], "-nimage") == 0) ||
	     (strcmp(argv[i], "-plane") == 0) ) {
    /* --- sequence number of image to display (1=first) --- */
    if( (++i >= argc) || ((img.nimage = atoi(argv[i])) <= 0) )
      return( usage("image number", argc, argv, i-1, i) );

  } else if( (strcmp(argv[i], "-buf") == 0) ||
	     (strcmp(argv[i], "-bufmax") == 0) ) {
    /* --- maximum buffer array size (1 side of square) --- */
    if( (++i >= argc) || ((img.bufmax = atoi(argv[i])) <= 0) )
      return( usage("buffer limit", argc, argv, i-1, i) );
  } else if( strcmp(argv[i], "-fbconfig") == 0 ) {
    /* --- iraf display configuration --- */
    if( (++i >= argc) || ((img.fbconfig = atoi(argv[i])) == 0) )
      return( usage("fbconfig", argc, argv, i-1, i) );
  } else {
    return( 0 );
  }
  *got_read |= CMD_FREAD;
  *argi = i;
  return( 1 );
}

/*
 * Subroutine:	parse_connection
 * Purpose:	Parse command line for socket or pipe connection stuff
 */
int parse_connection ( argc, argv, argi, got_read, init )
     int argc;		/* i: total number of arg tokens */
     char *argv[];	/* i: array of arg tokens */
     int *argi;		/* i/o: first arg to check, (return last arg used) */
     int *got_read;	/* o: status word (bit for connection parameters) */
     int init;		/* i: this is init time */
{
  int i;
  void open_imtool_connection(), close_imtool_connection();
  void rename_imtool_connection();
#ifdef REALTIME
  void open_rtsock_connection();
  void open_rtfifo_connection();
#endif

  i = *argi;
  if( strcmp(argv[i], "-pros") == 0 ) {
    /* --- Imtool pipe with pros extensions --- */
    if( init ) {
      control.IRAF_in.open = 1;
      control.IRAF_in.func = open_imtool_connection;
    } else if( control.IRAF_in.open == 0 )
      open_imtool_connection();
    control.IRAF_out.protocol = IOP_PROS;
  } else if( strcmp(argv[i], "+imtool") == 0 ) {
    /* --- Imtool pipe --- */
    if( init ) {
      control.IRAF_in.open = 1;
      control.IRAF_in.func = open_imtool_connection;
    } else if( control.IRAF_in.open == 0 )
      open_imtool_connection();
    control.IRAF_out.protocol = IOP_Imtool;
  } else if( (strcmp(argv[i], "-imtool") == 0) ||
	     (strcmp(argv[i], "-imtool-") == 0) ) {
    /* --- Close imtool pipe --- */
    if( (init == 0) && (control.IRAF_in.open != 0) )
      close_imtool_connection();
    control.IRAF_in.open = 0;
    control.IRAF_in.func = NULL;
  } else if( strcmp(argv[i], "-idev") == 0 ) {
    /* --- iraf input pipe device --- */
    if( ++i >= argc )
      return( usage ("idev", argc, argv, i-1, i) );
    rename_imtool_connection(argv[i], 1);
  } else if( strcmp(argv[i], "-odev") == 0 ) {
    /* --- iraf output pipe device --- */
    if( ++i >= argc )
      return( usage("odev", argc, argv, i-1, i) );
    rename_imtool_connection(argv[i], 0);
#ifdef REALTIME
  } else if( strcmp(argv[i], "-rtsock") == 0 ) {
    /* --- RTIO Socket --- */
    si_init(NULL);
    if( init ) {
      control.aux_in.open = 1;
      control.aux_in.func =  open_rtsock_connection;
      if( (argc >= i) && (argv[i][0] != '-') && isdigit(argv[i][0]) ) {
		int j = 0;
		i++;
	    control.aux_in.name = NULL;
      } else 
	return( usage("-rtsock", argc, argv, i-1, i ) );
      control.aux_in.address = atoi(argv[i]);
    } else
      if( control.aux_in.open == 0 )
        open_rtsock_connection(&control.aux_in);
    control.aux_in.protocol = IOP_socket;
  } else if( strcmp(argv[i], "-rtfifo") == 0 ) {
    /* --- RTIO FIFO --- */
    si_init(NULL);
    if( init ) {
      control.aux_in.open = 1;
      control.aux_in.func =  open_rtfifo_connection;
      if( ++i >= argc ) {
        return( usage("odev", argc, argv, i-1, i) );
      } else {
	control.aux_in.name = argv[i];
      };
    } else 
      if( control.aux_in.open == 0 )
        open_rtfifo_connection(&control.aux_in);
#endif
  } else {
    return( 0 );
  }
  *got_read |= CMD_CONNECT;
  *argi = i;
  return( 1 );
}
