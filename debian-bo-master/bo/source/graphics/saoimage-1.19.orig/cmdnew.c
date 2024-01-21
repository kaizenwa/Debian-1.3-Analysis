#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	cmdnew.c (Command New)
 * Purpose:	Orchestrate remembering and forgeting commandline arguments
 * Subroutine:	init_cmdline()			return: void
 * Subroutine:	get_new_cmd()			return: void
 * Xlib calls:	none
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version	       9 January 1989
 *		{1} Jay Travisano (STScI)  VMS changes		  17 Nov 1989
 *              {2} MVH BSDonly strings.h compatability           19 Feb 1990
 *		{3} MVH separate remote IO from file IO		 9 March 1990
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* stderr, NULL, etc. */

#ifndef VMS
#ifdef SYSV
#include <string.h>
#else
#include <strings.h>		/* strlen, etc. for unenlightened BSD's */
#endif
#else
#include <string.h>
#endif

#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/define.h"	/* define SZ_LINE, SZ_FNAME, etc */
#include "hfiles/struct.h"	/* declare structure types */
#include "hfiles/extern.h"	/* extern main parameter structures */
#include "hfiles/constant.h"	/* define codes */
#include "hfiles/cmdparse.h"	/* define parse status bits */
#include "hfiles/edit.h"

#ifdef VMS
#define close_disk close_pipe
#endif

static char input_line[SZ_LINE];
static char fname[SZ_FNAME];

/*
 * Subroutine:	init_cmdline
 * Purpose:	Save the original command line in a static string for use
 *		as an initial command line by get_new_command
 * Called by:	parse_cmdline() in CmdParse.c
 */
void init_cmdline ( argc, argv )
     int argc;
     char *argv[];
{
  void string_cmdline();

  string_cmdline (argc, argv, input_line, SZ_LINE);
}

static EditStruct *cmd_edit;
static char *prompt = "Enter new image file or command line:";
/*
 * Subroutine:	get_new_cmd
 * Purpose:	Enter a new command line and respond to it.
 * Called by:	key_response() in MainKey.c
 * Called by:	select_environment() in MainSelect.c
 */
void get_new_cmd ( )
{
  static int new_command();
  int get_edit_input();
  EditStruct *init_edit_popup();

  if( cmd_edit == NULL )
    cmd_edit = init_edit_popup(input_line, SZ_LINE);
  if( (get_edit_input(cmd_edit, 0, 1, 1, prompt) <= 0) ||
     (cmd_edit->char_cnt == 0) )
    return;
  (void)strncpy(input_line, cmd_edit->string, cmd_edit->char_cnt);
  input_line[cmd_edit->char_cnt] = '\0';
  /* parse line and respond */
  (void)new_command (input_line);
}

/*
 * Subroutine:	new_command
 * Purpose:	Given a new command line, parse it and do what is called for
 * Returns:	1 = success, 0 = user decided not to do anything, -1 = error
 */
static int new_command ( input_line )
     char *input_line;
{
  char **argv;
  int argc;
  int parse_status;
  int headersize;
  int parse_cmdline(), check_image();
  void reinit_color(), redraw_magnifier(), touch_submenu_button();
  static char **make_argv();
  static int new_file(), form_tokens();
  static void redo_displays(), clear_params(), reset_dispparams(), free_argv();

  /* store some key initial values */
  headersize = img.headersize;
  /* reinitialize user settings */
  clear_params();
  /* get space for listing tokens */
  argv = make_argv(20);
  argc = form_tokens(input_line, argv, 20);
  /* get values from the command line (uses extern.h) */
  parse_status = parse_cmdline(argc, argv, (char **)NULL);
  /* move filename to permanent string space, then free token space */
  if( (parse_status > 0) && (parse_status & CMD_FNAME) ) {
    (void)strcpy(fname, img.filename);
    img.filename = fname;
  }
  free_argv(argv, 20);
  if( parse_status < 0 )
    return( 0 );
  if( parse_status & CMD_COLOR ) {
    /* if requesting a different hardware configuration */
    /* do this work if it won't otherwise be done */
    if( (parse_status & (CMD_FNAME | CMD_FTYPE | CMD_FREAD |
			 CMD_ROTATE | CMD_SCALIM | CMD_SCALE)) == 0 )
      reinit_color(1);
    else
      reinit_color(0);
  }
  if( parse_status & (CMD_FNAME | CMD_FTYPE | CMD_FREAD) ) {
    if( check_image(&img, parse_status) < 0 ) {
      if( parse_status & CMD_COLOR )
	redo_displays();
    } else
      (void)new_file();
  } else {
    img.headersize = headersize;
    if( parse_status & (CMD_ROTATE | CMD_SCALIM) ) {
      reset_dispparams();
      (void)new_file();
    } else if( parse_status & CMD_SCALE ) {
      touch_submenu_button(SOP, color.scale.mode);
      redo_displays();
    } else if( parse_status & CMD_COLOR ) {
      /* redraw the magnibox display if color map was changed */
      redraw_magnifier();
    }
  }
  return( 1 );
}

/*
 * Subroutine:	redo_displays
 */
static void redo_displays ( )
{
  void new_scalemap(), map_panbox(), map_dispbox(), disp_panbox();
  void disp_dispbox(), redraw_magnifier();

  /* make the new lookup table */
  new_scalemap();
  /* refill display buffers with rescaled values and display */
  map_panbox();
  disp_panbox();
  /* panbox first because it is faster */
  map_dispbox();
  disp_dispbox();
  redraw_magnifier();
}

/*
 * Subroutine:	clear_params
 * Purpose:	Undo parameters checked for defaults when loading a new image
 */
static void clear_params ( )
{
  img.headersize = 0;
  img.byte_swap = 0;
  img.fdblock = 0;
  img.fdcenX = 0.0;
  img.fdcenY = 0.0;
  img.fiX1 = 0;
  img.fiX2 = 0;
  img.fiY1 = 0;
  img.fiY2 = 0;
}

/*
 * Subroutine:	reset_dispparams
 * Purpose:	Set the current display parameters for reloading the image
 */
static void reset_dispparams ( )
{
  float zoom;
  float fcenX, fcenY;
  void d_transform();

  if( img.fdcenX == 0.0 ) {
    /* calculate file coords of center of display */
    d_transform(&coord.imgtofile,
		(double)coord.id.cenX, (double)coord.id.cenY, &fcenX, &fcenY);
    img.fdcenX = (double)fcenX;
    img.fdcenY = (double)fcenY;
  }
  if( img.fdblock == 0.0 ) {
    if( coord.filetoimg.inx_outx != 0.0 )
      zoom = coord.filetoimg.inx_outx * coord.imgtodisp.inx_outx;
    else
      zoom = coord.filetoimg.iny_outx * coord.imgtodisp.inx_outx;
    /* take abs */
    if( zoom < 0.0 )
      zoom = -zoom;
    if( zoom < 1.0 )
      img.fdblock = (int)(-1.0/zoom);
    else
      img.fdblock = (int)zoom;
  }
}

/*
 * Subroutine:	make_argv
 * Purpose:	Allocate space used for commandline
 */
static char **make_argv ( maxargs )
     int maxargs;
{
  int i;
  char **argv;
  char *calloc_errchk();

  argv = (char **)calloc_errchk(maxargs, sizeof(char **), "Parse buffer");
  for( i = 0; i < maxargs; i++ )
    argv[i] = calloc_errchk(SZ_FNAME, sizeof(char), "Parse buffer");
  return( argv );
}

/*
 * Subroutine:	free_argv
 * Purpose:	Free the space used for parsing the command line
 */
static void free_argv ( argv, maxargs )
     int maxargs;
     char **argv;
{
  int i;
  for( i=0; i<maxargs; i++ )
    free(argv[i]);
  free( (char *)argv );
}

/*
 * Subroutine:	form_tokens
 * Purpose:	Break a command string into tokens (mimic initial command line)
 * Returns:	Count of tokens
 */
static int form_tokens ( input_string, argv, maxargs )
     char *input_string;
     char **argv;
     int maxargs;
{
  int i;
  char format[SZ_LINE];

  (void)strcpy(format, "%s");
  for( i=1; i<maxargs; i++ )
    (void)strncat(format, "%s", SZ_LINE);
  return( sscanf(input_string, format,
		 argv[0], argv[1], argv[2], argv[3], argv[4],
		 argv[5], argv[6], argv[7], argv[8], argv[9],
		 argv[10], argv[11], argv[12], argv[13], argv[14],
		 argv[15], argv[16], argv[17], argv[18], argv[19]) );
}

/*
 * Subroutine:	new_file
 * Purpose:	Having the image record set, do all that is needed to load
 *		a new image file
 */
static int new_file ( )
{
  int init_image(), init_imagebuf();
  void set_tdisp(), new_display(), new_panbox(), disp_panbox();

  /* get the image dimensions (need file name, type) */
  if( init_image() == 0 )
    return(0);
  /* resize the image buffer */
  (void)init_imagebuf();
  /* set display params and force reassessing and rescaling the image */
  set_tdisp(&coord);
  buffer.mm.img_leftX = coord.id.srcX1 + 1;
  /* read new image data */
  new_display(1, 1, 1, 1);
  /* replace the panbox with a new one */
  new_panbox(1);
  disp_panbox();
  return( 1 );
}
