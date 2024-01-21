/* Definitions used in X utility programs.
   Copyright (C) 1993, 1994, 1995 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

extern void close_displays PARAMS ((void));
extern int write_entire_game_state PARAMS ((char *fname));
extern void low_init_error PARAMS ((char *str));
extern void low_init_warning PARAMS ((char *str));
extern void low_run_error PARAMS ((char *str));
extern void low_run_warning PARAMS ((char *str));
extern int keyword_code PARAMS ((char *str));
extern Image *read_xbm_file PARAMS ((char *filename, ImageFamily *imf,
				    readimf_hook hook));
extern Image *read_xpm_file PARAMS ((char *filename, ImageFamily *imf,
				    readimf_hook hook));
extern void parse_xpm_colors PARAMS ((char *name, int *r, int *g, int *b)); 
extern void write_xpm_file PARAMS ((FILE *fp, char *name, Image *img));
extern void write_x11_bitmaps PARAMS ((ImageFamily *imf, int mkfiles));
extern void write_xbm_file PARAMS ((FILE *fp, char *name, int cols, int rows,
				   char *data)); 
extern char *find_imf_name PARAMS ((char *rawname));
extern int read_any_file PARAMS ((char *filename, readimf_hook hook));
extern void reverse_rawdata PARAMS ((ImageFamily *imf));
