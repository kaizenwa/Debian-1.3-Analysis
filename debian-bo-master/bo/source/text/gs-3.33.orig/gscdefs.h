/* Copyright (C) 1994 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* gscdefs.h */
/* Prototypes for configuration definitions in gconfig.c. */

/* Miscellaneous system constants (read-only systemparams). */
/* They should all be const, but one application needs some of them */
/* to be writable.... */
extern long gs_buildtime;
extern const char *gs_copyright;
extern const char *gs_product;
extern long gs_revision;
extern long gs_revisiondate;
extern long gs_serialnumber;

/* Installation directories and files */
extern const char *gs_doc_directory;
extern const char *gs_lib_default_path;
extern const char *gs_init_file;
