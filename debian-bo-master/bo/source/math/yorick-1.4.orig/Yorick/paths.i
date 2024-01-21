/*
   PATHS.I

   $Id: paths.i,v 1.1 1993/08/27 18:32:09 munro Exp $

   When Yorick starts, it must be able to find several standard include
   files which initialize the interpreter.  The default paths to these
   files are established when Yorick is compiled (see the Maketmpl and
   Makefile files in the Yorick distribution).  However, if the Yorick
   binary is moved to a new site, the absolute path names of these
   required startup files may change.  By editing this file and placing it
   in the same directory as the Yorick executable file, you can adjust for
   such path name changes, which makes it possible to port Yorick binaries.

   Yorick "knows" two crucial paths when it starts:

      Y_LAUNCH     the directory containing the executable file
                   which is now running.  This may be Yorick itself,
		   or a custom version of Yorick.  On UNIX systems,
		   Yorick will not be fooled by an "executable"
		   which is a soft link to a file in another
		   directory; the directory containing the actual
		   executable file will be found.

      Y_SITE       the directory containing the standard startup
                   files Yorick needs to be able to find.  The name
		   of this directory is compiled into Yorick (see
		   comments in Makefile, Maketmpl, and codger.c).

   The Y_SITE directory may not be correct, if this program is not
   at the site where it was compiled.  At startup, Yorick's (temporary)
   search path for include files is:

      Y_LAUNCH : Y_SITE : Y_SITE/contrib

   This file, paths.i, is the first file included.  Ordinarily, paths.i
   will be in Y_SITE, but if it is not (because the Y_SITE directory does
   not exist or is not writable by the proprietor of Yorick at this site),
   paths.i may alter the initial search path by calling the interpreted
   function set_site, which sets the interpreted variables Y_LAUNCH and
   Y_SITE, as well as setting the startup search path.

   These same considerations apply when porting the binary executable
   for any Yorick-based code -- the portion of Y_SITE required for startup
   must also be moved to the new site, and either placed directly in
   Y_LAUNCH, or an appropriate paths.i placed in Y_LAUNCH.

   A related consideration for custom versions of Yorick is the stdx.i
   file.  Just as paths.i is the first file to be included when Yorick
   starts, stdx.i is the last file which must be included for Yorick to
   start properly.  It resets the search path for include files to its
   "final" value, which is by default:

      . : ~/Yorick : Y_LAUNCH/include : Y_SITE/include : Y_SITE/contrib

   If a custom version overrides this default by placing a stdx.i in
   Y_LAUNCH, then that special stdx.i might need revision when that
   custom version of Yorick is moved to a new site.  I believe there is
   no reason that a custom Yorick should ever need a special stdx.i.

   ----------------------------------------------------------------------

   The Gist style sheets, palettes, and PostScript template must lie
   in a place known to the Gist library routines in order for Yorick's
   graphics functions to work.  If the Gist library was compiled with
   an incorrect value of GISTPATH, set the variable GISTPATH to the
   correct value below.

   The hcp_out command invokes the gist CGM browser and pipes its
   PostScript output into lpr.  If the binaries have been ported, gist's
   compiled-in GISTPATH may not be correct.  Furthermore, gist itself may
   not be on the user's execution path.  You may be able to remedy these
   defects by setting the variable GIST_FORMAT here.  The variable
   LPR_FORMAT provides a similar service for PostScript files generated
   directly by Yorick.
 */
/*    Copyright (c) 1994.  The Regents of the University of California.
                    All rights reserved.  */

/* ------------------------------------------------------------------------ */

extern Y_SITE, Y_LAUNCH;
/* DOCUMENT Y_LAUNCH       the directory containing the Yorick executable
            Y_SITE         Yorick's "site directory"
     Y_LAUNCH is set by compiled code when Yorick starts and should never
              be modified.
     Y_SITE is set to a default built-in value by compiled code at startup,
              but may be modified in the file Y_LAUNCH/paths.i to allow for
	      ports of Yorick binary executables.
 */

/* set_site, "YORICK_SITE_DIR"; */

/* ------------------------------------------------------------------------ */

/* The first component of the GISTPATH should always be ~/Gist
   -- you need this line only if the value compiled into the Gist library
      libgist.a is incorrect.  */
/* GISTPATH= "~/Gist:"+"GIST_SITE_DIR"; */
GISTPATH= "~/Gist:"+Y_SITE+"gist";

extern GIST_FORMAT, LPR_FORMAT;
/* DOCUMENT GIST_FORMAT
     is used by the hcp_out function to generate the system call which
     invokes the gist CGM browser and pipes its output to lpr.  This
     format should contain a single %s specification; after this %s is
     replaced by the name of the CGM file, Yorick invokes the system
     command on the resulting string.

     LPR_FORMAT is also used by hcp_out to process PostScript files
     made directly by Yorick.

     The default values are:

        GIST_FORMAT= "gist %s -f | lpr";
	LPR_FORMAT= "lpr %s";

   SEE ALSO: hcp_out
 */

GIST_FORMAT= /*GIST_HOME_DIR*/ "gist %s -f | lpr";
LPR_FORMAT= "lpr %s";

/* Here is how to remedy (1) gist not being on the user's execution path
   and (2) gist being compiled at a different site with the wrong
   compiled-in GISTPATH:
GIST_FORMAT= "env GISTPATH=/env/Gist /usr/local/Gist/gist/gist %s -f | lpr";
 */

/* ------------------------------------------------------------------------ */
