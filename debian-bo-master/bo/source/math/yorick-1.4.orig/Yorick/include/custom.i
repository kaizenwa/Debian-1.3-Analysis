/*
    CUSTOM.I
    Default version of user customization file --
    read after std.i and all package initalization files.

    $Id: custom.i,v 1.1 1993/08/27 18:50:06 munro Exp $
 */
/*    Copyright (c) 1994.  The Regents of the University of California.
                    All rights reserved.  */

/* Place your own customizations here.

   Be careful!  You can break Yorick in a personalized way, so that only
   you will be affected -- this makes it difficult to get anyone else to
   believe there is a problem!

   Examples:

   // I always need the distribution Bessel functions.
   #include "bessel.i"

   // Read in my_special_functions.i (in ~/Yorick), which I always need.
   #include "my_special_functions.i"

   // Use ugly boxed graphics and waste screen real estate by default.
   pldefault, style="boxed.gs", dpi=100;

 */

/* This should be the final line in your custom.i file-- it implements
   the default command line processing (see help for process_argv).  */
command_line= process_argv();
