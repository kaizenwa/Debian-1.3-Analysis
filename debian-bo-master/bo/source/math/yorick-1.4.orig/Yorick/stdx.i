/*
    STDX.I
    Perform any post-initialization tasks.

    $Id: stdx.i,v 1.1 1993/08/27 18:32:09 munro Exp $

    When Yorick starts, std.i is included, then any pkg.i files for
    compiled-in packages, then this file stdx.i, and finally
    the user customization file custom.i.
    For now, the only thing which must be done here is the critical job
    changing from the startup search path YORICK_PATH to the normal
    include file search path (which, among other things, allows custom.i
    to be found).  This path can be overridden from custom.i, which
    runs after this.
*/
/*    Copyright (c) 1994.  The Regents of the University of California.
                    All rights reserved.  */

set_path;   /* set compiled-in default include path */
