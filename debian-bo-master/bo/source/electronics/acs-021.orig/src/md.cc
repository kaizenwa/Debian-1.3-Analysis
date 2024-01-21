/*$Id: md.cc,v 11.22 96/02/18 11:44:18 al Exp $ -*- C++ -*-
 * a dumb selector for portability
 */
#include "io.h"
/*--------------------------------------------------------------------------*/
#if defined(MSDOS)
#   include "md_msdos.cc"
#elif defined(UNIX)
#   include "md_unix.cc"
#elif defined(VMS)
#   include "md_vms.cc"
#endif
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
