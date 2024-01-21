/*$Id: md.h,v 11.22 96/02/18 11:46:34 al Exp $ -*- C++ -*-
 * a dumb include for portability
 */
#ifndef MD_H
#define MD_H
/*--------------------------------------------------------------------------*/
#if defined(MSDOS)
#   include "md_msdos.h"
#elif defined(UNIX)
#   include "md_unix.h"
#elif defined(VMS)
#   include "md_vms.h"
#endif
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
