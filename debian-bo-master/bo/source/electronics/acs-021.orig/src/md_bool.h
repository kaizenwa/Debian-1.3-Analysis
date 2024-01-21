/*$Id: md_bool.h,v 11.21 96/02/02 23:19:33 al Exp $ -*- C++ -*-
 * A kluge for old compilers that don't have bool
 * A no-op for good up-to-date compilers
 */
#ifndef MD_BOOL_H
#define MD_BOOL_H
#if defined(BAD_BOOL)
  enum {false=0, true=1};
  typedef int bool;
#endif
#endif



