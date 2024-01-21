/*$Id: md_const.h,v 11.23 96/02/21 12:46:23 al Exp $ -*- C++ -*-
 * A kluge for old compilers that don't do const correctly
 * Almost a no-op for good up-to-date compilers
 */
#if !defined(CONST)
  #if defined(NO_ABSTRACT_CONST) || defined(NoAbsConsT)
    #define mutable	/* a work around for old compilers */
    #define CONST	/* that don't do const correctly */
  #else
    #define CONST const
  #endif
#endif
