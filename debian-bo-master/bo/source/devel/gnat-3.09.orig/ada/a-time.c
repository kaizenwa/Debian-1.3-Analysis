/****************************************************************************/
/*                                                                          */
/*                         GNAT COMPILER COMPONENTS                         */
/*                                                                          */
/*                              A - T I M E                                 */
/*                                                                          */
/*                          C Implementation File                           */
/*                                                                          */
/*                            $Revision: 1.4 $                              */
/*                                                                          */
/*    Copyright (C) 1992, 1993, 1994, 1995 Free Software Foundation, Inc.   */
/*                                                                          */
/* GNAT is free software;  you can  redistribute it  and/or modify it under */
/* terms of the  GNU General Public License as published  by the Free Soft- */
/* ware  Foundation;  either version 2,  or (at your option) any later ver- */
/* sion.  GNAT is distributed in the hope that it will be useful, but WITH- */
/* OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY */
/* or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License */
/* for  more details.  You should have  received  a copy of the GNU General */
/* Public License  distributed with GNAT;  see file COPYING.  If not, write */
/* to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, */
/* MA 02111-1307, USA.                                                      */
/*                                                                          */
/* As a  special  exception,  if you  link  this file  with other  files to */
/* produce an executable,  this file does not by itself cause the resulting */
/* executable to be covered by the GNU General Public License. This except- */
/* ion does not  however invalidate  any other reasons  why the  executable */
/* file might be covered by the  GNU Public License.                        */
/*                                                                          */
/* GNAT was originally developed  by the GNAT team at  New York University. */
/* It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). */
/*                                                                          */
/****************************************************************************/

/* This file contains system dependent symbols that are referenced in the
   GNAT Run Time Library */

#ifdef __vxworks
#include <types/vxTypesOld.h>
#endif

#include <time.h>

/* Return the value of the "time" C library function.  We always return
   a long and do it this way to avoid problems with not knowing
   what time_t is on the target.  */

long
gnat_time ()
{
  return time (0);
}
