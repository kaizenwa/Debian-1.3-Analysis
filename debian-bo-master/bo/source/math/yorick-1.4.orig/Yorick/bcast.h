/*
    BCAST.H
    Declare generic broadcast, scatter, and gather functions.

    $Id: bcast.h,v 1.1 1993/08/27 18:32:09 munro Exp $
 */
/*    Copyright (c) 1994.  The Regents of the University of California.
                    All rights reserved.  */

#ifndef BCAST_H
#define BCAST_H

/*--------------------------------------------------------------------------*/

#ifndef NOT_YORICK
#include "ydata.h"

/* Broadcast(dst, ddims, src, sdims, base)
   broadcasts the src array into the dst array (where both src
   and dst array elements are size bytes long), returning 0 on
   success, 1 if sdims is not L-conformable with ddims. */

extern int Broadcast(void *dst, const Dimension *ddims,
		     void *src, const Dimension *sdims, StructDef *base);

#else
/* Only Scatter and Gather functions are relevant to standalone
   binary file package.  */
#include "binio.h"
#endif

/*--------------------------------------------------------------------------*/

/* Scatter(src, dst, base, number, strider)
   scatters the src array into the dst array, given the strider for
   the dst array and the base StructDef for the objects to be scattered.
   If the strider is 0, the given number of contiguous objects is
   copied.
      x(index list)= expression      */

extern void Scatter(void *src, void *dst, StructDef *base, long number,
		    const Strider *strider);

/* Gather(dst, src, base, number, strider)
   gathers into the dst array from the src array, given the strider for
   the src array and the base StructDef for the objects to be gathered.
   If the strider is 0, the given number of contiguous objects is
   copied.
      ... evaluate x(index list) ...  */

extern void Gather(void *dst, void *src, StructDef *base, long number,
		   const Strider *strider);

/* Scatter and Gather have analogues YWrite and YRead when the data
   resides on an IOStream instead of in memory.  CastRead and CastWrite
   are low-level workers for YRead and YWrite which do not perform
   format conversions.  */
extern void CastWrite(void *src, long dst, StructDef *base, long number,
		      const Strider *strider);
extern void CastRead(void *dst, long src, StructDef *base, long number,
		     const Strider *strider);

/*--------------------------------------------------------------------------*/
#endif
