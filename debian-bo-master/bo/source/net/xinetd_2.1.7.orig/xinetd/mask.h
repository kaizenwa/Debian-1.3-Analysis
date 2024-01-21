/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

#ifndef MASK_H
#define MASK_H

/*
 * $Id: mask.h,v 1.1 1996/01/24 19:29:19 chuck Exp $
 */

/*
 * Macros about masks
 */

typedef unsigned mask_t ;

#define MASK_EMPTY								0
#define MASK_NULL									((mask_t *)0)

#define MASK( v )									( 1 << ( (v)-1 ) )

#define M_CLEAR_ALL( mask )					(mask) = 0
#define M_ASSIGN( mask1, mask2 )				(mask1) = (mask2)
#define M_ARE_ALL_CLEAR( mask )				( (mask) == 0 )
#define M_SET( mask, v )						(mask) |= MASK(v)
#define M_CLEAR( mask, v )						(mask) &= ~MASK(v)
#define M_IS_SET( mask, v )					( (mask) & MASK(v) )
#define M_IS_CLEAR( mask, v )					( ! M_IS_SET( mask, v ) )

#define M_AND( mres, m1, m2 )					( (mres) = (m1) & (m2) )
#define M_OR( mres, m1, m2 )					( (mres) = (m1) | (m2) )
#define M_XOR( mres, m1, m2 )					( (mres) = (m1) ^ (m2) )

#endif	/* MASK_H */
