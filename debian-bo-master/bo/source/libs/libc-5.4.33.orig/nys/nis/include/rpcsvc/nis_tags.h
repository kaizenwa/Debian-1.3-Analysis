/*
** nislib.h              NIS+ client access function definitions
**
** Copyright (c) 1993 Signum Support AB, Sweden
**
** This file is part of the NYS Library.
**
** The NYS Library is free software; you can redistribute it and/or
** modify it under the terms of the GNU Library General Public License as
** published by the Free Software Foundation; either version 2 of the
** License, or (at your option) any later version.
**
** The NYS Library is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
** Library General Public License for more details.
** 
** You should have received a copy of the GNU Library General Public
** License along with the NYS Library; see the file COPYING.LIB.  If
** not, write to the Free Software Foundation, Inc., 675 Mass Ave,
** Cambridge, MA 02139, USA.
**
** Author: Peter Eriksson <pen@signum.se>
*/

#ifndef __NISLIB_NIS_TAGS_H__
#define __NISLIB_NIS_TAGS_H__

#ifdef __cplusplus
extern "C" {
#endif
    
#define FOLLOW_LINKS	(1<<0)
#define FOLLOW_PATH	(1<<1)
#define HARD_LOOKUP	(1<<2)
#define ALL_RESULTS	(1<<3)
#define NO_CACHE	(1<<4)
#define MASTER_ONLY	(1<<5)
#define EXPAND_NAME	(1<<6)

#define RETURN_RESULT	(1<<7)
#define ADD_OVERWRITE	(1<<8)
#define REM_MULTIPLE	(1<<9)
#define MOD_SAMEOBJ	(1<<10)
#define ADD_RESERVED	(1<<11)
#define REM_RESERVED	(1<<12)
#define MOD_RESERVED	(1<<13)

#define USE_DGRAM	(1<<16)
#define NO_AUTHINFO	(1<<17)

#ifdef __cplusplus
}
#endif

#endif
