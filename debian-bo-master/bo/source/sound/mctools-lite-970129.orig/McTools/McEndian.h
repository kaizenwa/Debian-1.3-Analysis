/* Copyright (C) 1994 - 1996 
            Olav Woelfelschneider (wosch@rbg.informatik.th-darmstadt.de)

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public License as
  published by the Free Software Foundation; either version 2 of the
  License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this library; see the file COPYING.LIB.  If
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
*/

#ifndef _McEndian_h_
#define _McEndian_h_

#define SWAPW(w) ((((w)&0xFF)<<8)|(((w)>>8)&0xFF))
#define SWAPL(w) ((((w)&0xFF)<<24)|(((w)&0xFF00)<<8)|(((w)&0xFF0000)>>8)|(((w)>>24)))

#if !defined(USE_BIG_ENDIAN) && !defined(USE_LITTLE_ENDIAN)
#  include <endian.h>
#  if defined(BYTE_ORDER) && defined(BIG_ENDIAN) && defined(LITTLE_ENDIAN)
#    if BYTE_ORDER == BIG_ENDIAN
#      define USE_BIG_ENDIAN
#    else
#      if BYTE_ORDER == LITTLE_ENDIAN
#        define USE_LITTLE_ENDIAN
#      else
#        error Can not handle this endianess: "BYTE_ORDER"
#      endif
#    endif
#  endif
#endif

#ifdef USE_BIG_ENDIAN
#  ifdef USE_LITTLE_ENDIAN
#    error Both USE_LITTLE_ENDIAN and USE_BIG_ENDIAN are defined
#  endif
#  define LITTLEW(x) SWAPW(x)
#  define BIGW(x) (x)
#  define LITTLEL(x) SWAPL(x)
#  define BIGL(x) (x)
#else
#  ifndef USE_LITTLE_ENDIAN
#    error Neither USE_LITTLE_ENDIAN nor USE_BIG_ENDIAN is defined
#  endif
#  define LITTLEW(x) (x)
#  define BIGW(x) SWAPW(x)
#  define LITTLEL(x) (x)
#  define BIGL(x) SWAPL(x)
#endif

#endif /* _McEndian_h_ */
