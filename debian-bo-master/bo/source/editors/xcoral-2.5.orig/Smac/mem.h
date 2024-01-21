/* ########################################################################

				 mem.h

   File: mem.h
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/mem.h
   Description: 
   Created: Tue Feb 21 12:57:15 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:57:16 MET 1995
   Last maintained by: Bruno Pages

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   ########################################################################

   Copyright (c) : Bruno Pages

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   ######################################################################## */



#ifndef _mem_h
#define _mem_h

#include <stdlib.h>

#include "fctdecl.h"

extern FCT ( void *, Malloc,(int)	);
extern FCT ( void *, Calloc,(int, int)	);
extern FCT ( void , Free,(void *)	);
extern FCT ( char *, Strdup,(char *)	);

#ifndef RUNTIMECHECK
     
#define RTCMalloc	Malloc
#define RTCCalloc	Calloc
#define RTCStrdup	Strdup

#else

extern char * Address_Min;
extern char * Address_Char_Max;
extern char * Address_Int_Max;
extern char * Address_Pointer_Max;

extern FCT ( void, RTCFree,(void *)		);
extern FCT ( void *, RTCMalloc,(int)		);
extern FCT ( void *, RTCCalloc,(int, int)	);

extern FCT ( void, forbit_RTCfree,(void *)	);
     
extern FCT ( char *, init_rtcmalloc,(int)	);

/* strdup is not defined on all system (!!) */
     
extern FCT ( char *, stringdup,(char *)		);
     
#endif
  
#endif
