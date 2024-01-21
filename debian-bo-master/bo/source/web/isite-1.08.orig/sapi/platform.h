/*************************************************************************

Copyright Notice

Copyright (c) MCNC, Clearinghouse for Networked Information Discovery and
Retrieval, 1994. 

Permission to use, copy, modify, distribute, and sell this software and
its documentation, in whole or in part, for any purpose is hereby granted
without fee, provided that

1. The above copyright notice and this permission notice appear in all
copies of the software and related documentation. Notices of copyright
and/or attribution which appear at the beginning of any file included in
this distribution must remain intact. 

2. Users of this software agree to make their best efforts (a) to return
to MCNC any improvements or extensions that they make, so that these may
be included in future releases; and (b) to inform MCNC/CNIDR of noteworthy
uses of this software. 

3. The names of MCNC and Clearinghouse for Networked Information Discovery
and Retrieval may not be used in any advertising or publicity relating to
the software without the specific, prior written permission of MCNC/CNIDR. 

THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND,
EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY WARRANTY
OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. 

IN NO EVENT SHALL MCNC/CNIDR BE LIABLE FOR ANY SPECIAL, INCIDENTAL,
INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND, OR ANY DAMAGES WHATSOEVER
RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER OR NOT ADVISED OF THE
POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF LIABILITY, ARISING OUT OF OR
IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. 

*************************************************************************/

/*
      PLATFORM DEFINITIONS FILE, Version 0.05

      Include this file before any others.  It should be preceded only by
      _INC_ defines (such as PLATFORM_INC_MALLOC), if you wish to use them.

      #ifdef...		      Then...
	PLATFORM_MSC		Compiler is Microsoft C++/Visual C++.
	PLATFORM_BORLANDC	Compiler is Borland C++.
	PLATFORM_MSDOS		Operating system is MS-DOS.
	PLATFORM_WINDOWS	Windows API is available.

      If you have defined...  Then this file will...*
	PLATFORM_INC_MALLOC	Include .h file needed for calling malloc().

  * Note that your program must define these in your Makefile.

*/

#ifndef PLATFORM_H
#define PLATFORM_H

/* Compiler */

#ifdef _MSC_VER
#define PLATFORM_MSC
#endif
#ifdef __BORLANDC__
#define PLATFORM_BORLANDC
#endif

/* Operating system */

/* Under MSC */
#ifdef MSDOS
#define PLATFORM_MSDOS
#endif
/* Under Borland */
#ifdef __MSDOS__
#define PLATFORM_MSDOS
#endif

/* Windows */

/* Under MSC */
#ifdef _WINDOWS
#define PLATFORM_WINDOWS
#endif
/* Under Borland */
#ifdef _Windows
#define PLATFORM_WINDOWS
#endif

/* ANSI-like */

#ifdef PLATFORM_MSDOS
#define ANSI_LIKE
#endif

/* FAR */

#ifndef PLATFORM_MSDOS
#define FAR
#define PASCAL
#endif

/* User-defines */

#ifdef PLATFORM_INC_MALLOC
#ifdef PLATFORM_MSDOS
#ifdef PLATFORM_BORLANDC
#include "alloc.h"
#else
#include "malloc.h"
#endif
#else
#include <stdlib.h>
#endif
#endif

#endif
