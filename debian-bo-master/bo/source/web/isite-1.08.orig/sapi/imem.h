/*@@@
File:           imem.h
Version:        1.01.01
Description:    Memory allocation and freeing for UNIX/DOS-Windows
Author:         Nassib Nassar, nrn@cnidr.org
@@@*/

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
	To enable the memory debugging and auto-cleanup features, compile
	with IMEM_AUTO defined.
*/

#ifndef IMEM_H
#define IMEM_H

#include "platform.h"

#ifdef PLATFORM_WINDOWS
#include <windows.h>
#else
#define DWORD unsigned int
#endif

#ifndef LPSTR
#define LPSTR char *
#endif
 
/* Public function prototypes */

char FAR* imem_malloc(DWORD size);
char FAR* imem_calloc(DWORD nelem, DWORD elsize);
char FAR* imem_realloc(void FAR* ptr, DWORD size);
void imem_free(void FAR* ptr);
#ifdef IMEM_AUTO
void imem_FreeAll(void);
void imem_LogPtrList(char* FileName);
void imem_LogAllocSize(char* FileName);
#endif

#endif
