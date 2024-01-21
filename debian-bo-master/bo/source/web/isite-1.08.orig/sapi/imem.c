/*@@@
File:           imem.c
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

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include "imem.h"

/* Private variables */

#ifdef IMEM_AUTO
typedef struct tagAUTO {
	char FAR* ptr;
	DWORD size;
	struct tagAUTO FAR* next;
} AUTO;
typedef AUTO FAR* LPAUTO;
static LPAUTO AutoList;
static DWORD AutoCounter;
#endif

/* Private functions */

char FAR* AllocateMem(DWORD size, int Zero, int Realloc, char FAR* ptr);
#ifdef IMEM_AUTO
void AddToAutoList(char FAR* ptr, DWORD size);
void RemoveFromAutoList(char FAR* ptr);
#endif

/* Public functions */

char FAR* imem_malloc(DWORD size) {
	return AllocateMem(size, 0, 0, 0);
}

char FAR* imem_calloc(DWORD nelem, DWORD elsize) {
	return AllocateMem((nelem*elsize), 1, 0, 0);
}

char FAR* imem_realloc(void FAR* ptr, DWORD size) {
	return AllocateMem(size, 0, 1, (char FAR *)ptr);
}

void imem_free(void FAR* ptr) {
#ifdef PLATFORM_WINDOWS
	GlobalUnlock( (HGLOBAL)LOWORD(GlobalHandle(SELECTOROF(ptr))) );
	GlobalFree( (HGLOBAL)LOWORD(GlobalHandle(SELECTOROF(ptr))) );
#else
	free(ptr);
#endif
#ifdef IMEM_AUTO
	RemoveFromAutoList(ptr);
#endif
}

#ifdef IMEM_AUTO
void imem_FreeAll(void) {
	while (AutoList != NULL) {
		imem_free(AutoList->ptr);
	}
}
#endif

#ifdef IMEM_AUTO
void imem_LogPtrList(char* FileName) {
	FILE* fp = fopen(FileName, "a");
	if (fp != NULL) {
		LPAUTO p = AutoList;
		char FAR* t;
		int x, y;
		while (p != NULL) {
			fprintf(fp, "%ph (%u): [ ", p->ptr, p->size);
			y = 10;
			if (p->size < (DWORD)y)
				y = (int)p->size; /* OK since p->size small */
			t = (char FAR*)p->ptr;
			for (x = 1; x <= y; x++) {
				if (isprint(*t))
					fprintf(fp, "'%c' ", *t);
				else
					fprintf(fp, "%02Xh ", *t);
				t++;
			}
			if (p->size > 10)
				fprintf(fp, "... ");
			fprintf(fp, "]\n");
			p = p->next;
		}
		fprintf(fp, "-----------------------------------------------------------------------------\n");
		fprintf(fp, "Total allocated bytes = %u\n\n", AutoCounter);
		fclose(fp);
	}
}
#endif

#ifdef IMEM_AUTO
void imem_LogAllocSize(char* FileName) {
	FILE* fp = fopen(FileName, "a");
	if (fp != NULL) {
		fprintf(fp, "Total allocated bytes = %u\n\n", AutoCounter);
		fclose(fp);
	}
}
#endif

/* Private functions */

char FAR* AllocateMem(DWORD size, int Zero, int Realloc, char FAR* ptr) {
#ifdef PLATFORM_WINDOWS
	HGLOBAL hg;
	UINT fuAlloc;
	void FAR* p;
	if (Zero)
		fuAlloc = GMEM_MOVEABLE | GMEM_ZEROINIT;
	else
		fuAlloc = GMEM_MOVEABLE;
	if (Realloc)
		hg = GlobalReAlloc( \
			(HGLOBAL)LOWORD(GlobalHandle(SELECTOROF(ptr))), \
			(DWORD)size, fuAlloc);
	else
		hg = GlobalAlloc(fuAlloc, size);
	if (hg == (HGLOBAL)NULL)
		return NULL;
	p = GlobalLock(hg);
	if (p == NULL) {
		GlobalFree(hg);
		return NULL;
	}
#ifdef IMEM_AUTO
	if (Realloc)
		RemoveFromAutoList(ptr);
	AddToAutoList((char FAR*)p, size);
#endif
	return (char FAR*)p;
#else
	if (Realloc) {
		char* p = (char*)realloc(ptr, size);
#ifdef IMEM_AUTO
		if (p != NULL) {
			RemoveFromAutoList(ptr);
			AddToAutoList(p, size);
		}
#endif
		return p;
	} else {
		if (Zero) {
			char* p = (char*)calloc(1, size);
#ifdef IMEM_AUTO
			if (p != NULL)
				AddToAutoList(p, size);
#endif
			return p;
		} else {
			char* p = (char*)malloc(size);
#ifdef IMEM_AUTO
			if (p != NULL)
				AddToAutoList(p, size);
#endif
			return p;
		}
	}
#endif
}

#ifdef IMEM_AUTO
void AddToAutoList(char FAR* ptr, DWORD size) {
	LPAUTO N;
#ifdef PLATFORM_WINDOWS
	HGLOBAL hg;
	UINT fuAlloc;
	void FAR* p;
	fuAlloc = GMEM_MOVEABLE;
	hg = GlobalAlloc(fuAlloc, sizeof(AUTO));
	if (hg == (HGLOBAL)NULL)
		exit(-5);
	p = GlobalLock(hg);
	if (p == NULL) {
		GlobalFree(hg);
		exit(-6);
	}
	N = (LPAUTO)p;
#else
	N = (LPAUTO)malloc(sizeof(AUTO));
	if (N == NULL)
		exit(-5);
#endif
	N->ptr = ptr;
	N->size = size;
	N->next = AutoList;
	AutoList = N;
	AutoCounter += size;
}
#endif

#ifdef IMEM_AUTO
void RemoveFromAutoList(char FAR* ptr) {
	LPAUTO p = AutoList;
	LPAUTO op = NULL;
	while (p != NULL) {
		if (p->ptr == ptr) {
			LPAUTO t = p;
			p = p->next;
			AutoCounter -= t->size;
#ifdef PLATFORM_WINDOWS
			GlobalUnlock( (HGLOBAL)LOWORD(GlobalHandle(SELECTOROF(t))) );
			GlobalFree( (HGLOBAL)LOWORD(GlobalHandle(SELECTOROF(t))) );
#else
			free(t);
#endif
			if (op == NULL)
				AutoList = p;
			else
				op->next = p;
		} else {
			op = p;
			p = p->next;
		}
	}
}
#endif
