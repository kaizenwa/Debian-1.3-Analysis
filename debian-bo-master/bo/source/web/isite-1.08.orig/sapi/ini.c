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
	ini.c

	N.R.Nassar@CNIDR.org
*/

#include <stdlib.h>
#include "imem.h"
#include "ini.h"
#ifdef INI_USE
#include <stdio.h>
#include <string.h>
#endif

#ifdef INI_USE
int ProfileFileInUse = 0;
int CachedFileChanged = 0;
CHR ProfileFileName[MAXPROFILESTR];
typedef struct tagPS {
        CHR S[MAXPROFILESTR];
        struct tagPS* Prev;
        struct tagPS* Next;
} PS;
PS* ProfileHead = NULL;  
PS* ProfileTail = NULL;
#endif

/* Private function prototypes */

#ifdef INI_USE
PS* FindProfileSection(PCHR pszSection);
PS* FindProfileEntry(PCHR pszEntry, PS* SecPtr);
int CleanupProfileString(PCHR pszString);
int InsertBlankLine(PS* Ptr);
#endif

/* Global functions */

/*
	int CacheProfileFile(PCHR pszFileName)

	Reads a file into memory so that subsequent profile file operations
	will not require excessive disk access.  After these operations you
	should call UncacheProfileFile() or AbortProfileChanges() to
	deallocate the cache memory buffer.

	pszFileName	Points to a null-terminated string that specifies the
			file name to open for caching.

	The return value is nonzero if the function is successful.  If another
	file is currently open for caching, this function will fail.
*/
int CacheProfileFile(PCHR pszFileName) {
#ifdef INI_USE
	PS* p;
	CHR s[MAXPROFILESTR];
	PCHR pc;
	FILE* ProfileFile;
	if (ProfileFileInUse)
		return 0;
	CachedFileChanged = 0;
	if (ProfileHead == NULL) {
		if (!(ProfileHead = (PS*)imem_calloc(1,sizeof(PS))))
			return 0;
		if (!(ProfileTail = (PS*)imem_calloc(1,sizeof(PS)))) {
			free(ProfileHead);
			ProfileHead = NULL;
			return 0;
		}
		ProfileHead->Next = ProfileTail;
		ProfileTail->Prev = ProfileHead;
		ProfileHead->Prev = NULL;
		ProfileTail->Next = NULL;
	}
	ProfileFileInUse = 1;
	strcpy(ProfileFileName, pszFileName);
	ProfileFile = fopen(pszFileName, "r");
	if (ProfileFile != NULL) {
		do {
			pc = fgets(s, MAXPROFILESTR-1, ProfileFile);
			if (pc != NULL) {
				if (!(p = (PS*)imem_calloc(1,sizeof(PS)))) {
					fclose(ProfileFile);
					return 0;
				}
				strcpy(p->S, s);
				p->Prev = ProfileTail->Prev;
				p->Next = ProfileTail;
				p->Prev->Next = p;
				p->Next->Prev = p;
			}
		} while (pc != NULL);
	}
/* else {
		ProfileFileInUse = 0;
		return 0;
	}
*/
	fclose(ProfileFile);
	return 1;
#else
	return 1;
#endif
}

/*
	int UncacheProfileFile()

	Commits to disk any profile changes made since CacheProfileFile() was 
	called.

	The return value is nonzero if the function is successful.  If it is
	not successful due to a disk error, you can retry or call 
	AbortProfileChanges() to deallocate the cache without saving changes
	to disk.
*/
int UncacheProfileFile() {
#ifdef INI_USE
	if (FlushProfileCache() == 0)
		return 0;
	if (AbortProfileChanges() == 0)
		return 0;
	return 1;
#else
	return 1;
#endif
}

/*
	int FlushProfileCache()

	Writes to disk the cached profile settings if they have changed.
	Unlike UncacheProfileFile() this function does not deallocate the
	cache buffer; rather it remains active.

	The return value is nonzero if the function is successful.
*/
int FlushProfileCache() {
#ifdef INI_USE
	PS* p;
	FILE* ProfileFile;
	int Done = 0;
	if (!ProfileFileInUse)
		return 0;
	if (!CachedFileChanged)
		return 1;
	ProfileFile = fopen(ProfileFileName, "w");
	if (ProfileFile == NULL)
		return 0;
	p = ProfileHead->Next;
	while ( (p->Next != NULL) && ( Done == 0) ) {
		if ( (p->S[0] == '\n') || (p->S[0] == '\0') ) {
			PS* a;
			int trailing = 1;
			a = p->Next;
			while ( (a->Next != NULL) && (trailing == 1) ) {
				if ( (a->S[0] != '\n') && (a->S[0] != '\0') )
					trailing = 0;
				a = a->Next;
			}
			if (trailing == 1)
				Done = 1;
		}
		if (Done == 0) {
			if (fprintf(ProfileFile, "%s", p->S) == EOF) {
				fclose(ProfileFile);
				return 0;
			}
		}
		p = p->Next;
	}
	if (fclose(ProfileFile) == EOF)
		return 0;
	CachedFileChanged = 0;
	return 1;
#else
	return 1;
#endif
}

/*
	int AbortProfileChanges()

	Deallocates cache buffer without saving changes to disk.

	The return value is always nonzero, unless there is no profile file
	currently being cached.
*/
int AbortProfileChanges() {
#ifdef INI_USE
	PS* p;
	if (!ProfileFileInUse)
		return 0;
	p = ProfileHead->Next;
	while (p->Next != NULL) {
		p = p->Next;
		free(p->Prev);
	}
	free(ProfileHead);
	free(ProfileTail);
	ProfileHead = NULL;
	ProfileTail = NULL;
	ProfileFileInUse = 0;
	return 1;
#else
	return 1;
#endif
}

#ifdef INI_USE
int GetPrivateProfileInt(PCHR pszSection, PCHR pszEntry, int pszDefault,
        PCHR pszFileName) {
        CHR defaultBuf[MAXPROFILESTR];
        CHR valueBuf[MAXPROFILESTR];
 
        sprintf(defaultBuf,"%i",pszDefault);
 
        GetPrivateProfileString(pszSection, pszEntry, defaultBuf,
                valueBuf, sizeof(valueBuf), pszFileName);
 
        return(atoi(valueBuf));
}

/* Everything after this point is within INI_USE. */

/*
	int GetPrivateProfileString(PCHR pszSection, PCHR pszEntry,
			PCHR pszDefault, PCHR pszReturnBuffer,
			int iBufferSize, PCHR pszFileName)

	This function retrieves a character string from the specified
	section in the specified initialization file.

	pszSection	Points to a null-terminated string that specifies the
			section to retrieve data from.
	pszEntry	Points to a null-terminated string that specifies the
			entry to retrieve data from.
	pszDefault	Points to a null-terminated string that should be
			returned (without leading or trailing spaces) if the
			specified entry is not found.
	pszReturnBuffer	Points to a buffer that should be filled with the
			retrieved data, minus any leading or trailing spaces.
	iBufferSize	Specifies the size, in bytes, of the buffer
			pointed to by pszReturnBuffer.
	pszFileName	Points to the null-terminaed string that names the
			initialization file.  If a file has been opened for
			caching with CacheProfileFile() then this file name
			must be identical to the one passed to
			CacheProfileFile(); otherwise this function will
			fail.

	The return value is nonzero if the function is successful.  Note that
	on completion the string pointed to by pszReturnBuffer will not
	contain leading or trailing spaces.
*/
int GetPrivateProfileString(PCHR pszSection, PCHR pszEntry,
		PCHR pszDefault, PCHR pszReturnBuffer, int iBufferSize,
		PCHR pszFileName) {
        int TempCache = 0;
        PS* ps;
        PS* pe;
        PCHR pd;
        int Found = 0;
        int x;
        CHR Section[MAXPROFILESTR];
        if (ProfileFileInUse) {
                if (strcmp(pszFileName, ProfileFileName) != 0)
                        return 0;
        } else {
                if (!CacheProfileFile(pszFileName)) /* should never happen */
                        return 0;
                TempCache = 1;
        }
        strcpy(Section, "[");
        strncat(Section, pszSection, MAXPROFILESTR-strlen(Section)-1);
        strncat(Section, "]\n", MAXPROFILESTR-strlen(Section)-1);
        ps = FindProfileSection(Section);
        if (ps != NULL) {
                pe = FindProfileEntry(pszEntry, ps);
                if (pe != NULL) {
                        pd = strchr(pe->S, '=');
                        if (pd != NULL) {
                                strncpy(pszReturnBuffer, pd+1, iBufferSize-1);
                                Found = 1;
                        }
                }
        }
        if (Found == 0)
                strncpy(pszReturnBuffer, pszDefault, iBufferSize-1);
        CleanupProfileString(pszReturnBuffer);
        while ( ((x=strlen(pszReturnBuffer)) > 0) &&
                        (pszReturnBuffer[x-1] == '\n') )
                pszReturnBuffer[x-1] = '\0';
        
        if (TempCache)   
                if (!UncacheProfileFile())
                        return 0;
        return 1;
}

/*
	int WritePrivateProfileString(PCHR pszSection, PCHR pszEntry,
			PCHR pszString, PCHR pszFileName)

	This function copies a character string into the specified section
	of the specified initialization file.

	pszSection	Points to a null-terminated string that specifies
			the section to which the string should be copied.
			If the section does not exist, it is created.  The
			name of the section is case-independent; the string
			may be any combination of uppercase and lowercase
			letters.
	pszEntry	Points to the null-terminated string containing the
			entry to be associated with the string.  If the entry
			does not exist in the specified section; it is
			created.  If this parameter is NULL, the entire
			section, including all entries within the section is
			deleted.
	pszString	Points to the null-terminated string to be written
			to the file.  If this parameter is NULL, the entry
			specified by the pszEnter parameter is deleted.
	pszFileName	Points to the null-terminaed string that names the
			initialization file.  If a file has been opened for
			caching with CacheProfileFile() then this file name
			must be identical to the one passed to
			CacheProfileFile(); otherwise this function will
			fail.

	The return value is nonzero if the function is successful.
*/
int WritePrivateProfileString(PCHR pszSection, PCHR pszEntry,
		PCHR pszString, PCHR pszFileName) {
	int TempCache = 0;
	PS* p;
	PS* SecPtr;
	CHR Section[MAXPROFILESTR];
	if (ProfileFileInUse) {
		if (strcmp(pszFileName, ProfileFileName) != 0)
			return 0;
	} else {
		if (!CacheProfileFile(pszFileName)) /* should never happen */
			return 0;
		TempCache = 1;
	}
	/* Section */
	strcpy(Section, "[");
	strncat(Section, pszSection, MAXPROFILESTR-strlen(Section)-1);
	strncat(Section, "]\n", MAXPROFILESTR-strlen(Section)-1);
	p = FindProfileSection(Section);
	if ( (p == NULL) && (pszEntry != NULL) ) {
		if (!(p = (PS*)imem_calloc(1,sizeof(PS)))) {
			if (TempCache)
				AbortProfileChanges();
			return 0;
		}
		strcpy(p->S, Section);
		p->Prev = ProfileTail->Prev;
		p->Next = ProfileTail;
		p->Prev->Next = p;
		p->Next->Prev = p;
		if (!InsertBlankLine(p)) {
			if (TempCache)
				AbortProfileChanges();
			return 0;
		}
		CachedFileChanged = 1;
	}
	SecPtr = p;
	/* Entry */
	if ( (pszEntry == NULL) && (p != NULL) ) {
		/* delete entire section */
		PS* t;
		CHR s[MAXPROFILESTR];
		do {
			t = p;
			p = p->Next;
			t->Prev->Next = p;
			p->Prev = t->Prev;
			free(t);
			strcpy(s, p->S);
			CleanupProfileString(s);
		} while ( (p->Next != NULL) && (s[0] != '[') );
		CachedFileChanged = 1;
	} else {
		if (pszEntry != NULL) {
			/* set entry */
			p = FindProfileEntry(pszEntry, SecPtr);
			if ( (p == NULL) && (pszString != NULL) ) {
				CHR s[MAXPROFILESTR];
				PS* t = SecPtr;
				do {
					t = t->Next;
					if (t->Next != NULL) {
						strcpy(s, t->S);
						CleanupProfileString(s);
					} else
						s[0] = '\0';
				} while ( (t->Next != NULL) && (s[0] != '[') );
				if (!(p = (PS*)imem_calloc(1,sizeof(PS)))) {
					if (TempCache)
						AbortProfileChanges();
					return 0;
				}
				strcpy(p->S, "\n");
				p->Prev = t->Prev;
				p->Next = t;
				p->Prev->Next = p;
				p->Next->Prev = p;
				/* backup to end of last section */
				strcpy(s, "\n");
				while ( (p->Prev->Prev != NULL) &&
						(s[0] == '\n') ) {
					strcpy(s, p->Prev->S);
					CleanupProfileString(s);
					if (s[0] == '\n')
						p = p->Prev;
				}
				CachedFileChanged = 1;
			}
			/* String */
			if ( (pszString == NULL) && (p != NULL) ) {
				/* delete entry */
				p->Prev->Next = p->Next;
				p->Next->Prev = p->Prev;
				free(p);
				CachedFileChanged = 1;
			} else {
				PCHR skip = p->S;
				/* set string */
				while ( ( (*skip == ' ') || (*skip == '\t') )
						&& (*skip != '\0') )
					skip++;	/* preserve indentation */
				strcpy(skip, pszEntry);
				strncat(skip, "=",
						MAXPROFILESTR-strlen(skip)-1);
				strncat(skip, pszString,
						MAXPROFILESTR-strlen(skip)-1);
				strncat(skip, "\n",
						MAXPROFILESTR-strlen(skip)-1);
				CachedFileChanged = 1;
			}
		}
	}

	if (TempCache)
		if (!UncacheProfileFile())
			return 0;
	return 1;
}

/* Internal functions */

/*
	PS* FindProfileSection(PCHR pszSection)

	Searches the cached profile file for a section name.

	pszSection	Points to a null-terminated string that specifies a
			section name to search for.

	The return value is a pointer to the line that contains the specified
	section name.
*/
PS* FindProfileSection(PCHR pszSection) {
	PS* p = ProfileHead->Next;
	int done = 0;
	CHR s[MAXPROFILESTR];
	while ( (p->Next != NULL) && (!done) ) {
		strcpy(s, p->S);
		CleanupProfileString(s);
		if (strcasecmp(s, pszSection) == 0)
			done = 1;
		else
			p = p->Next;
	}
	if (p->Next == NULL)
		return NULL;
	return p;
}

/*
	PS* FindProfileEntry(PCHR pszEntry)

	Searches the cached profile file for an entry name.

	pszSection	Points to a null-terminated string that specifies an
			entry name to search for.

	The return value is a pointer to the line that contains the specified
	entry name.
*/
PS* FindProfileEntry(PCHR pszEntry, PS* SecPtr) {
	PS* p = SecPtr->Next;
	CHR s[MAXPROFILESTR], Entry[MAXPROFILESTR];
	PCHR t;
	int done = 0;
	strcpy(Entry, pszEntry);
	strncat(Entry, "=", MAXPROFILESTR-strlen(Entry)-1);
	while ( (p->Next != NULL) && (!done) ) {
		strcpy(s, p->S);
		CleanupProfileString(s);
		if ( (t = strchr(s, '=')) != NULL ) {
			while ( (*(t-1) == ' ') && (t > s) ) {
				strcpy(t-1, t);
				t--;
			}
		}
		if ( (strncmp(s, Entry, strlen(Entry)) == 0) ||
				(s[0] == '[') )
			done = 1;
		else
			p = p->Next;
	}
	if ( (p->Next == NULL) || (s[0] == '[') )
		return NULL;
	return p;
}

/*
	int CleanupProfileString(PCHR pszString)

	Removes leading and trailing spaces and tabs from a string.

	pszString	Points to a null-terminated string.

	The return value is always nonzero.
*/
int CleanupProfileString(PCHR pszString) {
	/* remove leading and trailing spaces and tabs */
	PCHR p = pszString;
	PCHR t;
	while ( ( (*p == ' ') || (*p == '\t') ) && (*p != '\0') )
		p++;
	if((strlen(p) == 0) || (*p == '\n'))
		return 1;

	while ( ( (*(t=p+strlen(p)-2) == ' ') || (*t == '\t') ) && (t >= p) ) {
		t[0] = '\n';
		t[1] = '\0';
	}
	strcpy(pszString, p);
	return 1;
}

/*
	int InsertBlankLine(PS* Ptr)

	Inserts a blank line in the cached profile file.

	Ptr	Points to a position in the cached file before which to insert
		a blank line.

	The return value is nonzero if the function is successful.
*/
int InsertBlankLine(PS* Ptr) {
	PS* p;
	if (!(p = (PS*)imem_calloc(1,sizeof(PS))))
		return 0;
	strcpy(p->S, "\n");
	p->Prev = Ptr->Prev;
	p->Next = Ptr;
	Ptr->Prev->Next = p;
	Ptr->Prev = p;
	return 1;
}

#endif
