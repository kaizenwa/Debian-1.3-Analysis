/***************************************************************
Copyright Notice

Copyright (c) MCNC, Clearinghouse for Networked Information Discovery
and Retrieval, 1993.

Permission to use, copy, modify, distribute, and sell this software
and its documentation, in whole or in part, for any purpose is hereby
granted without fee, provided that

1. The above copyright notice and this permission notice appear in all
copies of the software and related documentation. Notices of copyright
and/or attribution which appear at the beginning of any file included
in this distribution must remain intact.
2. Users of this software agree to make their best efforts (a) to
return to MCNC any improvements or extensions that they make, so that
these may be included in future releases; and (b) to inform MCNC/CNIDR
of noteworthy uses of this software.

3. The names of MCNC and Clearinghouse for Networked Information
Discovery and Retrieval may not be used in any advertising or publicity
relating to the software without the specific, prior written permission
of MCNC/CNIDR.

THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND,
EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY
WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.

IN NO EVENT SHALL MCNC/CNIDR BE LIABLE FOR ANY SPECIAL, INCIDENTAL,
INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND, OR ANY DAMAGES
WHATSOEVER
RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER OR NOT ADVISED OF
THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF LIABILITY, ARISING OUT
OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

***************************************************************/
#include "gdt.h"
#include <ctype.h>
#define PLATFORM_INC_MALLOC
#include "platform.h"
#include <stdio.h>
#include <string.h>
#include "strbox.h"
#include <time.h>

#include "imem.h"

#define CALLOC(a,b) imem_calloc(a,b)
#define MALLOC(a) imem_malloc(a)

#ifdef NO_STRDUP
PCHR strdup(PCHR S) {
	PCHR T;
	   T = (PCHR)MALLOC(strlen(S)+1);
        if (T != NULL) {
                strcpy(T, S);
	   }
        return T;
}
#endif

#ifdef PLATFORM_MSC
#define strdup() _strdup()
#endif

/*
FUNCTION:	strbox_StringInSet()													      
PURPOSE:	Looks for a specified string within a specified
		set of strings.										      
PRE:		s = string for which to look.
		stopList = Array of null-terminated strings containing
		string set.                   
POST:		0 if s is found within stopList.
		1 if s not found within stopList.
*/
INT strbox_StringInSet(PCHR s, PCHR *stopList) {
	INT i=0;

	while(stopList[i] != NULL) {
		if(!strcmp(s,stopList[i]))
			return 0;
		i++;
	}
	return 1;
}

void strbox_Upper(PCHR s) {
	INT i;

	for(i=0;i<strlen(s);i++) {
		if(islower(s[i]))
			s[i]=s[i]-0x20;
        }
}

/*
FUNCTION:	strbox_TheTime()
PURPOSE:	Copies the current time as a string into specified
buffer.
PRE:		buf = a pointer to an allocated char buffer.
POST:		buf = NULL-terminated string of the form:
		"Thu Feb 10 13:00:27 1994"
CALLS:		time(), sprintf(), ctime(),strlen()
*/
void strbox_TheTime(PCHR buf)
{
	time_t lTimePtr;
	INT i;

	time(&lTimePtr);
	sprintf(buf,"%s",ctime(&lTimePtr));
  	/* strip off the trailing newline */
	for(i=0;i<strlen(buf);i++) {
		if(buf[i] == '\r' || buf[i] == '\n') {
			buf[i]='\0';
			return;
		}
	}
}

INT strbox_itemCountd(PCHR S, CHR Delim) {
	PCHR NS;
	INT x, c;
	if (S == NULL)
		return 0;
	NS = (PCHR)CALLOC(1,strlen(S)+1);
	strcpy(NS, S);
	if (NS == NULL)
		return 0;
	strbox_Cleanup(NS);
	if (NS[0] == '\0')
		return 0;
	c = 1;
	for (x = 0; x < strlen(NS); x++)
		c += (NS[x] == Delim);
	return c;
}

INT strbox_itemCount(PCHR S) {
	PCHR NS;
	INT x, c;
	if (S == NULL)
		return 0;
	NS = (PCHR)CALLOC(1,strlen(S)+1);
	strcpy(NS, S);
	if (NS == NULL)
		return 0;
	strbox_Cleanup(NS);
	if (NS[0] == '\0')
		return 0;
	c = 1;
	for (x = 0; x < strlen(NS); x++)
		c += (NS[x] == ',');
	return c;
}


PCHR strbox_itemd(PCHR S, INT X, PCHR Item, CHR Delim) {
	PCHR NS;
	PCHR p;
	PCHR NSp;
	INT c;
	if (Item == NULL)
		return NULL;
	Item[0] = '\0';
	if (S == NULL)
		return Item;
	if (X < 1)
		return Item;
	NS = (PCHR)CALLOC(1,strlen(S)+1);
	strcpy(NS,S);
	if (NS == NULL)
		return Item;
	strbox_Cleanup(NS);
	if (NS[0] == '\0')
		return Item;
	c = X - 1;
	NSp = NS;
	while ( (c > 0) && ((p=strchr(NSp, Delim)) != NULL) ) {
		NSp = p + 1;
		c--;
	}
	if (c > 0)
		return Item;
	if ((p=strchr(NSp, Delim)) != NULL)
		*p = '\0';
	strcpy(Item, NSp);
	free(NS);
	return Item;
}
PCHR strbox_item(PCHR S, INT X, PCHR Item) {
	PCHR NS;
	PCHR p;
	PCHR NSp;
	INT c;
	if (Item == NULL)
		return NULL;
	Item[0] = '\0';
	if (S == NULL)
		return Item;
	if (X < 1)
		return Item;
	NS = (PCHR)CALLOC(1,strlen(S)+1);
	strcpy(NS, S);
	if (NS == NULL)
		return Item;
	strbox_Cleanup(NS);
	if (NS[0] == '\0')
		return Item;
	c = X - 1;
	NSp = NS;
	while ( (c > 0) && ((p=strchr(NSp, ',')) != NULL) ) {
		NSp = p + 1;
		c--;
	}
	if (c > 0)
		return Item;
	if ((p=strchr(NSp, ',')) != NULL)
		*p = '\0';
	strcpy(Item, NSp);
	free(NS);
	return Item;
}


void strbox_Cleanup(PCHR s) {
	PCHR p = s;
	while (*p == ' ')
		p++;
	strcpy(s, p);
	p = s + strlen(s) - 1;
	while ( (p >= s) && (*p == ' ') )
		*p-- = '\0';
}

/*
Returns number of characters read up to and including CR or CRLF and
writes them into Dest.
However, Dest does not contain CR or CRLF.
*/
INT strbox_SGetS(PCHR Dest, PCHR Src)
{
        INT i;
 
        if(Src==NULL || Src[0]=='\0' || Dest==NULL)
                return -1;
 
        for(i=0;i<strlen(Src);i++) {
                if(Src[i]==0x0a && Src[i+1]!=0x0d) {
                        Dest[i]='\0';
                        return(strlen(Dest)+1);
                } else
                if(Src[i]==0x0d) {
                        Dest[i]='\0';
                        return(strlen(Dest)+2);
                }
                Dest[i]=Src[i];
        }
        Dest[i]='\0';
 
        return(strlen(Dest));
}

/* 
Converts all spaces in a string to the character sequence "%22".

You must enocode spaces in URLs in this manner.  Make sure that your
buffer is large enough to handle the extra space!
*/
void strbox_EscapeSpaces(PCHR S)
{
	INT i,j=0;
	PCHR T;

	T = (PCHR)CALLOC(1,strlen(S)+100);
	T[0]='\0';
	
	for(i=0;i<strlen(S);i++) {
		if(S[i]==' ') {
			strcat(T,"%22");
			j+=3;
		} else {
			T[j++]=S[i];
			T[j]='\0';
		}
	}
	strcpy(S,T);
	free(T);

}

/*
Strip LF and CR from line
*/
void strbox_chop(char *line)
{
        register char   *cp;
 
        if ( *line == '\0')
                return;
        cp = line;
        while ( *cp )
                cp++;
        if ( *--cp == '\n') {
                *cp = '\0';
                if ( (cp > line) && *--cp == '\r')
                        *cp = '\0';
        }
}
 

