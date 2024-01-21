#include <stdio.h>
#include <string.h>
#ifdef SYSV
#define index strchr
#endif

char * Findsub(sC,sF)
char *sC, *sF;
/*
    return the start of substring sF in string sC
    or NULL if sC doesn't contain sF

    Copyright
	Rudi van Houten, Academic Computer Centre Utrecht
			 Budapestlaan 6  3584 CD  Utrecht  Netherlands
*/
{
    int iFlen;
    register char *pcC;
    iFlen= strlen(sF);
    if ((pcC= index(sC,sF[0])) == NULL) return(NULL);
    while (strlen(pcC) >= iFlen)
	if (strncmp(pcC,sF,iFlen) == 0) return(pcC);
	else
	if ((pcC= index(++pcC,sF[0])) == NULL) return(NULL);
    return(NULL);
} /* Findsub */
