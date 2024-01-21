/*-
 * Copyright (c) 1993, Trusted Information Systems, Incorporated
 * All rights reserved.
 *
 * Redistribution and use are governed by the terms detailed in the
 * license document ("LICENSE") included with the toolkit.
 */

/*
 *	Author: Wei Xu, Trusted Information Systems, Inc.
 */
static	char	RcsId[] = "Header: string.c,v 1.3 94/07/18 13:59:32 wei Locked ";

#include        "ulib.h"

extern  char **tokens();


/*******bncat**********************************************************
 # like strncat this routine handle binary cat
 * return a new buf size.
 **********************************************************************/
char *bncat( s, sbytes, add, addbytes )
char	*s;
int      sbytes;
char	*add;
int 	 addbytes;
{
	int	n=sbytes+addbytes;

	if( !add || addbytes<=0 ) return s;

	if(!sbytes) s=(char*)malloc(n+1);
	else        s=(char*)realloc(s,n+1);
        bcopy((void*)add,(void*)&s[sbytes],(size_t)addbytes);

	return s;
}

/*******strsave********************************************************
 * allocate the memory for the string and return the addr
 **********************************************************************/
 char * strsave( s, len )
 char * s;
 int    len;
 {
     char  *rs;

     if( len <= 0 ) len= strlen( s );

     rs = (char*)malloc( len+1 );
     bzero( rs, len+1 );
     strncpy( rs,s,len );

     return rs;
 }

/*******stradd********************************************************
 * allocate memory and add addbuf to the string s. return string size.
 *********************************************************************/
char	*stradd(s,addbuf,len)
char	*s, *addbuf;
int	len;
{
	int	n;

	if(len<=0) return s;

	s=(char*)realloc(s,(s)?(n=strlen(s)+len+1):(n=len+1));
        strncat(s,addbuf,len);
	s[n]=NULL;

	return s;
}
 
/***************addstrlist*********************************************
 * add to the string arry of the list.
 * return: a) function return the list array point.
 * if len is not null, return the element # of the list.
 **********************************************************************/
char ** addstrlist( list, s, len )
char  ** list;
char   * s;
int    * len;
 {
     int    i, n=0;
     char **olist=list;

     if( list ) while( list[++n] );

     list = (char **)calloc( n+2, sizeof(char*) );
     for( i=0; i<n; i++ ) list[i]=olist[i];
     free( olist );

     list[n++] = (char*)strsave( s, (len && *len>0)? *len: 0 );
     if( len ) *len=n;
     list[n]=(char*)0;

     return list;
 }

void clearstrlist( list )
char  ** list;
{
   int  n=0;

   while( list[n] )  free( (char*)list[n++] );
   free( (char**)list );
}


int wmatch(pattern,string)
char    *pattern;
char    *string;
{
	register char c;
	while(1)
		switch(c = *pattern++) {
			case '\0': return(*string == '\0');
			case '*' : c = *pattern;
				while(c == '*') c = *++pattern;
                                if(c == '\0') return(1);

                             /* general case, use recursion */
                                while(*string != '\0') {
                                       if(wmatch(pattern,string)) return(1);
                                       ++string;
                                }
                                return(0);

                        default: if(c != *string++) return(0);
                                break;
                    }
}

int wtmatch( pattern, string )
char *pattern;
char *string;
{
   int    n=0, cnt;
   char **list;

   list=tokens( pattern," ",&cnt );
   if( !list ) return -1;

   while( cnt-- >0 ) {
	if( (wmatch( list[cnt], string ))>0 ) {
	    n=cnt+1;
	    break;
	 }
   }
   clearstrlist( list );
   return( n );
}

#ifdef TEST_WTMATCH
main()
{
   int   n;
   char *p = "**abc d**ef gh**i jkl***\0";
   char  buf[128];

   while(1) {
       printf("\n input a string: ");
       fscanf( stdin,"%s",buf );

       if( (n=wtmatch( p, buf )) >0 ) printf( "matched %d\n",n );

       else printf( "failed\n" );
   }
}
#endif

