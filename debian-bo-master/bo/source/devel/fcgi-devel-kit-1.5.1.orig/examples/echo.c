/* 
 * echo.c --
 *
 *	Produce a page containing all FastCGI inputs
 *
 *
 * Copyright (c) 1996 Open Market, Inc.
 *
 * See the file "LICENSE.TERMS" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#ifndef lint
static const char rcsid[] = "$Id: echo.c,v 1.2 1996/10/30 14:38:03 mbrown Exp $";
#endif /* not lint */

#include "fcgi_stdio.h"
#include <stdlib.h>

extern char **environ;

void PrintEnv(char *label, char **envp)
{
    printf("%s:<br>\n<pre>\n", label);
    for(; *envp != NULL; envp++) {
        printf("%s\n", *envp);
    }
    printf("</pre><p>\n");
}

void main ()
{
    char **initialEnv = environ;
    int count = 0;
    while(FCGI_Accept() >= 0) {
        char *contentLength = getenv("CONTENT_LENGTH");
        int len;
	printf("Content-type: text/html\r\n"
	       "\r\n"
	       "<title>FastCGI echo</title>"
	       "<h1>FastCGI echo</h1>\n"
               "Request number %d <p>\n", ++count);
        if(contentLength != NULL) {
            len = strtod(contentLength, NULL);
        } else {
            len = 0;
        }
        if(len <= 0) {
	    printf("No data from standard input.<p>\n");
        } else {
            int i, ch;
	    printf("Standard input:<br>\n<pre>\n");
            for(i = 0; i < len; i++) {
                if((ch = getchar()) < 0) {
                    printf("Error: Not enough bytes received "
                           "on standard input<p>\n");
                    break;
		}
                putchar(ch);
            }
            printf("\n</pre><p>\n");
        }
        PrintEnv("Request environment", environ);
        PrintEnv("Initial environment", initialEnv);
    } /* while */
}
