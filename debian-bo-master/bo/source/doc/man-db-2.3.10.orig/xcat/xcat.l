%{

/*
 * xcat: produce .m file from .c file for gencat 
 *  
 * Copyright (C), 1994, 1995, Graeme W. Wilford. (Wilf.)
 *
 * You may distribute under the terms of the GNU General Public
 * License as specified in the file COPYING that comes with this
 * distribution.
 *
 * This program should be used with C programs containing certain 
 * directives, specifically CATGETS() and NLS_SET should be #defined.
 *
 * usage: xcat Register < foo.c > foo.m
 *
 * Wed Oct 12 18:46:11 BST 1994  Wilf. (G.Wilford@ee.surrey.ac.uk) 
 */

/* This program provides a basic method of producing message catalogues
   for C source files. Each C source file must define NLS_SET prior to
   #including "nls.h", which will set up the CATGETS() macro. eg

   #define NLS_SET	<file_id>Set
   #include "nls.h"

   where <file_id> is a unique file id such as the filename itself.
   From then on, you may use the following CATGETS() function instead of 
   the library function catgets(). The CATGETS function is defined:

   char * CATGETS(int id, char *message)

   where id is the name <file_id>_<message_id>, message_id being any upper
   case name that uniquely identifies the message. Identical messages may
   have identical message id's. 

   gencat can be used to process the message file produced, and should also
   be used to create a macro header file. This header will contain definitions
   of all of the <file_id>_<message_id> names you used. These map to integers.
 */

#define MAX_MSGS	100	/* maximum number of messages per C-file */
#define NLS_TEXT	""	/* bogus string prepended to each message
				   for debugging purposes, set to "" if
				   preferred  */ 
#define STATIC_VER

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif /* HAVE_CONFIG_H */

#if defined(STDC_HEADERS) || defined(HAVE_STRING_H)
#  include <string.h>
#elif defined(HAVE_STRINGS_H)
#  include <strings.h>
#endif /* STDC_HEADERS */

#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif /* HAVE_UNISTD_H */

#include <ctype.h>

#include "manconfig.h"
#include "lib/error.h"

static int get_set_number(void);

char *program_name = "xcat";
static short Set_len;
static char ch, *register_file = NULL;
static char buffer[4096];
%}

%x in_string
%x catgets_arg1
%x catgets_rest
%x in_Set
%x normal

%%

#define[ \t]+NLS_SET[ \t]+ 	BEGIN(in_Set);
<in_Set>[^ \t\n"]+	   {
			yytext[yyleng - 3] = '\0';
			printf("$set %d #%s\n", get_set_number(), yytext);
			Set_len = yyleng - 3;
			BEGIN(normal);
		   }

<normal>CATGETS[ \t\n]*\([ \t\n]*	BEGIN(catgets_arg1);
<catgets_arg1>[^" \n\t,]+  {
			static struct {
				char *text;
				int seen;
			} message[MAX_MSGS];
			static int msg_no = 0;
			int i = 0;

			/* don't duplicate messages with identical id's */
			while (i != msg_no) {
				if (strcmp(message[i].text, yytext) == 0) {
					if (!message[i].seen) {
						error(0, 0, 
						      "warning: duplicated message id: %s",
						      yytext);
						message[i].seen = 1;
					}
					BEGIN(normal);
					i = -1;
					break;
				}
				i++;
			}
				
			if (i != -1) {
				message[msg_no++].text = xstrdup(yytext);
				yytext += Set_len;
				printf("\n$ #%s Original Message:", yytext);
				BEGIN(catgets_rest);
				ch = '\0';
			}
		   }
<catgets_rest>[^"\) \t\n,]*    strcat(buffer, yytext);
<catgets_rest>\)   {
			printf("(%s)\n# " NLS_TEXT "%s\n", buffer, buffer);
			buffer[0] = '\0';
			BEGIN(normal);
		   }
<catgets_rest>\"   {	
			if (ch)
				strcat(buffer, "\\\n");
			BEGIN(in_string);
			ch = '\\';
		   }
<catgets_arg1,catgets_rest>[ \n\t,]
<in_string>[^\\"]*	strcat(buffer, yytext);
<in_string>\\\"		putchar(*(yytext + 1));
<in_string>\\[^"]	strcat(buffer, yytext);
<in_string>\"      {
			BEGIN(catgets_rest);
		   }
<in_string,catgets_arg1,catgets_rest,in_Set><<EOF>>  {
			error (0, 0, "fatal: EOF within CATGETS() or Set definition");
			yyterminate();
		   }
<INITIAL,normal>.|\n

%%

int main(int argc, char *argv[]) 
{
	if (argc == 2)
		register_file = argv[1];

	yylex();
	return 0;
}

static int get_set_number(void)
{
	if (register_file) {
		FILE *file;

		if ( (file = fopen(register_file, "r")) ) {
			char line[1024];

			while ( fgets(line, 1023, file) ) {
				if (*line == '#')
					continue;
				if (strncmp(line, yytext, yyleng - 3) == 0 &&
				    isspace(*(line + yyleng - 3))) {
					fclose(file);
					return atoi( line + yyleng - 3 + 1 );
				}
			}
			error (1, 0, "set \"%s\" in %s not found, aborting",
			       yytext, register_file);
		}
	} 
	return 1;
}

/* for compatibility with systems not having libfl */
int yywrap(void)
{
	return 1;
}

