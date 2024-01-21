/*
This software is provided as-is, with no warranties for suitability of
use or support implied.  This package makes no guarantees that it will
perform in any manner.  The authors and Texas A&M University accept no
liability for any damages incurred while using this software.

This software may be copied, redistributed, and updated in any
fashion as long as this comment header is not removed or altered.

Douglas Lee Schales
Doug.Schales@sc.tamu.edu
Texas A&M University
Supercomputer Center

01/30/1993
*/
#include <stdio.h>
#include <ctype.h>
#define PARSE_TABLES
#include "amddefs.h"
#include "parser.h"

extern char *getline(void);
extern char *findchar(char *, char *);

extern void parse(void);
static void dosel(char *, char *, int);
static void doopt(char *, char *, int);

int parse_error;

void
parse(void)
{
     char *buffer;
     char *key, *selopt;
     char *s, *bp;
     int len;
     int i;

     parse_error = 0;
     while(buffer = getline()){
	  /* Skip leading space to start of key */
	  for(bp=buffer;*bp && isspace(*bp);bp++)
	       ;
	  s = bp;
	  /* Find end of key */
	  for(;*bp && !isspace(*bp);bp++)
	       ;
	  len = bp-s;
	  key = (char *)malloc(len+1);
	  memmove(key, s, len);
	  key[len] = 0;
	  while(bp && *bp){
	       /* Skip space to start of selopt */
	       for(;*bp && isspace(*bp);bp++)
		    ;
	       setdefaults(0);
	       while(bp && *bp && !isspace(*bp) && *bp != '|'){
		    if(*bp == '-'){
			 setdefaults(1);
			 bp++;
		    }
		    s = bp;
		    for(;*bp;bp++)
			 if(isspace(*bp))
			      break;
			 else if(*bp == '|')
			      break;
			 else if(*bp == ':' ||
				 *bp == '=' ||
				 *bp == '!')
			      break;
		    if(*bp == ':' ||
		       *bp == '=' ||
		       *bp == '!'){
			 len = bp-s;
			 selopt = (char *)malloc(len + 1);
			 memmove(selopt, s, len);
			 selopt[len] = 0;
			 for(i=0;selector[i].name;i++)
			      if(!strcmp(selector[i].name, selopt))
				   break;
			 if(selector[i].name)
			      dosel(key, bp, selector[i].token);
			 else {
			      for(i=0;options[i].name;i++)
				   if(!strcmp(options[i].name, selopt))
					break;
			      if(options[i].name)
				   doopt(key,bp, options[i].token);
			      else {
				   printf("Key \"%s\": unrecognized sel-opt: %s\n",
					  key,
					  selopt);
				   parse_error = 1;
			      }
			 }
			 free(selopt);
		    }
		    else {
			 printf("Key \"%s\": syntax error\n", key);
			 if(*bp == '|')
			      bp--;
		    }
		    bp = findchar(bp, "; \t|");
		    if(bp && *bp == ';')
			 bp++;
	       }
	       pushrecord();
	       if(bp){
		    if(*bp == '|' && *(bp+1) == '|')
			 bp++;
		    bp++;
	       }
		    
	  }
	  pushentry(key);
	  free(key);
	  free(buffer);
     }
}

static void
dosel(char *key, char *op, int seltok)
{
     int eqne;
     char *s, *e, *val;
     int len;

     if(*op == ':' && *(op+1) == '='){
	  printf("Key \"%s\": ':=' should be either '!=' or '=='\n", key);
	  parse_error = 1;
	  return;
     }
     if(*op == '!' && *(op+1) != '='){
	  printf("Key \"%s\": missing '=' after '!'\n", key);
	  parse_error = 1;
	  return;
     }
     if(*op == '=' && *(op+1) != '='){
	  printf("Key \"%s\": old style selector; '=' should be '=='\n", key);
     }
     if(*op == '!')
	  eqne = 0;
     else if(*op = '=')
	  eqne = 1;
     else {
	  printf("Key \"%s\": unknown operator\n", key);
	  parse_error = 1;
	  return;
     }
     if(*(op+1) == '=')
	  s = op + 2;
     else
	  s = op + 1; /* old style */

     if(e = findchar(s, "; \t|"))
	  len = e - s;
     else
	  len = strlen(s);

     val = (char *)malloc(len+1);
     memmove(val, s, len);
     val[len] = 0;
     pushsel(seltok, eqne, val);
     free(val);
}

static void
doopt(char *key, char *op, int vartok)
{
     int eqne;
     char *s, *e, *val;
     int len;

     if(*op == '=' && *(op+1) != '='){
	  printf("Key \"%s\": old style assignment; '=' should be ':='\n",
		 key);
     }
     if(*op == ':' && *(op+1) != '='){
	  printf("Key \"%s\": unknown assignment operator\n", key);
	  parse_error = 1;
	  return;
     }
     if(*op != '=' && *op != ':'){
	  printf("Key \"%s\": unknown assignment operator\n", key);
	  parse_error = 1;
	  return;
     }
     if(*op == ':')
	  s = op + 2;
     else
	  s = op + 1; /* old style */

     if(e = findchar(s, "; \t|"))
	  len = e - s;
     else
	  len = strlen(s);

     val = (char *)malloc(len+1);
     memmove(val, s, len);
     val[len] = 0;
     pushvar(vartok, val);
     free(val);
}
