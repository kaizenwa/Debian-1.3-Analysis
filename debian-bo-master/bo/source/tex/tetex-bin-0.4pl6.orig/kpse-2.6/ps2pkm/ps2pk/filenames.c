/* FILE:    filenames.c
 * PURPOSE: some handy functions for working with TeXfiles
 * AUTHOR:  Piet Tutelaers (internet: rcpt@urc.tue.nl)
 * VERSION: 1.3 (August 1992)
 * VERSION: 1.4 (January 1994)
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

void fatal(char *fmt, ...);

#if !defined(MSDOS) && !defined(VMS)
#define DIRECTORY '/'
#else
#   ifdef MSDOS
#   define DIRECTORY '\\'
#   else
#   define DIRECTORY ']'
#   endif
#endif

#define VOLUME ':'
#define MAXSTRLEN 80

/* for those systems that don't provide an ANSI strchr() */
static char *charptr(char *name, char c)
{
   while (*name != c && *name != '\0') name++; 
   if (*name == '\0') return NULL;
   else return name;
}

/* comparing names (system dependant) */
int equal(char *s, char *t)
{
#  ifndef UNIX
   while (tolower(*s) == tolower(*t)) {
      if (*s == '\0') break;
      s++; t++;
   }
#  else
   while (*s == *t) {
      if (*s == '\0' || *t == '\0')  break;
      s++; t++;
   }
#  endif
   if (*s == '\0' && *t == '\0') return 1;
   else return 0;
}

/* add extension if it is not contained in the filename */
char *fullname(char *name, char *ext)
{  char *charspace, *s_name; int length;
   
   s_name = name;
   while ((name = charptr(name, '.')) != NULL) {
      if (equal(name, ext)) break;
      name++;
   }
   if (name == NULL) { /* this must be the basename */
      length = strlen(s_name)+strlen(ext)+1;
      if (length > MAXSTRLEN) 
         fatal("String too long!\n");
      charspace = (char *) malloc(length);
      if (charspace == NULL)
         fatal("Error in allocating space for a fullname\n");
      strcpy(charspace, s_name);
      strcat(charspace, ext);
      charspace[length-1] = '\0';
      return charspace;
   }
   else  /* we have already a full name */
      return s_name;
}

char *trim_basename(char *name, char *ext)
{  char *charspace, *s_name; int length;
   
   if (ext[0] != '.') fatal("Wrong extension.\n");
   /* skip over DIRECTORY */
   s_name = name;
   while ((name = charptr(name, DIRECTORY)) != NULL) s_name = ++name;
   /* skip over VOLUME */
   name = s_name;
   while ((name = charptr(name, VOLUME)) != NULL) s_name = ++name;
   /* look for extension */
   name = s_name;
   while ((name = charptr(name, '.')) != NULL) {
      if (strcmp(ext, ".") == 0) break;
      if (equal(name, ext)) break;
      name++;
   }
   if (name == NULL)  /* dotless or `ext'less name */
      return s_name;
   else { /* OK we have to build a copy */
      length = strlen(s_name)-strlen(name)+1;
      if (length > MAXSTRLEN) fatal("String too long!\n");
      charspace = (char *) malloc(length);
      if (charspace == NULL)
         fatal("Error in allocating space for a basename\n");
      strncpy(charspace, s_name, length);
      *(charspace+length-1) = '\0';
      return charspace;
   }
}

