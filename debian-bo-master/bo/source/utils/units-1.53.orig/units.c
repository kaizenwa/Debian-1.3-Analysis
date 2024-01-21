#define VERSION "1.53"

/*
 *  units, a program for units conversion
 *  Copyright (C) 1996, 1997 Free Software Foundation, Inc
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 * 
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *     
 *
 *  This program was written by Adrian Mariano (adrian@cam.cornell.edu)
 *
 */

#include <stdio.h>
#include <errno.h>

#ifdef STRINGS_H
#  include <strings.h>
#else
#  include <string.h>
#endif 

#ifndef NO_STDLIB_H
#  include <stdlib.h>
#else
   char *malloc(),  *realloc(),  *getenv();
#endif

#ifndef strchr
#  ifdef NO_STRCHR
#    define strchr(a,b) index((a),(b))
#  else
     char *strchr();
#  endif
#endif /* !strchr */

char *strtok(); 

#ifdef READLINE
#  include <readline/readline.h>
#  define RVERSTR "with readline"
#else
#  define RVERSTR "without readline"
#endif

#include "getopt.h"

#ifndef __STDC__
#  define void char
#endif

#ifndef UNITSFILE
#  define UNITSFILE "units.dat"
#endif

#define MAXSUBUNITS 100		/* Size of internal unit reduction buffer */

#define PRIMITIVECHAR '!'	/* Character that marks irreducible units */

char *numformat = "%.8g";	/* printf format for numeric output */
char *powerstring = "^";	/* Exponent character used in output */
int quiet = 0;                  /* Flag for supressing prompting (-q option) */
int unitcheck = 0;              /* Flag for unit checking, set to 1 for 
                                   regular check, set to 2 for verbose check */
int verbose = 0;                /* Flag for verbose operation */
char *userfile = NULL;          /* User specified units file name */
char *progname="units";         /* Used in error messages */

#define  HASHSIZE 101           /* Straight from K&R */
#define  HASHNUMBER 31

#define  PREFIXTABSIZE 128
#define  prefixhash(str) (*(str) & 127)    /* "hash" value for prefixes */


/* Hash table for unit definitions. */

struct unitlist {
   char *name;			/* unit name */
   char *value;			/* unit value */
   struct unitlist *next;	/* next item in list */
} *utab[HASHSIZE];


/* Table for prefix definitions. */

struct prefixlist {
   int len;			/* length of name string */
   char *name;			/* prefix name */
   char *value;			/* prefix value */
   struct prefixlist *last;	/* last item in list--only set in first item */
   struct prefixlist *next;   	/* next item in list */
} *ptab[PREFIXTABSIZE];


/* Data type used to store a single unit being operated on. 

   The numerator and denominator arrays contain lists of units
   (strings) which are terminated by a null pointer.  The special
   string NULLUNIT is used to mark blank units that occur in the
   middle of the list.  

*/

char *NULLUNIT = "";

struct unittype {
   char *numerator[MAXSUBUNITS];
   char *denominator[MAXSUBUNITS];
   double factor;
};


/* Increases the buffer by BUFGROW bytes and leaves the new pointer in buf
   and the new buffer size in bufsize. */

#define BUFGROW 10/*24*/  /* For debugging purposes, buffers grow very slowly*/

void
growbuffer(char **buf, int *bufsize)
{
  int usemalloc;

  usemalloc = !*buf || !*bufsize;
  *bufsize += BUFGROW;
  if (usemalloc)
    *buf = malloc(*bufsize);
  else
    *buf = realloc(*buf,*bufsize);
  if (!*buf){
    fprintf(stderr, "%s: memory allocation error (growbuffer)\n",progname);  
    exit(3); 
  }
}


/* Gets arbitrarily long input data into a buffer using growbuffer(). 
   Returns 0 if no data is read. */

char *
fgetslong(char **buf, int *bufsize, FILE *file)
{
   if (!*bufsize) growbuffer(buf,bufsize);
   if (!fgets(*buf, *bufsize, file))
     return 0;
   while ((*buf)[strlen(*buf)-1] != '\n' && !feof(file)){
     growbuffer(buf, bufsize);
     fgets(*buf+strlen(*buf), *bufsize-strlen(*buf), file);
   }   
   return *buf;
}

/* Allocates memory and aborts if malloc fails. */

void *
mymalloc(int bytes,char *mesg)
{
   void *pointer;

   pointer = malloc(bytes);
   if (!pointer){
     fprintf(stderr, "%s: memory allocation error %s\n", progname,mesg);
     exit(3);
   }
   return pointer;
}


/* Duplicates a string */

char *
dupstr(char *str)
{
   char *ret;

   ret = mymalloc(strlen(str) + 1,"(dupstr)");
   strcpy(ret, str);
   return (ret);
}


/* hashing algorithm for units */

unsigned
uhash(const char *str)
{
   unsigned hashval;

   for (hashval = 0; *str; str++)
      hashval = *str + HASHNUMBER * hashval;
   return (hashval % HASHSIZE);
}


/* Lookup a unit in the units table.  Returns the definition, or NULL
   if the unit isn't found in the table. */

struct unitlist *
ulookup(const char *str)
{
   struct unitlist *uptr;

   for (uptr = utab[uhash(str)]; uptr; uptr = uptr->next)
      if (strcmp(str, uptr->name) == 0)
	 return uptr;
   return NULL;
}

/* Lookup a prefix in the prefix table.  Finds the first prefix that
   matches the beginning of the input string.  Returns NULL if no
   prefixes match. */

struct prefixlist *
plookup(const char *str)
{
   struct prefixlist *prefix;

   for (prefix = ptab[prefixhash(str)]; prefix; prefix = prefix->next) {
      if (!strncmp(str, prefix->name, prefix->len))
	 return prefix;
   }
   return NULL;
}


/* Print out error message encountered while reading the units file. */

void
readerror(int linenum, const char *filename)
{
   fprintf(stderr, "%s: error in units file '%s' line %d\n",
	   progname, filename, linenum);
}


/* Read in units data.  

   If unitcheck is set then primitive units are set to "1" so all reductions
   should give a plain number.  */

void
readunits(char *userfile)
{
   struct prefixlist *pfxptr;
   struct unitlist *uptr;
   FILE *unitfile;
   char *line, *lineptr, *unitsfilename, *unitdef, *unitname;
   int len, linenum, linebufsize;
   unsigned hashval, pval;
   int unitcount, prefixcount;

   unitcount = 0;
   prefixcount = 0;
   linenum = 0;
   linebufsize = 0;

   growbuffer(&line,&linebufsize);

   if (userfile) {
      unitfile = fopen(userfile, "rt");
      if (!unitfile) {
	 fprintf(stderr, "%s: unable to open units file '%s'.  ",
		 progname, userfile);
         perror(0);
	 exit(1);
      }
      unitsfilename = userfile;
   } else {
      unitfile = fopen(UNITSFILE, "rt");
      unitsfilename = UNITSFILE;
      if (!unitfile) {
	 char *direc, *env;
	 char *filename;
	 char separator[2];

	 env = getenv("PATH");
         filename = mymalloc(strlen(env)+strlen(UNITSFILE)+2,"(readunits)");
	 if (env) {
	    if (strchr(env, ';'))	/* MS-DOS */
	       strcpy(separator, ";");
	    else		        /* UNIX */
	       strcpy(separator, ":");
	    direc = strtok(env, separator);
	    while (direc) {
	       strcpy(filename, "");
	       strcat(filename, direc);
	       strcat(filename, "/");
	       strcat(filename, UNITSFILE);
	       unitfile = fopen(filename, "rt");
	       if (unitfile){
                  unitsfilename = dupstr(filename);
		  break;
	       }
	       direc = strtok(NULL, separator);
	    }
	 }
	 if (!unitfile) {
	    fprintf(stderr, "%s: can't find units file '%s'\n",
		    progname, UNITSFILE);
	    exit(1);
	 }
         free(filename);
      }
   }
   while (!feof(unitfile)) {
      if (!fgetslong(&line, &linebufsize, unitfile)) 
        break;
      linenum++;
      if (*line == '/')    /* units file comment */
	 continue;
      if (lineptr = strchr(line,'#'))
         *lineptr = 0;
      unitname = strtok(line, " \n\t");
      if (!unitname || !*unitname) 
	continue;
      unitdef = strtok(NULL, "\n");
      if (!unitdef){
        readerror(linenum,unitsfilename);
        continue;
      }
      unitdef += strspn(unitdef," \t"); /* Remove leading white space */
      lineptr = unitdef+strlen(unitdef)-1;
      while (strchr("\t ",*lineptr))  /* Remove trailing white space */
        *(lineptr--) = 0;
      if (!*unitdef){ 
        readerror(linenum,unitsfilename);
        continue;
      }
      
      len = strlen(unitname);      
      if (unitname[len - 1] == '-') {	/* it's a prefix definition */
         unitname[len - 1] = 0;
	 if (pfxptr = plookup(unitname)) {  /* already there: redefinition */
	    if (!strcmp(pfxptr->name, unitname))
	       fprintf(stderr,
   	       "%s: redefinition of prefix '%s-' on line %d ignored.\n",
		       progname, unitname, linenum);
	    else
	       fprintf(stderr, "%s: prefix '%s-' on line %d is hidden by earlier definition of '%s-'.\n",
		       progname, unitname, linenum, pfxptr->name);
	    continue;
	 } 

         /* Make new prefix table entry */
         
         pfxptr = (struct prefixlist *) mymalloc(sizeof(*pfxptr),"(readunits)");
	 pfxptr->name = dupstr(unitname);
	 pfxptr->len = len - 1;
	 pfxptr->value = dupstr(unitdef);
         if (unitcheck && strchr(unitdef,'/'))
            printf("Warning: prefix '%s-' defined as '%s' contains a '/'\n",
                   unitname, unitdef);
	 /*
	    Install prefix name/len/value in list
	    Order is FIFO, so a prefix that is a substring of another
	    prefix must follow the longer prefix in the units file
	    (e.g, 'k' must come after 'kilo') or it will not be found
	    by plookup().
	  */

	 pfxptr->next = NULL;
	 pval = prefixhash(pfxptr->name);
	 if (ptab[pval] == NULL)
	    ptab[pval] = pfxptr;
	 else
	    ptab[pval]->last->next = pfxptr;
	 ptab[pval]->last = pfxptr;
	 prefixcount++;

      } else {	/* it is a unit definition */

         /* Is it a redefinition? */

	 if (ulookup(unitname)) {
	    fprintf(stderr,
		    "%s: redefinition of unit '%s' on line %d ignored\n",
		    progname, unitname, linenum);
    	    continue;
	 }

         /* make new units table entry */

         uptr = (struct unitlist *) mymalloc(sizeof(*uptr),"(readunits)");
	 uptr->name = dupstr(unitname);
         if (unitcheck && strchr(unitdef,PRIMITIVECHAR)) 
           uptr->value = dupstr("1");
         else
  	   uptr->value = dupstr(unitdef);

	 /* install unit name/value pair in list */

	 hashval = uhash(uptr->name);
	 uptr->next = utab[hashval];
	 utab[hashval] = uptr;
	 unitcount++;
      }
   }
   fclose(unitfile);
   free(line);
   if (!quiet)
     printf("%d units, %d prefixes\n\n", unitcount, prefixcount);
}

/* Initialize a unit to be equal to 1. */

void
initializeunit(struct unittype *theunit)
{
   theunit->factor = 1.0;
   theunit->numerator[0] = theunit->denominator[0] = NULL;
}


/* Free a unit: frees all the strings used in the unit structure.
   Does not free the unit structure itself.  */

void
freeunit(struct unittype *theunit)
{
   char **ptr;

   for(ptr = theunit->numerator; *ptr; ptr++)
     if (*ptr != NULLUNIT) free(*ptr);
   for(ptr = theunit->denominator; *ptr; ptr++)
     if (*ptr != NULLUNIT) free(*ptr);
}


void
productoverflow()
{
  printf("\tBuffer overflow in unit reduction.  This means you formed a");
  printf("\n\tproduct of over %d units.  If you really need more, you can",
                     MAXSUBUNITS);
  printf("\n\tchange MAXSUBUNITS and recompile this program.\n");
}


/* Add a unit string onto the array of strings representing a product
   of units.  The new string will be added at the first NULLUNIT or at
   the end (which is marked by a NULL pointer. */

int
addsubunit(char *product[], char *toadd)
{
   char **ptr;

   for (ptr = product; *ptr && *ptr != NULLUNIT; ptr++);
   if (ptr >= product + MAXSUBUNITS) {
      productoverflow();
      return 1;
   }
   if (!*ptr)
      *(ptr + 1) = 0;
   *ptr = dupstr(toadd);
   return 0;
}

/* Adds one product list to another */

int
addsubunitlist(char *product[], char *toadd[])
{
   char **dest, **src;

   dest=product;
   for(src = toadd; *src; src++){
     if (*src == NULLUNIT) continue;
     for(; *dest && *dest != NULLUNIT; dest++);
     if (dest >= product + MAXSUBUNITS) {
       productoverflow();
       return 1;
     }
     if (!*dest)
        *(dest + 1) = '\0';
     *dest = dupstr(*src);
   }
   return 0;
}


/* Print out a unit  */

void
showunit(struct unittype *theunit)
{
   char **ptr;
   int printedslash;
   int counter = 1;

   printf(numformat, theunit->factor);

   for (ptr = theunit->numerator; *ptr; ptr++) {
      if (ptr > theunit->numerator && **ptr &&
	  !strcmp(*ptr, *(ptr - 1)))
	 counter++;
      else {
	 if (counter > 1)
	    printf("%s%d", powerstring, counter);
	 if (**ptr)
	    printf(" %s", *ptr);
	 counter = 1;
      }
   }
   if (counter > 1)
      printf("%s%d", powerstring, counter);
   counter = 1;
   printedslash = 0;
   for (ptr = theunit->denominator; *ptr; ptr++) {
      if (ptr > theunit->denominator && **ptr &&
	  !strcmp(*ptr, *(ptr - 1)))
	 counter++;
      else {
	 if (counter > 1)
	    printf("%s%d", powerstring, counter);
	 if (**ptr) {
	    if (!printedslash)
	       printf(" /");
	    printedslash = 1;
	    printf(" %s", *ptr);
	 }
	 counter = 1;
      }
   }
   if (counter > 1)
      printf("%s%d", powerstring, counter);
   printf("\n");
}

/* Error to print when a unit reduction in a numerator or denominator
   leads to zero. */

void 
zeroerror()
{
   fprintf(stderr, "Unit is zero, infinity, or indeterminate.\n");
}


/* 
   Finds the length of the largest prefix of the argument which
   is a number.  A number has the form of a sequence of combinations
   that are regular numbers separated by '|' characters.  No spaces  
   are allowed and the string must not start or end with '|'.
*/


int
numlength(char *str)
{
   float dummy;
   int length,answer,backup;

   backup=0;
   answer=0;
   for(;;){
     if (1 != sscanf(str,"%f%n",&dummy,&length)) {
       answer-=backup;
       break;
     }
     answer+=length;
     str+=length;
     if (*str != '|') break;
     str++;
     answer++;
     backup = 1;
   }
   return answer;
}


/* qsort comparison function */

int
compare(const void *item1, const void *item2)
{
   return strcmp(*(char **) item1, *(char **) item2);
}

/* Sort numerator and denominator of a unit so we can compare different
   units */

void
sortunit(struct unittype *theunit)
{
   char **ptr;
   int count;

   for (count = 0, ptr = theunit->numerator; *ptr; ptr++, count++);
   qsort(theunit->numerator, count, sizeof(char *), compare);
   for (count = 0, ptr = theunit->denominator; *ptr; ptr++, count++);
   qsort(theunit->denominator, count, sizeof(char *), compare);
}


/* Cancels duplicate units that appear in the numerator and
   denominator.  The input unit must be sorted. */

void
cancelunit(struct unittype *theunit)
{
   char **den, **num;
   int comp;

   den = theunit->denominator;
   num = theunit->numerator;

   while (*num && *den) { 
      comp = strcmp(*den, *num);
      if (!comp) { /* units match, so cancel them */
         if (*den!=NULLUNIT) free(*den);
         if (*num!=NULLUNIT) free(*num);  
	 *den++ = NULLUNIT;
	 *num++ = NULLUNIT;
      } else if (comp < 0) /* Move up whichever pointer is alphabetically */
	 den++;            /* behind to look for future matches */
      else
	 num++;
   }
}


/*
   Looks up the definition for the specified unit including prefix processing
   and plural removal.

   Returns a pointer to the definition or a null pointer
   if the specified unit does not appear in the units table.

   Sometimes the returned pointer will be a pointer to the special
   buffer created to hold the data.  This buffer grows as needed during 
   program execution.  */

static int bufsize=0;
static char *buffer;  /* buffer for lookupunit answers with prefixes */

char *
lookupunit(char *unit,int prefixok)
{
   char *copy;
   struct prefixlist *pfxptr;
   struct unitlist *uptr;

   if (numlength(unit) == strlen(unit)) {
      while(strlen(unit)+1 > bufsize) growbuffer(&buffer, &bufsize);
      strcpy(buffer,unit);
      return buffer;
   }

   if (uptr = ulookup(unit))
      return uptr->value;

   if (prefixok && (pfxptr = plookup(unit))) {
      copy=unit;
      copy += pfxptr->len;
      if (!strlen(copy) || lookupunit(copy,0)) {
         char *tempbuf;
         while (strlen(pfxptr->value)+strlen(copy)+2 > bufsize){
            growbuffer(&buffer, &bufsize);
         }
         tempbuf = strdup(copy);   /* copy might point into buffer */
	 strcpy(buffer, pfxptr->value);
	 strcat(buffer, " ");
	 strcat(buffer, tempbuf);
         free(tempbuf);
	 return buffer;
      }
   }
   if (strlen(unit)>2 && unit[strlen(unit) - 1] == 's') {
      copy = dupstr(unit);
      copy[strlen(copy) - 1] = 0;
      if (lookupunit(copy,prefixok)){
         while(strlen(copy)+1 > bufsize) {
            growbuffer(&buffer, &bufsize);
         }
         strcpy(buffer, copy);  /* Note: returning looked up result seems   */
	 free(copy);		/*   better but it causes problems when it  */
         return buffer;		/*   contains PRIMITIVECHAR.                */
      }
      if (strlen(copy)>2 && copy[strlen(copy) - 1] == 'e') {
	 copy[strlen(copy) - 1] = 0;
	 if (lookupunit(copy,prefixok)){
	    while (strlen(copy)+1 > bufsize) {
	       growbuffer(&buffer,&bufsize);
	    }
            strcpy(buffer,copy);
	    free(copy);
            return buffer;
	 }
      }
      free(copy);
   }
   return 0;
}


void
removespaces(char *in, char magic)
{
  char *src, *dest;
  
  for(dest=src=in; *src; dest++, src++){
    if (*src==magic) {
      while(dest-in>0 && strchr(" \t",*(dest-1))) dest--;
      while(*(src+1) && strchr(" \t",*(src+1))) src++;
      *src=magic;
    }
    if (src!=dest) *dest=*src;
  }
  *dest=0;
}


/* 
   Returns 1 if the input consists entirely of whitespace characters
   and returns 0 otherwise. 
*/

int
isblankstr(char *str)
{
  while(*str) if (!strchr(" \t\n",*str++)) return 0;
  return 1;
}

/* Remove leading and trailing white space from the input */

char *
removepadding(char *in)
{
   in += strspn(in," \t\n");
   while(strchr(" \t\n",in[strlen(in)-1])) in[strlen(in)-1]=0;
   return in;
}


/* Add a number to the unit.  flip is zero for numerator, 1 for denominator.
   Returns zero on success, 1  on failure.
*/

int
addnumber(struct unittype *theunit, int flip, char  *item)
{            
  double number;
  int len;
  
  sscanf(item,"%lf%n",&number,&len);
  if (!number) {
     zeroerror();
     return  1;
  }
  if (flip)
    theunit->factor *= number;
  else
    theunit->factor /= number;
  item += len;
  while(*item){
    if (*item != '|') {
      fprintf(stderr,"Mysterious number error, should not happen\n");
      return 1;
    }
    item++;
    sscanf(item,"%lf%n",&number,&len);
    if (!number) {
      zeroerror();
      return  1;
    }
    if (flip)
      theunit->factor /= number;
    else
      theunit->factor *= number;
    item+=len;
  }
  return 0;
}



/*
   Parses the given string as a product of units and adds them to 
   the unit (by multiplication).

   Flip is 0 for normal product, 1 for taking reciprocal.

   Returns 0 for successful addition, nonzero on error.
*/


int
addunit(struct unittype *theunit, char *toadd, int flip)
{
   char *scratch, *savescr;
   char *item;
   char *slash;
   int doingtop;


   for(scratch=toadd;*scratch;scratch++)
     if (scratch>toadd && *scratch=='+' && !strchr("eE",*(scratch-1))) 
        return handlesum(theunit,toadd,flip);

   removespaces(toadd,'^');   

   savescr = scratch = dupstr(toadd);

   /* Changes all '-' characters to space unless after an 'e' as in 4.3e-5 */
   for (slash = scratch + 1; *slash; slash++)
      if (*slash == '-' && (!strchr("eE",*(slash - 1)) ||
			    !strchr(".0123456789", *(slash + 1))))
	 *slash = ' ';
   slash = strchr(scratch, '/');
   if (slash)
      *slash = 0;
   doingtop = 1;
   do {
      item = strtok(scratch, " *\t\n/");
      while (item) {
         if (numlength(item) == strlen(item)){  /* item is a number */
            if (addnumber(theunit,doingtop^flip,item)) {
              free(savescr);
              return 1;
            }
	 } else {		/* item is not a number */
	    int repeat = 1;
            char *power;

            if (power = strchr(item,'^')) {
               if (power == item+strlen(item)-1) {
                 printf("Expression '%s' has no exponent.\n",item);
                 free(savescr);
                 return 1;
               }
               *power++ = 0;
               if (!strlen(item)) {
                 printf("Expression '^%s' has no left side.\n",power);
                 free(savescr);
                 return 1;
               }
               if (strspn(power,"0123456789") != strlen(power)) {
                 printf("Exponent '%s' is not an integer.\n",power);
                 free(savescr);
                 return 1;
               }
               repeat = atoi(power);
            }
	    else if (strchr("23456789", item[strlen(item) - 1])) {
	       repeat = item[strlen(item) - 1] - '0';
	       item[strlen(item) - 1] = 0;
	    }
            if (!lookupunit(item,1)) {
               int numlen;
               char *temp;
               numlen=numlength(item);
               if (numlen){
                 temp=dupstr(item);
                 temp[numlen]=0;
	         addnumber(theunit,doingtop ^ flip,temp);
                 free(temp);
                 item+=numlen;
               }
            }
	    for (; repeat; repeat--)
	       if (addsubunit(doingtop ^ flip ? theunit->numerator : 
                          theunit->denominator, item)){
                  free(savescr);
		  return 1;
               }
	 }
	 item = strtok(NULL, " *\t/\n");
      }
      doingtop--;
      if (slash) {
	 scratch = slash + 1;
      } else
	 doingtop--;
   } while (doingtop >= 0);
   free(savescr);
   return 0;
}


/* 
   Adds a unit expression which contains '+' characters to 
   the unit structure by completely reducing each subexpression.
*/

int
handlesum(struct unittype *theunit, char *toadd, int flip)
{
  struct unittype firstunit, nextunit;
  char *first, *text, *rest;
  int retval;
  initializeunit(&firstunit);
  first=strdup(toadd);
  rest=strchr(first,'+');   
  if (rest) *rest++=0;      
  if (isblankstr(first)){
    free(first);
    printf("Encountered '+' without operand.\n");
    return 1;
  }
  if (addunit(&firstunit,first,flip) || completereduce(&firstunit)) {
    free(first);
    return 1;
  }
  while(rest){
    text=rest;
    rest=strchr(text,'+');
    if (rest) *rest++=0;
    if (isblankstr(text)){
      printf("Encountered '+' without operand.\n");
      return 1;
    }
    initializeunit(&nextunit);
    if (addunit(&nextunit,text,flip) || completereduce(&nextunit)) {
      free(first);
      return 1;
    }
    if (compareunits(&firstunit,&nextunit)){
      printf("Illegal sum of non-conformable units:\n");
      first = removepadding(first);
      text = removepadding(text);
      printf("\t%s reduces to ",first);
      showunit(&firstunit);
      printf("\t%s reduces to ",text);
      showunit(&nextunit);
      free(first);
      return 1;
    }
    firstunit.factor  += nextunit.factor;
    freeunit(&nextunit);
  }

  retval = addsubunitlist(theunit->numerator,firstunit.numerator) ||
    addsubunitlist(theunit->denominator,firstunit.denominator);
  theunit->factor *= firstunit.factor;

  freeunit(&firstunit);
  free(first);
  return retval;
}


/*
   reduces a product of symbolic units to primitive units.
   The three low bits are used to return flags:

   bit 0 set if reductions were performed without error.
   bit 1 set if no reductions are performed.
   bit 2 set if an unknown unit is discovered.

   Return values from multiple calls will be ORed together later.
 */

#define DIDREDUCTION (1<<0)
#define NOREDUCTION  (1<<1)
#define ERROR        (1<<2)

int
reduceproduct(struct unittype *theunit, int flip)
{

   char *toadd;
   char **product;
   int didsomething = NOREDUCTION;

   if (flip)
      product = theunit->denominator;
   else
      product = theunit->numerator;

   for (; *product; product++) {

      for (;;) {
	 if (!strlen(*product))
	    break;
	 toadd = lookupunit(*product,1);
	 if (!toadd) {
	    printf("Unknown unit '%s'.", *product);
            if (strchr(*product,'|')){
              if (!strcmp(*product,"|")) 
                printf("  (Spaces are forbidden around '|'.)");
              else
                printf("  (Named units cannot be used with '|'.)");
            }
	    putchar('\n');
	    return ERROR;
	 }
	 if (strchr(toadd, PRIMITIVECHAR))
	    break;
	 didsomething = DIDREDUCTION;
	 if (*product != NULLUNIT) {
	    free(*product);
	    *product = NULLUNIT;
	 }
	 if (addunit(theunit, toadd, flip))
	    return ERROR;
      }
   }
   return didsomething;
}


/*
   Reduces numerator and denominator of the specified unit.
   Returns 0 on success, or 1 on unknown unit error.
 */

int
reduceunit(struct unittype *theunit)
{
   int ret;

   ret = DIDREDUCTION;

   /* Keep calling reduceprodut until it doesn't do anything */

   while (ret & DIDREDUCTION) {
      ret = reduceproduct(theunit, 0) | reduceproduct(theunit, 1);
      if (ret & ERROR)
	 return 1;
   }
   return 0;
}

/* Compare two product lists, return zero if they match and one if
   they do not match.  They may contain embedded NULLUNITs which are
   ignored in the comparison. */

int
compareproducts(char **one, char **two)
{
   while (*one || *two) {
      if (!*one && *two != NULLUNIT)
	 return 1;
      if (!*two && *one != NULLUNIT)
	 return 1;
      if (*one == NULLUNIT)
	 one++;
      else if (*two == NULLUNIT)
	 two++;
      else if (strcmp(*one, *two))
	 return 1;
      else
	 one++, two++;
   }
   return 0;
}


/* Return zero if units are compatible, nonzero otherwise.  The units
   must be reduced, sorted and canceled for this to work.  */

int
compareunits(struct unittype *first, struct unittype *second)
{
   return
      compareproducts(first->numerator, second->numerator) ||
      compareproducts(first->denominator, second->denominator);
}


/* Reduce a unit as much as possible */

int
completereduce(struct unittype *unit)
{
   if (reduceunit(unit))
      return 1;
   sortunit(unit);
   cancelunit(unit);
   return 0;
}


/* 
   If the given character string has only one unit name in it, then print out
   the rule for that unit.  In any case, print out the reduced form for
   the unit.
*/

void
showdefinition(char *unitstr, struct unittype *theunit)
{
  char *first, *second;
  first = strtok(unitstr," \t\n-*^/+");
  second = strtok(NULL, " \t\n-*^/+");
  printf("\tDefinition: ");
  if (first && !second){
    second = lookupunit(first,1);
    if (second) {
      while(second && numlength(second)!=strlen(second) && 
           !strchr(second,PRIMITIVECHAR)) {
        printf("%s = ",second);
	second=lookupunit(second,1);
      } 
    }
  }
  showunit(theunit);
}


/* Show the conversion factors or print the conformability error message */

int
showanswer(char *havestr,struct unittype *have,
           char *wantstr,struct unittype *want)
{
   havestr = removepadding(havestr);
   wantstr = removepadding(wantstr);
   if (compareunits(have, want)) {
      printf("conformability error\n");
      if (verbose) printf("\t%s = ",havestr);
      else putchar('\t');
      showunit(have);
      if (verbose) printf("\t%s = ",wantstr);
      else putchar('\t');
      showunit(want);
      return -1;
   } else {
      if (verbose) printf("\t%s = ",havestr);
      else printf("\t* ");
      printf(numformat, have->factor / want->factor);
      if (verbose) {
        if (numlength(wantstr)>0) printf(" *");
        printf(" %s\n\t%s = (1 / ",wantstr,havestr);
      } else printf("\n\t/ ");
      printf(numformat, want->factor / have->factor);
      if (verbose){
        printf(")");
        if (numlength(wantstr)>0) printf(" *");
        printf(" %s",wantstr);
      }
      putchar('\n');
   }
   return 0;
}


/* 
   Cycle through all units and prefixes and attempt to reduce each one to 1.
   Print a message for all units which do not reduce to 1.  

   (When this function is called, all of the primitive units will have been 
   defined equal to 1, so all valid unit definitions should reduce to 1.)
*/

void 
checkunits()
{
  struct unittype have,one;
  struct unitlist *uptr;
  struct prefixlist *pptr;
  int i;

  initializeunit(&one);
  
  for(i=0;i<HASHSIZE;i++)
    for (uptr = utab[i]; uptr; uptr = uptr->next){
      if (unitcheck==2 || verbose)
        printf("doing '%s'\n",uptr->name);
      initializeunit(&have);
      addunit(&have,uptr->name,0);
      completereduce(&have);
      if (compareunits(&have,&one))
         printf("'%s' defined as '%s' irreducible\n",uptr->name,uptr->value);
      freeunit(&have);
    }
  for(i=0;i<PREFIXTABSIZE;i++)
    for(pptr = ptab[i]; pptr; pptr = pptr->next){
      initializeunit(&have);
      addunit(&have, pptr->name,0);
      completereduce(&have);
      if (compareunits(&have,&one))
         printf("'%s-' defined as '%s' irreducible\n",pptr->name,pptr->value);
      freeunit(&have);
    }
}

/* print usage message */

void 
usage()
{
   printf("Usage: %s [option] ['from-unit' 'to-unit']\n",progname);
   fputs("\
\n\
    -h, --help          print this help and exit\n\
    -c, --check         check that all units reduce to primitive units\n\
        --check-verbose like --check, but lists units as they are checked\n\
                          so you can find units that cause endless loops\n\
    -e, --exponential   exponential format output\n\
    -f, --file          specify units data file\n\
    -o, --output-format specify printf numeric output format\n\
    -q, --quiet         supress prompting\n\
        --silent        same as --quiet\n\
    -v, --verbose       print slightly more verbose output\n\
    -V, --version       print version number and exit\n", stdout);
   exit(3);
}

/* Print message about how to get help */

void 
helpmsg()
{
  fprintf(stderr,"Try `%s --help' for more information.\n",progname);
  exit(3);
}


/* If quiet is false then prompt user with the query.  

   Fetch one line of input and return it in *buffer.

   The bufsize argument is a dummy argument if we are using readline. 
   The readline version frees buffer if it is non-null.  The other
   version keeps the same buffer and grows it as needed.

   If no data is read, then this function exits the program. 
*/

#ifdef READLINE

void
getuser(char **buffer, int *bufsize, char *query)
{
  if (*buffer) free(*buffer);
  *buffer = readline(quiet?"":query);
  if (*buffer && **buffer) add_history(*buffer);
  if (!*buffer){
    if (!quiet)
       putchar('\n');
    exit(0);
  }
}

char *
completeunits(char *text, int state)
{
  static struct prefixlist *curprefix;
  static struct unitlist *curunit;
  static int uhash;
  char *output,*thistry;
  
  if (!state){
     uhash = 0;
     curprefix=0;
     curunit=utab[uhash];
  }
  output = 0;
  while (!output){
    while (!curunit  && uhash<HASHSIZE-1){
      uhash++;
      curunit = utab[uhash];
    }
    if (!curunit) return 0;
    thistry = text;
    if (curprefix)
       thistry+=curprefix->len;
    if (strlen(thistry)<=strlen(curunit->name) && 
           !strncmp(curunit->name,thistry,strlen(thistry))){
       output = (char *)mymalloc(1 + strlen(curunit->name)
                        + (curprefix ? curprefix->len:0),"(completeunits)");
       strcpy(output,curprefix?curprefix->name:"");
       strcat(output,curunit->name);
    }
    curunit = curunit->next;
    while (!curunit  && uhash<HASHSIZE-1){
      uhash++;
      curunit = utab[uhash];
    }
    if (!curunit && !curprefix){
      if (curprefix = plookup(text)){
        uhash = 0;
        curunit = utab[uhash];
      }
    }
  }    
  return output;
}

#else /* We aren't using READLINE */

void
getuser(char **buffer, int *bufsize, char *query)
{
  if (!quiet) fputs(query, stdout);
  if (!fgetslong(buffer, bufsize, stdin)){
    if (!quiet)
       putchar('\n');
    exit(0);
  }
}  


#endif /* READLINE */


char *shortoptions = "Vvqechf:o:";

struct option longoptions[] = {
  {"version", no_argument, 0, 'V'},
  {"quiet", no_argument, &quiet, 1},
  {"silent", no_argument, &quiet, 1},
  {"exponential", no_argument, 0, 'e'},
  {"check", no_argument, &unitcheck, 1},
  {"check-verbose", no_argument, &unitcheck, 2},
  {"verbose-check", no_argument, &unitcheck, 2},
  {"verbose", no_argument, &verbose, 1},
  {"file", required_argument, 0, 'f'},
  {"output-format", required_argument, 0, 'o'},
  {"help",no_argument,0,'h'},
  {0,0,0,0} };

/* Process the args.  Returns 1 if interactive mode is desired, and 0
   for command line operation.  If units appear on the command line
   they are returned in the from and to parameters. */

int
processargs(int argc, char **argv, char **from, char **to)
{
   extern char *optarg;
   extern int optind;
   int optchar, optindex;

   while ( -1 != 
      (optchar = 
         getopt_long(argc, argv,shortoptions,longoptions, &optindex ))) {
      switch (optchar) {
         case 'o':
            numformat = optarg;
            break;
         case 'c':
            unitcheck = 1;
            break;
	 case 'e':
	    numformat = "%6e";
	    break;
	 case 'f':
	    userfile = optarg;
	    break;
	 case 'q':
	    quiet = 1;
	    break;
         case 'v':
            verbose = 1;
            break;
	 case 'V':
	    printf("%s version %s %s, units database in %s\n",progname, 
                   VERSION, RVERSTR,UNITSFILE);
	    exit(3);
         case 0: break;  /* This is reached if a long option is 
                            processed with no return value set. */
         case '?':
	 case 'h':
	    usage();
	    break;
	 default:
            helpmsg();
      } 
   }

   if (optind == argc - 2) {
      quiet=1;
      *from = argv[optind];
      *to = argv[optind+1]; 
      return 0;
   }

   if (optind == argc - 1) {
      quiet=1;
      *from = argv[optind];
      *to=0;
      return 0;
   }
   if (optind < argc - 2) {
      fprintf(stderr,"Too many arguments (maybe you need quotes).\n");
      helpmsg();
   }
   return 1;
}

   

int
main(int argc, char **argv)
{
   struct unittype have, want;
   char *havestr=0, *wantstr=0;
   int havestrsize=0;   /* Only used if READLINE is undefined */
   int wantstrsize=0;   /* Only used if READLINE is undefined */
   int interactive;

#ifdef READLINE
/*   rl_bind_key ('\t', rl_insert);   Turn off filename completion */

   rl_completion_entry_function = (Function *)completeunits;
#endif

   interactive = processargs(argc, argv, &havestr, &wantstr);

   readunits(userfile);

   if (unitcheck) {
      checkunits();
      exit(0);
   }

   if (!interactive) {
      initializeunit(&have);
      if (addunit(&have, havestr, 0) || completereduce(&have))
	 exit(1);
      if (!wantstr){
         showdefinition(havestr,&have);
         exit(0);
      }
      initializeunit(&want);
      if (addunit(&want, wantstr, 0) || completereduce(&want))
	 exit(1);
      if (showanswer(havestr,&have,wantstr,&want))
	 exit(1);
      else
	 exit(0);
   } else {
      for (;;) {
	 do {
	    initializeunit(&have);
            getuser(&havestr,&havestrsize,"You have: ");
	 } while (isblankstr(havestr) || addunit(&have, havestr, 0) 
                  || completereduce(&have));
	 do {
	    initializeunit(&want);
            getuser(&wantstr,&wantstrsize,"You want: ");
	 } while (addunit(&want, wantstr, 0) || completereduce(&want));
         if (isblankstr(wantstr))
           showdefinition(havestr,&have);
         else
           showanswer(havestr,&have,wantstr, &want);
         freeunit(&have);
	 freeunit(&want);
      }
   }
   return (0);
}
