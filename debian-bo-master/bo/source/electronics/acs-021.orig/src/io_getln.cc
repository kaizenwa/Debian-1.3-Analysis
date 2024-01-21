/*$Id: io_getln.cc,v 11.28 96/03/03 23:07:53 al Exp $ -*- C++ -*-
 * get a bunch of lines, from a file
 * interface is just line fgets.
 * hooks together extension lines
 * not recommended for getting from stdin.  use for files only.
 * is always a line ahead
 * start with + is extension line, spice compatibility.
 */
#include "md.h"
#include "error.h"
#include "l_lib.h"
/*--------------------------------------------------------------------------*/
	char 	*getlines(char*,int,FILE*);
/*--------------------------------------------------------------------------*/
extern const char e_int[];
/*--------------------------------------------------------------------------*/
char *getlines(char *buffer, int bufsize, FILE *fileptr)
{
  int count = 0;
  int more;
  char *got;
  
  do {
    got = fgets(&buffer[count], bufsize-count, fileptr);
    if (!got){
      if (count == 0)
	return (char*)NULL;
      else
	error(bWARNING, e_int, "getlines");
    }
    trim(buffer);
    count = strlen(buffer);
    if (buffer[count-1] == '\\'){
      count--;
      more = true;
    }else{
      int c = fgetc(fileptr);
      if (c == '+'){
	more = true;
      }else{
	more = false;
	ungetc(c,fileptr);
      }
    }
    if (count >= bufsize-1)
      break;
    buffer[count++] = ' ';
  } while (more);
  
  return buffer;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
