/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                                                                             %
%                            V   V  M   M  SSSSS                              %
%                            V   V  MM MM  SS                                 %
%                            V   V  M M M   SSS                               %
%                             V V   M   M     SS                              %
%                              V    M   M  SSSSS                              %
%                                                                             %
%                    VMS Utility Routines for ImageMagick.                    %
%                                                                             %
%                                                                             %
%                               Software Design                               %
%                                 John Cristy                                 %
%                                October 1994                                 %
%                                                                             %
%                                                                             %
%  Copyright 1997 E. I. du Pont de Nemours and Company                        %
%                                                                             %
%  Permission to use, copy, modify, distribute, and sell this software and    %
%  its documentation for any purpose is hereby granted without fee,           %
%  provided that the above Copyright notice appear in all copies and that     %
%  both that Copyright notice and this permission notice appear in            %
%  supporting documentation, and that the name of E. I. du Pont de Nemours    %
%  and Company not be used in advertising or publicity pertaining to          %
%  distribution of the software without specific, written prior               %
%  permission.  E. I. du Pont de Nemours and Company makes no representations %
%  about the suitability of this software for any purpose.  It is provided    %
%  "as is" without express or implied warranty.                               %
%                                                                             %
%  E. I. du Pont de Nemours and Company disclaims all warranties with regard  %
%  to this software, including all implied warranties of merchantability      %
%  and fitness, in no event shall E. I. du Pont de Nemours and Company be     %
%  liable for any special, indirect or consequential damages or any           %
%  damages whatsoever resulting from loss of use, data or profits, whether    %
%  in an action of contract, negligence or other tortious action, arising     %
%  out of or in connection with the use or performance of this software.      %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  The directory routines are strongly based on similiar routines written
%  by Rich Salz.
%
*/

#if defined(vms)
/*
  Include declarations.
*/
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "vms.h"

/*
  Function closedir closes the named directory stream and frees the DIR
  structure.
*/
void closedir(DIR *directory)
{
  assert(directory != (DIR *) NULL);
  free(directory->pattern);
  free((char *) directory);
}
/*
  Return a pointer to the first occurrence of a character within a string.
*/
char *index(char *string,char c)
{
  assert(string != (char *) NULL);
  return(strchr(string,c));
}
/*
  Function opendir opens the directory named by filename and associates
  a directory stream with it.
*/
DIR *opendir(char *name)
{
  DIR
    *directory;

  /*
    Allocate memory for handle and the pattern.
  */
  directory=(DIR *) malloc(sizeof(*directory));
  if (directory == (DIR *) NULL)
    {
      errno=ENOMEM;
      return((DIR *) NULL);
    }
  if (strcmp(".",name) == 0)
    name="";
  directory->pattern=malloc((unsigned int) (strlen(name)+sizeof("*.*")+1));
  if (directory->pattern == (char *) NULL)
    {
      free((char *) directory);
      errno=ENOMEM;
      return(NULL);
    }
  /*
    Initialize descriptor.
  */
  (void) sprintf(directory->pattern,"%s*.*",name);
  directory->context=0;
  directory->pat.dsc$a_pointer=directory->pattern;
  directory->pat.dsc$w_length=strlen(directory->pattern);
  directory->pat.dsc$b_dtype=DSC$K_DTYPE_T;
  directory->pat.dsc$b_class=DSC$K_CLASS_S;
  return(directory);
}
/*
  Function readdir returns a pointer to a structure representing the
  directory entry at the current position in the directory stream to
  which entry refers.
*/
struct dirent *readdir(DIR *directory)
{
  char
    buffer[sizeof(directory->entry.d_name)];

  int
    status;

  register char
    *p;

  register int
    i;

  struct dsc$descriptor_s
    result;

  /*
    Initialize the result descriptor.
  */
  result.dsc$a_pointer=buffer;
  result.dsc$w_length=sizeof(buffer)-2;
  result.dsc$b_dtype=DSC$K_DTYPE_T;
  result.dsc$b_class=DSC$K_CLASS_S;
  status=lib$find_file(&directory->pat,&result,&directory->context);
  if ((status == RMS$_NMF) || (directory->context == 0L))
    return((struct dirent *) NULL);
  /*
    Lowercase all filenames.
  */
  buffer[sizeof(buffer)-1]='\0';
  for (p=buffer; *p; p++)
    if (isupper(*p))
      *p=tolower(*p);
  /*
    Skip any directory component and just copy the name.
  */
  p=buffer;
  while (!isspace(*p))
    p++;
  *p='\0';
  p=strchr(buffer,']');
  if (p)
    (void) strcpy(directory->entry.d_name,p+1);
  else
    (void) strcpy(directory->entry.d_name,buffer);
  /*
    Remove the file version number.
  */
  p=strchr(directory->entry.d_name,';');
  if (p)
    *p='\0';
  directory->entry.d_namlen=strlen(directory->entry.d_name);
  return(&directory->entry);
}
#endif
