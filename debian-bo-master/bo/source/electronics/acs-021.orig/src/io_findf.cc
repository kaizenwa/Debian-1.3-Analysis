/*$Id: io_findf.cc,v 11.22 96/02/18 11:43:53 al Exp $ -*- C++ -*-
 * Modified by AD.  Sent to me by C-WARE
 * This file contains the routine to locate a file,
 *	using a path string for the directories to search.
 * Interface:
 *	findfile(filename, paths, mode)
 *	    filename is the name of the file to be searched for,
 *	    paths is the path to follow to find it.
 *	    mode is how you want to open the file
 *	returns full path name, if successful, else NULL.
*/
#include "md.h"
#include "constant.h"
/*--------------------------------------------------------------------------*/
	char*	findfile(const char*,const char*,int);
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
char *findfile(const char *filename, const char *paths, int mode)
{
  const char *p_ptr;
  static char target_buf[MAXPATHLEN];

#if CHECK_LOCAL_FIRST
  strcpy(target_buf, filename);	    /* check in the local directory */
  if (access(target_buf, mode) == GOOD)
    return target_buf;
#endif
  
  if (!paths)
    paths = "";
  
  for (p_ptr=paths;  *p_ptr != '\0';   ) {  /* for each item in the path ...*/
    char *t_ptr;
    t_ptr = target_buf;			    /* copy the directory name	    */
    while (*p_ptr != PATHSEP  &&  *p_ptr != '\0')
      *t_ptr++ = *p_ptr++;
    if (t_ptr != target_buf  &&  !strchr(ENDDIR,t_ptr[-1]))
      *t_ptr++ = *ENDDIR;		    /* append '/' if needed	    */
    *t_ptr = '\0';
    
    strcat(target_buf, filename);
    if (access(target_buf, mode) == GOOD)
      return target_buf;
    if (*p_ptr)		    		    /* beyond the SEP		    */
      p_ptr++;
  }
  return (char*)NULL;			    /* can't find one		    */
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
