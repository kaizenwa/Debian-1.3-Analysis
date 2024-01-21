/* Find the full program name from argv[0].
   Copyright 1994, 1995 Tristan Gingold
		  Written July 1994 by Tristan Gingold

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License 
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

The author may be reached by US/French mail:
		Tristan Gingold 
		8 rue Parmentier
		F-91120 PALAISEAU
		FRANCE
*/
#include <unistd.h>
#include <fcntl.h>
#include <sys/param.h>
#include "checker.h"

#ifndef DEFAULT_PATH
#define DEFAULT_PATH ":/bin:/usr/bin:/usr/local/bin"
#endif
#ifndef DEFAULT_HOME
#define DEFAULT_HOME "/"
#endif

static void simplify_path (void);

/* The buffer where the full program name is saved.  */
static char exec_name_buffer [MAXPATHLEN];

/* Copy the file into the buffer and simplify it.  */
char *
copy_of_exec_file (char *file)
{
  strncpy (exec_name_buffer, file, MAXPATHLEN);
  simplify_path ();
  return exec_name_buffer;
}

#ifndef DONT_NEED_EXECUTABLE
/* This function is like strncat, but MAXLEN is the maximal length of DEST,
   not the maximal length of SRC.  */
static void
my_strncat(char *dest, char *src, int maxlen)
{
  int len = strlen(dest);
  strncpy (dest + len, src, maxlen - len - 1);	/* 1 for the '\0'? */
}

/* The main function: from FILE and the PATH, search the full name of FILE.
 */
char *
chkr_find_executable (char *file)
{
  char name[MAXPATHLEN];
  char *path;
  
  /* If FILE is already a full path, this is easy!  */
  if (file[0] == '/')
    return copy_of_exec_file (file);

  /* If FILE contains a '/', the current directory must be used.  */
  if (strchr(file, '/') != (char*)0)
    {
      chkr_getcwd (name, MAXPATHLEN);
      my_strncat (name, "/", MAXPATHLEN);
      my_strncat (name, file, MAXPATHLEN);
      
      if (access (name, X_OK) == 0)
        return copy_of_exec_file (name);
      /* Do not stop here. */
    }
  
  /* Get the path.  */
  path = chkr_getenv ("PATH");
  if (path == (char*)0)
    path = DEFAULT_PATH;
    
  /* Try each directory of the path.  */
  while (*path)
    {
      char *next = name;
      
      /* Test if we must insert a directory before.  */
      if (*path != '/' )
        {
          if (path[0] == '~' || path[1] == '/')
            {
              char *home = chkr_getenv ("HOME");
              if (home == (char*) 0)
                home = DEFAULT_HOME;
              strncpy (name, home, MAXPATHLEN);
              path++;	/* Skip the tilde. */
            }
          else
            chkr_getcwd (name, MAXPATHLEN);
          next = name + strlen(name);
          *next++='/';	/* Sometime useful with :./dir:/bin/:  */
        }
        
      /* Copy the directory into the name.  */
      while (*path && *path != ':')
        *next++ = *path++;
      *next = '\0';
      
      /* Skip the colon.  */
      if (*path)
        path++; 
      
      my_strncat (name, "/", MAXPATHLEN);
      my_strncat (name, file, MAXPATHLEN);
      
      if (access (name, X_OK) == 0)
        return copy_of_exec_file (name);
    }
    
  /* The current directory must always be tested, because the parent
     could use exeve() instead of execvp() for example.  */
  chkr_getcwd (name, MAXPATHLEN);
  my_strncat (name, "/", MAXPATHLEN);
  my_strncat (name, file, MAXPATHLEN);
  if (access (name, X_OK) == 0)
    return copy_of_exec_file (name);
  return 0;
}  
#endif /* !DONT_NEED_EXECUTABLE */

/* Simplify the path.  */
static void
simplify_path (void)
{
 char *path = exec_name_buffer;
 char *sl = path;
 char *sl1;
 
 if (*path != '/')
   return;	/* Too bad... */
   
 do
   {
     /* Find the first '/'. */
     while (*sl && *sl != '/')
       sl++;
       
     /* End of the path?  */
     if (*sl == '\0')
       return;
       
     do
       {
         /* NOTA: each time we use memmove, we must not forget the '\0'!  */
         /* "//" is simplified into "/".  */
         if (sl[1] == '/')
           memmove (sl, sl + 1, strlen(sl + 1) + 1);
         /* "/./" -> "/" */
         else if (sl[1] == '.' && sl[2] == '/')
           memmove (sl, sl + 2, strlen(sl + 2) + 1);
         /* "/x/../" -> "/" */
         else if (sl[1] == '.' && sl[2] == '.' && sl[3] == '/')
           {
             /* If the path really begins with "/../x", it is simplified
                into "/x".  */
             if (sl == path)
               memmove (sl, sl + 3, strlen(sl + 3) + 1);
             else
              {
                /* Find the previous "/".  */
                for (sl1 = sl - 1; sl1 >= path && *sl1 != '/'; sl1--)
                  ;
                if (*sl1 != '/' || sl1 == sl)
                  return; /* ERROR */
                memmove (sl1, sl + 3, strlen(sl + 3) + 1);
                sl = sl1;
                continue;
              }
           }
         /* No simplification.  */
         else break;
       }
     while (1);
     sl++;	/* Next directory. */
   }
 while (1);
}
