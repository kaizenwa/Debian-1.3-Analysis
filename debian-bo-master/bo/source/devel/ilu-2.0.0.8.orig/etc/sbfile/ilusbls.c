#include <stdio.h>
#include <string.h>

#if defined(WIN32)
#include <windows.h>
#include <io.h>		/* needed for _findfirst(), _findnext(), _findclose() */
#include <direct.h>	/* needed for _chdir() */
#include "iluwin.h"
#else
#include <dirent.h>
#include <unistd.h>
#include "iluconf.h"	/* for ILU_BINDING_DIRECTORY */
#endif

#define ILU_NIL	0

extern char *getenv(const char *name);

/* List the currently registered ILU objects */

static void usage (char *pname)
{
  fprintf (stderr, "Usage:  %s [PATTERN]\n", pname);
}

int main (int ac, char **av)
{
#if defined(WIN32)
	long hDirHandle;
	struct _finddata_t findFileStruct;
	char buf[260];
	BOOLEAN finding = FALSE;
#else
  DIR *d;
  struct dirent *ent;
#endif
  char filename[260];
  char *dirname;
  char *pattern = "";
  FILE *f;
  char sbh[2048];
  int count;

  if ((dirname = getenv("ILU_BINDING_DIRECTORY")) == ILU_NIL)
    dirname = ILU_BINDING_DIRECTORY;

#if defined(WIN32)
  strcpy(buf, dirname);
  strcat(buf, "\\*.*");
  if (_chdir(dirname))
  {
	  fprintf(stderr, "Can't cd to directory \"%s\".\n", dirname);
	  return (1);
  }
#else
  if ((d = opendir(dirname)) == ILU_NIL)
    {
      fprintf (stderr, "Can't open directory \"%s\".\n", dirname);
      return 1;
    }
  if (chdir(dirname) != 0)
    {
      fprintf (stderr, "Can't cd to directory \"%s\".\n", dirname);
      return 1;
    }
#endif

  if (ac > 1)
    pattern = av[1];

  /* count the number of entries */
  count = 0;
#if defined(WIN32)
  if ((hDirHandle = _findfirst(buf, &findFileStruct)) != -1L)
  {
	finding = TRUE;
  }
  while (finding)
#else
  while ((ent = readdir(d)) != ILU_NIL)
#endif
    {
#if defined(WIN32)
	  strcpy(filename, findFileStruct.name);
#else
	  strcpy(filename, ent->d_name);
#endif
      if ((strlen(filename) == 8) &&
	  (strspn(filename, "0123456789abcdefABCDEF") == 8))
	{
	  if ((f = fopen(filename, "r")) == ILU_NIL)
	    fprintf (stderr, "Can't read file %s/%s.\n", dirname, filename);	    
	  else
	    {
	      if ((fgets(sbh, sizeof(sbh), f) == ILU_NIL) ||
		  (fgets(sbh, sizeof(sbh), f) == ILU_NIL))
		fprintf (stderr, "Can't read file %s/%s.\n", dirname, filename);
	      else
		if ((pattern[0] == 0) ||
		    (strstr(sbh, pattern) != ILU_NIL))
		  count++;
	      fclose(f);
	    }
	}
#if defined(WIN32)
	  if (_findnext(hDirHandle, &findFileStruct) != 0)
		  finding = FALSE;
#endif
    }
  if (pattern[0] == 0)
    printf ("%d object%s%s\n", count,
	    (count == 1) ? "" : "s",
	    (count > 0) ? ":" : ".");
  else
    printf ("%d object%s matching \"%s\"%s\n", count,
	    (count == 1) ? "" : "s", pattern,
	    (count > 0) ? ":" : ".");

  /* now print them out */
#if defined(WIN32)
  _findclose(hDirHandle);
  finding = FALSE;
  if ((hDirHandle = _findfirst(buf, &findFileStruct)) != -1L)
  {
	finding = TRUE;
  }
  while (finding)
#else
  rewinddir(d);
  while ((ent = readdir(d)) != ILU_NIL)
#endif
    {
#if defined(WIN32)
	  strcpy(filename, findFileStruct.name);
#else
	  strcpy(filename, ent->d_name);
#endif
      if ((strlen(filename) == 8) &&
	  (strspn(filename, "0123456789abcdefABCDEF") == 8))
	{
	  if ((f = fopen(filename, "r")) == ILU_NIL)
	    fprintf (stderr, "Can't read file %s/%s.\n", dirname, filename);	    
	  else
	    {
	      if ((fgets(sbh, sizeof(sbh), f) == ILU_NIL) ||
		  (fgets(sbh, sizeof(sbh), f) == ILU_NIL))
		fprintf (stderr, "Can't read file %s/%s.\n", dirname, filename);
	      else
		if ((pattern[0] == 0) ||
		    (strstr(sbh, pattern) != ILU_NIL))
		  printf ("  %s", sbh);
	      fclose(f);
	    }
	}
#if defined(WIN32)
	  if (_findnext(hDirHandle, &findFileStruct) != 0)
		  finding = FALSE;
#endif
    }
#if defined(WIN32)
  _findclose(hDirHandle);
#else
  closedir(d);
#endif
  return 0;
}
