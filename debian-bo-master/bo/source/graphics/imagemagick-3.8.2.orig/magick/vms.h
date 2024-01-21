/*
  VMS specific include declarations.
*/
#include <lib$routines.h>
#include <errno.h>
#include <descrip.h>
#include <rmsdef.h>
#include <ctype.h> 

/*
  Typedef declarations.
*/
struct dirent
{
  char
     d_name[255];

  int
    d_namlen;
};

typedef struct _dirdesc
{
  long
    context;

  char
    *pattern;

  struct dirent
    entry;

  struct dsc$descriptor_s
    pat;
} DIR;

/*
  VMS utilities routines.
*/
extern DIR
  *opendir(char *);

extern struct dirent
  *readdir(DIR *);

extern void
  closedir(DIR *);
