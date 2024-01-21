/*
  Windows NT specific include declarations.
*/
#include <windows.h>
#include <errno.h>

/*
  Define declarations.
*/
#define S_IREAD  00400
#define S_IWRITE  00200

/*
  Typedef declarations.
*/
typedef struct _DIR
{
  HANDLE
    hSearch;

  WIN32_FIND_DATA
    Win32FindData;
} DIR;

struct dirent
{
  char
     d_name[2048];
 
  int
    d_namlen;
};

/*
  NT utilities routines.
*/
extern int
  systemNT(char *);

extern long
  telldir(DIR *);

extern DIR
  *opendir(char *);
 
extern struct dirent
  *readdir(DIR *);
 
extern void
  closedir(DIR *),
  seekdir(DIR *,long);
