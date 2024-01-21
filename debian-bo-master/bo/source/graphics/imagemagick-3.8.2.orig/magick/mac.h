/*
  Macintosh specific include declarations.
*/
#include <stat.h>
#include <errno.h>
#include <Errors.h>
#include <Files.h>

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
  int
    d_VRefNum;

  long int
    d_DirID;

  int
    d_index;
} DIR;

struct dirent
{
  char
     d_name[255];
 
  int
    d_namlen;
};

/*
  Macintosh utilities routines.
*/
extern long
  telldir(DIR *);

extern DIR
  *opendir(char *);

extern int
  systemMAC(const char *);

extern struct dirent
  *readdir(DIR *);
 
extern void
  closedir(DIR *),
  seekdir(DIR *,long);
