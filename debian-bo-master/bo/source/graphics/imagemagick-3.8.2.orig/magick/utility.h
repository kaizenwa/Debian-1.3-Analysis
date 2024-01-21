#if !defined(vms) && !defined(macintosh) && !defined(WIN32)
#if HAVE_SYS_NDIR_H || HAVE_SYS_DIR_H || HAVE_NDIR_H
# define dirent direct
# define NAMLEN(dirent) (dirent)->d_namlen
# if HAVE_SYS_NDIR_H
#  include <sys/ndir.h>
# endif
# if HAVE_SYS_DIR_H
#  include <sys/dir.h>
# endif
# if HAVE_NDIR_H
#  include <ndir.h>
# endif
#else
# include <dirent.h>
# define NAMLEN(dirent) Extent((dirent)->d_name)
#endif
#include <pwd.h>
#else
#if defined(vms)
#include "vms.h"
#endif
#if defined(macintosh)
#include "mac.h"
#endif
#if defined(WIN32)
#include "nt.h"
#endif
#endif

#ifndef S_ISDIR
#define S_ISDIR(mode) (((mode) & S_IFMT) == S_IFDIR)
#endif

/*
  Utility define declarations.
*/
#if !defined(vms)
#define IsGlob(text) \
  ((strchr(text,'*') != (char *) NULL) || \
   (strchr(text,'?') != (char *) NULL) || \
   (strchr(text,'{') != (char *) NULL) || \
   (strchr(text,'}') != (char *) NULL) || \
   (strchr(text,'[') != (char *) NULL) || \
   (strchr(text,']') != (char *) NULL))
#else
#define IsGlob(text) \
  ((strchr(text,'*') != (char *) NULL) || \
   (strchr(text,'?') != (char *) NULL) || \
   (strchr(text,'{') != (char *) NULL) || \
   (strchr(text,'}') != (char *) NULL))
#endif
#if !defined(vms) && !defined(macintosh) && !defined(WIN32)
#define BasenameSeparator  "/"
#define DirectorySeparator  "/"
#define SystemCommand(command)  system(command)
#define TemporaryTemplate  "%s/magickXXXXXX"
#else
#if defined(vms)
#define BasenameSeparator  "]"
#define DirectorySeparator  ""
#define SystemCommand(command)  (!system(command))
#endif
#if defined(macintosh)
#define BasenameSeparator  ":"
#define DirectorySeparator  ":"
#define SystemCommand(command)  systemMAC(command)
#endif
#if defined(WIN32)
#define BasenameSeparator  "/"
#define DirectorySeparator  "/"
#define SystemCommand(command)  systemNT(command)
#endif
#endif

/*
  Utilities routines.
*/
extern char
  *ClientName(const char *),
  **ListColors(const char *,int *),
  **ListFiles(char *,const char *,int *),
  *PostscriptGeometry(const char *),
  **StringToList(char *);

extern int
  GlobExpression(char *,const char *),
  MultilineCensus(const char *),
  ReadDataBlock(char *,FILE *);

extern unsigned int
  IsAccessible(const char *),
  IsDirectory(const char *),
  ReadData(char *,const unsigned int,const unsigned int,FILE *);

extern unsigned long
  LSBFirstReadLong(FILE *),
  MSBFirstReadLong(FILE *);

extern unsigned short
  LSBFirstReadShort(FILE *),
  MSBFirstReadShort(FILE *);

extern void
  AppendImageFormat(const char *,char *),
  ExpandFilename(char *),
  ExpandFilenames(int *,char ***),
  LocaleFilename(char *),
  LSBFirstWriteLong(const unsigned long,FILE *),
  LSBFirstWriteShort(const unsigned int,FILE *),
  MSBFirstOrderLong(char *,const unsigned int),
  MSBFirstOrderShort(char *,const unsigned int),
  MSBFirstWriteLong(const unsigned long,FILE *),
  MSBFirstWriteShort(const unsigned int,FILE *),
  Strip(char *),
  TemporaryFilename(char *);
