/*    s_files.h
 *
 *    Copyright (c) 1994-1996, Marko Macek
 *
 *    You may distribute under the terms of either the GNU General Public
 *    License or the Artistic License, as specified in the README file.
 *
 */

#ifndef __FILESYS_H__
#define __FILESYS_H__

#define SDOT   "."    

#ifdef UNIX
#define SLASH  '/'
#define SSLASH "/"

#define ISSEP(c) ((c) == SLASH)
#define ISSLASH(c) ((c) == SLASH)
#endif

#if defined(OS2) || defined(NT) || defined(DOS) || defined(DOSP32)
#define SLASH '\\'
#define SSLASH "\\"

#define ISSEP(c) (((c) == ':') || ((c) == SLASH))
#define ISSLASH(c) (((c) == '/') || ((c) == '\\'))
#endif

char *Slash(char *Path, int Add);
char *SlashDir(char *Path);
int ExpandPath(char *Path, char *Expanded);
int CompletePath(char *Path, char *Completed, int Num);
int IsSameFile(char *Path1, char *Path2);
int JustDirectory(char *Path, char *Dir);
int JustFileName(char *Path, char *Name);
int JustRoot(char *Path, char *Root);
int FileExists(char *Path);
int IsFullPath(char *Path);
int IsDirectory(char *Path);
char *ShortFName(char *Path, int len);
int ChangeDir(char *Dir);
int JoinDirFile(char *Dest, char *Dir, char *Name);

#endif
