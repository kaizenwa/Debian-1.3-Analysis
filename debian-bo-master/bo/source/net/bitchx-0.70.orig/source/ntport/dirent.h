//
// Copyright(c) 1996 Amol Deshpande
// amold@microsoft.com
// Redistribution in source or binary form is permitted as long as you 
// retain this notice in the distribution
//
// There is no warranty, implied or otherwise, with this software. Use at your
// own risk.
// 
// See LICENSE.TXT for complete redistribution/source rights
// The original ircii code is under the copyright indicated in the source.
// 
// And finally,
// Microsoft Corporation has nothing to do with this code. 
//
// dirent.h
// directory interface functions. Sort of like dirent functions on unix.
//
//
#ifndef DIRENT_H
#define DIRENT_H

//#define _WINSOCKAPI_ // conflicts with timeval
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#define NAME_MAX MAX_PATH

struct dirent {
	long            d_ino;
	int             d_off;
	unsigned short  d_reclen;
	char            d_name[NAME_MAX+1];
};

typedef struct {
	HANDLE dd_fd;
	int dd_loc;
	int dd_size;
	int is_root;
	char orig_dir_name[NAME_MAX +1];
	struct dirent *dd_buf;
}DIR;

DIR *opendir(char*);
struct dirent *readdir(DIR*);
int closedir(DIR*);
void rewinddir(DIR*);
#endif DIRENT_H
