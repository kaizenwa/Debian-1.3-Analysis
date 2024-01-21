//
// Copyright(c) 1996 Amol Deshpande
// amold@microsoft.com
// Redistribution in source or binary form is permitted as long as you 
// retain this notice in the distribution
//
// There is no warranty, implied or otherwise, with this software. Use at your
// own risk.
//
// See LICENSE.TXT for complete redistribution/usage conditions
//
// The original ircii code is under the copyright indicated in the source.
// 
// And finally,
// Microsoft Corporation has nothing to do with this code. 
//
// dirent.c
// directory interface functions. Sort of like dirent functions on unix.
//
//
#define WIN32_LEAN_AND_MEAN
#include <stdio.h>
#include <errno.h>
#include <assert.h>
#include <direct.h>
#include "dirent.h"

#define malloc(a) HeapAlloc(GetProcessHeap(),HEAP_ZERO_MEMORY,(a))
#define free(a) HeapFree(GetProcessHeap(),0,(a))

static int inode= 1; // useless piece that some unix programs need
DIR * opendir(char *buf) {

	DIR *dptr;
	WIN32_FIND_DATA fdata;
	char *tmp ;
	

	if (!buf)
		buf = "." ;
	tmp= (char *)malloc(lstrlen(buf) + 4); 
	memset(tmp,0,lstrlen(buf) +4);
	/*
	 * paths like / confuse NT because it looks like a UNC name
	 * when we append "\*" -amol
	 */
	if ( (buf[0] == '/') && (buf[1] != '/') ) {
		wsprintf(tmp,"%c:%s*",'A' + (_getdrive()-1),buf);
	}
	else { 
		//if (buf[1] != ':')
			wsprintf(tmp,"%s/*",buf);
		//else
		//	wsprintf(tmp,"%s*",buf);
	}
	
	dptr = (DIR *)malloc(sizeof(DIR));
	if (!dptr){
		errno = ENOMEM;
		return NULL;
	}
	
	dptr->dd_fd = FindFirstFile(tmp,&fdata);
	if (dptr->dd_fd == INVALID_HANDLE_VALUE){
		if (GetLastError() == ERROR_DIRECTORY)
			errno = ENOTDIR;
		else
			errno = ENFILE;	
		free(dptr);
		return NULL;
	}
	memset(dptr->orig_dir_name,0,sizeof(dptr->orig_dir_name));
	memcpy(dptr->orig_dir_name,tmp,lstrlen(tmp));
	free(tmp);

	/* this is useless */
#if 0
	if( !(fdata.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY ) ){
		errno = ENOTDIR;	
		free(dptr);
		return NULL;
	}
#endif
	dptr->dd_loc = 0;
	dptr->dd_size = fdata.nFileSizeLow;
	dptr->dd_buf = (struct dirent *)malloc(sizeof(struct dirent));
	if (!dptr->dd_buf){
		free(dptr);
		errno = ENOMEM;
		return NULL;
	}
	(dptr->dd_buf)->d_ino = inode++;
	(dptr->dd_buf)->d_off = 0;
	(dptr->dd_buf)->d_reclen = 0;
	if (lstrcmpi(fdata.cFileName,".") ){
		//dptr->dd_buf->d_name[0] = '.';
		memcpy((dptr->dd_buf)->d_name,".",2);
		dptr->is_root=1;
	}
	else
		memcpy((dptr->dd_buf)->d_name,fdata.cFileName,MAX_PATH);
	return dptr;
}
int closedir(DIR *dptr){

	if (!dptr)
		return 0;
	FindClose(dptr->dd_fd);
	free(dptr->dd_buf);
	free(dptr);
	return 0;
}
void rewinddir(DIR *dptr) {

	HANDLE hfind;
	WIN32_FIND_DATA fdata;
	char *tmp = dptr->orig_dir_name;

	if (!dptr) return;

	hfind = FindFirstFile(tmp,&fdata);
	assert(hfind != INVALID_HANDLE_VALUE);
	FindClose(dptr->dd_fd);
	dptr->dd_fd = hfind;
	dptr->dd_size = fdata.nFileSizeLow;
	(dptr->dd_buf)->d_ino = inode++;
	(dptr->dd_buf)->d_off = 0;
	(dptr->dd_buf)->d_reclen = 0;
	memcpy((dptr->dd_buf)->d_name,fdata.cFileName,MAX_PATH);
	/*
	DIR *temp = opendir((*dptr)->orig_dir_name);
	closedir(*dptr);
	*dptr = temp;
	*/
	return;
}
struct dirent *readdir(DIR *dir) {

	WIN32_FIND_DATA fdata;
	HANDLE hfind;
	char *tmp ;

	if (!dir)
		return NULL;

		// special hack for root (which does not have . or ..)
	if (dir->is_root) {
		tmp= dir->orig_dir_name;
		hfind = FindFirstFile(tmp,&fdata);
		FindClose(dir->dd_fd);
		dir->dd_fd = hfind;
		dir->dd_size = fdata.nFileSizeLow;
		(dir->dd_buf)->d_ino = inode++;
		(dir->dd_buf)->d_off = 0;
		(dir->dd_buf)->d_reclen = 0;
		memcpy((dir->dd_buf)->d_name,fdata.cFileName,MAX_PATH);
		dir->is_root=0;
		return dir->dd_buf;

	}
	if(!FindNextFile(dir->dd_fd,&fdata) ){
		return NULL;
	}
	(dir->dd_buf)->d_ino = inode++;
	(dir->dd_buf)->d_off = 0;
	(dir->dd_buf)->d_reclen = 0;
	memcpy((dir->dd_buf)->d_name,fdata.cFileName,MAX_PATH);

	return dir->dd_buf;

}
