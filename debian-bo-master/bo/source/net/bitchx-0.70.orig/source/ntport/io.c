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
// io.c
// wrapper functions for i/o.
//
//
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdio.h>
#include <fcntl.h>
#include <memory.h>
#include <errno.h>
#include "ntport.h"

#define CR 0x0d


#define xmalloc(s) HeapAlloc(GetProcessHeap(),HEAP_ZERO_MEMORY,(s))
#define xfree(p) HeapFree(GetProcessHeap(),0,(p))

extern void meta1_char(void*,void*);
extern void meta2_char(void*,void*);
extern void meta3_char(void*,void*);

extern int li,co;
int consoleread(HANDLE , unsigned char * ,int ) ;


INPUT_RECORD girec[2048];

int nt_read(int fd, unsigned char * buf, int howmany) {

	int numread=0,err,i,actual,repeater=0;
	HANDLE hRead ;
	DWORD ftype;
	char *auxbuf;

	//
	// We guess here that if fd's not a C-runtime handle, it must be a
	// real OS handle.
	// This could bomb horribly in the right (wrong) circumstances.
	// Easy to fix those bugs as they are found.
	//-amol
	//
	hRead= (HANDLE)_get_osfhandle(fd);
	if (hRead == INVALID_HANDLE_VALUE)
		hRead = (HANDLE)fd;
	
	ftype = GetFileType(hRead);


	if (ftype == FILE_TYPE_CHAR)
		return consoleread(hRead,buf,howmany);
	if (!ReadFile(hRead, buf,howmany,&numread, NULL ) ){
		err = GetLastError();
		if (err == 87) {
			MessageBox(NULL,"This should never happen.Report it",
				"ircii-NT",MB_ICONHAND);
		}
		switch(err) {
			case ERROR_IO_PENDING:
				break;
			case ERROR_ACCESS_DENIED:
			case ERROR_INVALID_HANDLE:
				errno = EBADF;
				return -1;
				break;
			case ERROR_HANDLE_EOF:
			case ERROR_BROKEN_PIPE:
				errno = 0;
				return 0;
			default:
				errno = EBADF;
				return 0;
		}
	}
	if (!numread)
		return numread;
	 USE(actual);
	 USE(auxbuf);
	 USE(i);
		return numread;


}

int nt_write(void * fd, char * buf, int howmany) {

	int bytes_rtn,err;//,bwrote;
	extern nt_win_write(char*,int);

	//
	// This routine MUST always get a valid OS handle, whether it is a socket
	// or a File handle.
	// -amol

	// horribly bad, but it works most of the time -amol
	err = send((SOCKET)fd,buf,howmany,0);
	if (err != SOCKET_ERROR) // We are returning the bytes read, hopefully
		return err;

	if(!WriteFile((HANDLE)fd, buf,howmany,(ULONG*)&bytes_rtn,
			NULL)){
		err = GetLastError();
		if (err == 87 ) {
			MessageBox(NULL,"This should never happen either.Report it",
				"ircii-NT",MB_ICONHAND);
		}
		switch(err) {
			case ERROR_ACCESS_DENIED:
			case ERROR_INVALID_HANDLE:
				errno = EBADF;
				return -1;
				break;
			case ERROR_BROKEN_PIPE:
				errno = EPIPE;
				return -1;
			default:
				errno = EBADF;
				return -1;
		}
		
	}
	return bytes_rtn;

}

int consoleread(HANDLE hInput, unsigned char * buf,int howmany) {

	INPUT_RECORD *irec;
	DWORD numread,controlkey,i;
	WORD vcode;
	unsigned char ch;
	int rc, where=0;
	int alt_pressed = 0,memfree=0;

// This function is called very frequently. So, we don't:
// 1. Declare large arrays on the stack (use girec)
// 2. Allocate any memory
//
// This gives me the illusion of speedups, so there.
//
// -amol
//
	if (howmany >0) {
		if (howmany > 2048){
			irec = xmalloc(howmany*sizeof(INPUT_RECORD));
			memfree=1;
		}
		else
			irec = &(girec[0]);
		if (!irec){
			errno = ENOMEM;
			return -1;
		}
	}
	while(1) {
		rc = ReadConsoleInput(hInput,irec,howmany,&numread);
		if (!rc) {
			rc = GetLastError();
			switch (rc) {
				case ERROR_INVALID_HANDLE:
				case ERROR_ACCESS_DENIED:
					errno = EBADF;
					break;
			}
			if (memfree)
				xfree(irec);
			return -1;
		}
		for(i=0;i<numread;i++) {
			switch(irec[i].EventType) {
				case WINDOW_BUFFER_SIZE_EVENT:
					co=irec[i].Event.WindowBufferSizeEvent.dwSize.X;
					li = irec[i].Event.WindowBufferSizeEvent.dwSize.Y;
					refresh_screen(NULL,NULL);
					break;
				case KEY_EVENT:
					if (irec[i].Event.KeyEvent.bKeyDown) {
						vcode=(irec[i].Event.KeyEvent.wVirtualKeyCode);
						ch=(irec[i].Event.KeyEvent.uChar.AsciiChar);
						controlkey=(irec[i].Event.KeyEvent.dwControlKeyState);
						if (controlkey & LEFT_ALT_PRESSED)
							alt_pressed=1;
						else if (controlkey & RIGHT_ALT_PRESSED)
							alt_pressed=2;

						/* This hack for arrow keys -amol 9/28/96 */

						if (vcode>= VK_LEFT && vcode <= VK_DOWN) {
							if ( (howmany - where-1) > 2 ){
								buf[where++] = '\033';
								buf[where++] = 'O';
								switch(vcode){
									case VK_LEFT:
										buf[where++] = 'D';
									break;
									case VK_UP:
										buf[where++] = 'A';
									break;
									case VK_RIGHT:
										buf[where++] = 'C';
									break;
									case VK_DOWN:
										buf[where++] = 'B';
									break;

								}
							}
							else 
								break;
						}
						switch(vcode) {
						  /* This hack for ins,del etc. -amol 11/13/96 */
							case  VK_INSERT:
								buf[where++]='\033';
								buf[where++]='O';
								buf[where++] = '1';
								break;
							case  VK_DELETE :
								buf[where++]='\033';
								buf[where++]='O';
								buf[where++] = '2';
								break;
							case  VK_PRIOR: 
								buf[where++]='\033';
								buf[where++]='O';
								buf[where++] = '5';
								break;
							case  VK_NEXT: 
								buf[where++]='\033';
								buf[where++]='O';
								buf[where++] = '6';
								break;
							case  VK_END: 
								buf[where++]='\033';
								buf[where++]='O';
								buf[where++] = '4';
								break;
							case  VK_HOME: 
								buf[where++]='\033';
								buf[where++]='O';
								buf[where++] = '3';
								break;
							case VK_ESCAPE:
								buf[where++]='\033';
								break;
							default:
								if(ch){
									if(1==alt_pressed){
										meta1_char(NULL,NULL);
									}
									/*
									if(2==alt_pressed){
										meta2_char(NULL,NULL);
									}
									*/
								}
								if (ch) {
									buf[where++]=ch;
								}
								break;
						}
						alt_pressed=0;
					}
					break;
				default:
					break;
			}
		}
		buf[where]=0; break;
	}
	if (memfree)
		xfree(irec);
	if (!where)
		return -1;
	return (where );
}

//
// replacement for creat that makes handle non-inheritable. 
// -amol 
//
int nt_creat(char *filename, int mode) {

	// ignore the bloody mode

	int fd;
	HANDLE retval;
	SECURITY_ATTRIBUTES security;

	security.nLength = sizeof(security);
	security.lpSecurityDescriptor = NULL;
	security.bInheritHandle = FALSE;

	retval = CreateFile(filename,
						GENERIC_READ | GENERIC_WRITE,
						FILE_SHARE_READ | FILE_SHARE_WRITE,
						&security,
						CREATE_ALWAYS,
						0,
						NULL);

	if (retval == INVALID_HANDLE_VALUE) {
		errno = EACCES;
		return -1;
	}
	fd = _open_osfhandle((long)retval,_O_BINARY);
	if (fd <0) {
		//should never happen
		fprintf(stderr,"error %s %d %d\n",__FILE__,__LINE__,errno);
	}
	return fd;
	
}
extern int nt_open(char *filename, int perms) { 

	// ignore the bloody mode

	int fd;
	HANDLE retval;
	SECURITY_ATTRIBUTES security;
	DWORD dwAccess, dwSharemode, dwCreateDist;

	security.nLength = sizeof(security);
	security.lpSecurityDescriptor = NULL;
	security.bInheritHandle = FALSE;

	switch (perms & (_O_RDONLY | _O_WRONLY | _O_RDWR) ) {
		case _O_RDONLY:
			dwAccess = GENERIC_READ;
			break;
		case _O_WRONLY:
			dwAccess = GENERIC_WRITE;
			break;
		case _O_RDWR:
			dwAccess = GENERIC_READ | GENERIC_WRITE ;
			break;
		default:
			errno = EINVAL;
			return -1;
	}
	switch (perms & (_O_CREAT | _O_TRUNC) ){
		case 0:
			dwCreateDist = OPEN_EXISTING;
			break;
		case _O_CREAT:
			dwCreateDist = CREATE_ALWAYS;
			break;
		case _O_CREAT | _O_TRUNC:
			dwCreateDist = CREATE_ALWAYS;
			break;
		case _O_TRUNC:
			dwCreateDist = TRUNCATE_EXISTING;
			break;
		default:
			errno = EINVAL;
			return -1;
	}
	USE(dwSharemode);
	retval = CreateFile(filename,
						dwAccess,//GENERIC_READ | GENERIC_WRITE,
						FILE_SHARE_READ | FILE_SHARE_WRITE,
						&security,
						dwCreateDist,//CREATE_ALWAYS,
						0,
						NULL);

	if (retval == INVALID_HANDLE_VALUE) {
		int err = GetLastError();
		char errbuf[128];
		make_err_str(err,errbuf,128);
		fprintf(stderr,"open error %s\n",errbuf);
		errno = EACCES;
		return -1;
	}

	fd = _open_osfhandle((long)retval,_O_BINARY);
	if (fd <0) {
		//should never happen
		fprintf(stderr,"error %s %d %d\n",__FILE__,__LINE__,errno);
	}
	return fd;
	
}
