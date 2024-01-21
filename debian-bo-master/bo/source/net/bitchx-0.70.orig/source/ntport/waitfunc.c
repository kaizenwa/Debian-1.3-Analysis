//
// Copyright(c) 1996 Amol Deshpande
// amold@microsoft.com
// Redistribution in source or binary form is permitted as long as you 
// retain this notice in the distribution
// 
// See LICENSE.TXT for complete redistribution/usage conditions
//
// There is no warranty, implied or otherwise, with this software. Use at your
// own risk.
//
// The original ircii code is under the copyright indicated in the source.
// 
// And finally,
// Microsoft Corporation has nothing to do with this code. 
//
//
// waitfunc.c: This file contains the heart of, and hence the ugliest hacks 
// in the ircii-NT port. -amol
//
#define WIN32_LEAN_AND_MEAN
#include <winsock.h>
#include <stdio.h>
#include <stdlib.h>
#include <io.h>

HANDLE io_sem;

long prevcount;


// The problem with making these inline macros is that it's difficult to 
// check errors in those.-amol
//
#define RELEASE_LOCK() release_lock() 
#define GET_LOCK() get_lock() 


void DccThreadProc(void);
void release_lock(void);
void get_lock(void);

extern void dprintf(char *,...);

extern void set_server_bits(fd_set *);
extern void do_server(fd_set *);
extern void	do_processes(fd_set*);
extern void set_dcc_bits(fd_set *,fd_set*);
extern void dcc_check(fd_set *);
extern void cursor_to_input(void);
extern int new_select(fd_set *, fd_set*, struct timeval *);

extern int break_io_processing;

extern unsigned long  numprocs;


void release_lock(void) {

	unsigned long err;
	if (!ReleaseMutex(io_sem)) {
		//
		// 288 is when you don't have the mutex but try to give it up
		// benign for our purposes -amol
		//
		if ((err=GetLastError()) !=288){
			fprintf(stderr,"Unable to release mutex. Error:%d\n",err);
			abort();
		}
	}
}
void get_lock(void) {
	if(WaitForSingleObject(io_sem,INFINITE) == WAIT_FAILED) {
		int dbgerr = GetLastError();
		fprintf(stderr,"Wait failed %d\n",dbgerr);
		abort();
	}
}
void init_io_lock(void) {
	
	//
	// Do *NOT* give this puppy a name. Sometimes, a process on win95
	// will not die cleanly (blocked on a read or something) and if
	// it holds a named mutex, you can't run any other instances of the app
	// -amol
	//
	io_sem = CreateMutex(NULL,TRUE,NULL);
	if (io_sem == NULL) {
		fprintf(stderr,"Fatal.. CreateSem failed\n",GetLastError());
		abort();
	}
}

//
// The common thread (no pun intended) in the following logic of select (for
// socket waits ) and WaitConsole is that only one of either thread must be
// running at any time. If you make modifications and do not follow this
// rule, be prepared for synchronization headaches.
// 
// -amol
void start_server_and_dcc_threads(int level) {

	HANDLE h1;
	DWORD tid;
	if (level >1 ) // don't do this recursively. 
		return;
	h1 = CreateThread ( NULL,
						0,
						(LPTHREAD_START_ROUTINE)DccThreadProc,
						NULL,
						0,
						&tid);

	if (!h1) {
		fprintf(stderr,"Fatal.. Cannnot create threads ???\n",GetLastError());
		abort();
	}
	CloseHandle(h1);
}
void DccThreadProc(void){
	fd_set rd,wd;
	struct timeval timer = {0,0};
	int rc;

	while(1) {
		FD_ZERO(&rd);
		FD_ZERO(&wd);
		set_server_bits(&rd);
		set_dcc_bits(&rd,&wd);

		if (numprocs >1)
			timer.tv_sec = 2;
		else
			timer.tv_sec = 30;

		cursor_to_input();
		RELEASE_LOCK();
		if ((wd.fd_count == 0) && (rd.fd_count == 0) ) {
			if (numprocs <=1)
				Sleep(2000);
		}
		else {
			rc = new_select(rd.fd_count?&rd:NULL, wd.fd_count?&wd:NULL, &timer);
			if (rc == SOCKET_ERROR)
				;//break;
		}
		GET_LOCK();
		if(!break_io_processing) {
			do_server(&rd);
		}
		if(!break_io_processing) {
			dcc_check(&rd);
		}
		//
		// There is no way to wait for a pipe to get data, so we are
		// forced to poll every two seconds. this sucks !
		// -amol
		if (numprocs >1) {
			if(!break_io_processing) {
				do_processes(&rd);
			}
		}

	}
	fprintf(stderr,"Select in dcc returned error %d\n",WSAGetLastError());
	abort();
}
int WaitConsole(struct timeval * timer, HANDLE hStdin) {
	
	DWORD dwRet = 0;
	unsigned long osec;
	RELEASE_LOCK();

	osec = timer->tv_sec;
	if (numprocs >1){
		timer->tv_sec = 0;
		timer->tv_usec = 50;
	}
	dwRet = WaitForSingleObject(hStdin,timer->tv_sec*1000);

	if (numprocs >1) {
		timer->tv_sec = osec;
		timer->tv_usec = 0;
	}

	GET_LOCK();
	if (dwRet == WAIT_FAILED)
		return 0;
	if (dwRet == WAIT_TIMEOUT)
		return 0;
	return 1001;
}
//
// This is an ugly poll, but we have to poll to avoid blocking 
// keyboard input for the entire period. remember that this function is
// called when irc_io_loop is recursive, so we don't spawn any other threads.
// -amol
int non_thread_check_desc(void){

	fd_set rd,wd;
	struct timeval timer = {0,50*1000};
	int rc;

	FD_ZERO(&rd);
	FD_ZERO(&wd);

	GET_LOCK();

	set_server_bits(&rd);
	set_dcc_bits(&rd,&wd);

	// attempt to speed up /exec -amol 9/23/96
	if(!break_io_processing) {
		do_processes(&rd);
	}
	if ( (wd.fd_count == 0) && (rd.fd_count == 0) ) {
		Sleep(50);
	}
	else {
		rc = new_select(rd.fd_count?&rd:NULL, wd.fd_count?&wd:NULL, &timer);
		if (rc == SOCKET_ERROR)
			;//break;
	}
	if (rc <= 0) {
		RELEASE_LOCK();
		return rc;
	}
	if(!break_io_processing) {
		do_server(&rd);
	}
	if(!break_io_processing) {
		dcc_check(&rd);
	}
	RELEASE_LOCK();

}
