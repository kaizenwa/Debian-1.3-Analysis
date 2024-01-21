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

/* 
 * \ports\irc\source\ntport\support.c - OS dependent routines for NT/95
 * 
 * Author:	Amol Deshpande
 * Date:	Thu Jul 04 11:59:01 1996
 *
 * Copyright (c) 1996 Amol Deshpande
 */
#define WIN32_LEAN_AND_MEAN
#include <winsock.h>
#include <stdio.h>
#include <locale.h>
#include <fcntl.h>
#include <io.h>
#include <errno.h>
#include <direct.h>
#include <stdlib.h>

#include "irc_std.h"
#include "vars.h"
#include "ntport.h"

#define xmalloc(s) HeapAlloc(GetProcessHeap(),HEAP_ZERO_MEMORY,(s))
#define xfree(p) HeapFree(GetProcessHeap(),0,(p))


_filemode = _O_NOINHERIT;

int li, co;

DWORD gdwPlatform;

unsigned long numprocs = 1;

char *cwdnofix = NULL;


extern HANDLE ghstdout;


void nt_init(void) {

  WSADATA wsaData;
  int err=0;
  DWORD oldmode;
  OSVERSIONINFO osver;


  if (WSAStartup(0x101, &wsaData) != 0) {
	fprintf(stderr,"Startup failed %d\n",GetLastError());
	ExitProcess(0xFFFF);
  }

  if (!GetConsoleMode(GetStdHandle(STD_INPUT_HANDLE),&oldmode )) {
	fprintf(stderr,"GetConsoleMode failed %d\n",GetLastError());
	ExitProcess(0xFFFF);
  }
  if (!SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE)
  	,(oldmode | ENABLE_WINDOW_INPUT)) ){
	fprintf(stderr,"SetConsoldeMode failed %d\n",GetLastError());
	ExitProcess(0xFFFF);
  }
	osver.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);

	if (!GetVersionEx(&osver)) {
		MessageBox(NULL,"GetVersionEx failed","tcsh",MB_ICONHAND);
		ExitProcess(0xFF);
	}
	gdwPlatform = osver.dwPlatformId;

	cwdnofix = getenv("IRC_DONT_FIX_GETCWD");
	nt_term_init();
	nt_getsize(&li, &co);
	init_io_lock();
	init_proc_critter();
	setlocale(LC_ALL,"");

}

void make_err_str(unsigned int error,char *buf,int size) {

	FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM,
				  NULL,
				  error, 
				  MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US),
				  buf,
				  size,
				  NULL);
	return;

}
int nt_check_ws(HANDLE *hproc) {

	DWORD status;

	if ((int)(*hproc) ==  -1)
		return wait_any_child(hproc);
	if (!GetExitCodeProcess(*hproc,&status))
		return -1;
	if (status == STILL_ACTIVE)
		return -1;
	return (int)status;
}
unsigned long nt_pipe_has_data(int fdpipe) {
	
	DWORD bytesavail=0;
	DWORD err;
	extern int check_bufs(int);

	if(!PeekNamedPipe((HANDLE)fdpipe,//(HANDLE)_get_osfhandle(fdpipe),
					NULL,
					0,
					NULL,
					&bytesavail,
					NULL)){
		err =GetLastError();
		if (err != 109)
			say("peekaboo");
		return 1;

	}
	if (!bytesavail) {
		return check_bufs(fdpipe);
	}

	return bytesavail;
}
//
// The following are replacements for pipe() and dup2() which create
// non-inheritable handles.
//
int nt_pipe(HANDLE hpipe[2]) {

	SECURITY_ATTRIBUTES secd;

	secd.nLength=sizeof(secd);
	secd.lpSecurityDescriptor=NULL;
	secd.bInheritHandle=FALSE;

	return (!CreatePipe(&hpipe[0],&hpipe[1],&secd,0));
}
int nt_dup2(HANDLE source, HANDLE *dest) {
	return (!DuplicateHandle(GetCurrentProcess(),
							source,
							GetCurrentProcess(),
							dest,
							0,
							TRUE,
							DUPLICATE_SAME_ACCESS));
}
void nt_start_process(char *name,char*logical,char*redirect,char*who,
						unsigned int refnum) {


	HANDLE p0[2],p1[2],p2[2],pid;
	char *shell,*flag,*arg;
	char errbuf[128];
	STARTUPINFO si;
	PROCESS_INFORMATION pi;

	p0[0] = p0[1] = INVALID_HANDLE_VALUE;
	p1[0] = p1[1] = INVALID_HANDLE_VALUE;
	p2[0] = p2[1] = INVALID_HANDLE_VALUE;

	memset(&si,0,sizeof(si));
	memset(&pi,0,sizeof(pi));

	si.cb = sizeof(si);
	si.dwFlags = STARTF_USESTDHANDLES;

	if (nt_pipe(p0) || nt_pipe(p1) || nt_pipe(p2))
	{
		say("Unable to start new process: %s", strerror(errno));
		return;
	}
	if(nt_dup2(p0[0], &si.hStdInput)){
		say("%s %d This should never happen",__FILE__,__LINE__);
		return;
	}
	if(nt_dup2(p1[1], &si.hStdOutput)){
		say("%s %d This should never happen",__FILE__,__LINE__);
		return;
	}
	if(nt_dup2(p1[1], &si.hStdError)){
		say("%s %d This should never happen",__FILE__,__LINE__);
		return;
	}
	CloseHandle(p0[0]);
	CloseHandle(p1[1]);
	CloseHandle(p2[1]);

	if ((shell = get_string_var(SHELL_VAR)) == (char *) 0) {
		//
		// Use dynamic memory here to detect buffer overruns
		// -amol
		char	*args=xmalloc(512);

		while ((arg = next_arg(name, &name)) != NULL)
		{
			lstrcat(args,arg);
			lstrcat(args," ");
		}
		dprintf("Creating %s\n",args);
		if(!CreateProcess(NULL,args,NULL,NULL,
					  TRUE,
					  0,
					  NULL,
					  NULL,
					  &si,
					  &pi)) {
			make_err_str(GetLastError(),errbuf,128);
			say("Createprocess(%s) error %s: ",args,errbuf);
			xfree(args);
			return;
		}
		xfree(args);
	}
	else {
		char	*args=xmalloc(512);
		int is_unixy=0;

		if (shell && strstr(shell,"sh.exe"))
			is_unixy=1;

		if ((flag = get_string_var(SHELL_FLAGS_VAR)) ==
				(char *) 0)
			flag = empty_string;

		__try {
			lstrcat(args,shell);
			lstrcat(args," ");
			lstrcat(args,flag);
			lstrcat(args," ");
			if (is_unixy)
				lstrcat(args,"\"");
			lstrcat(args,name);
			if (is_unixy)
				lstrcat(args,"\"");
		}
		__except(1) {
			say("command length exceeds 512 bytes. I refuse to exec");	
		}
		dprintf("Shell + Creating %s\n",args);
		if(!CreateProcess(NULL, args, NULL, NULL,
						  TRUE,
						  0,
						  NULL,
						  NULL,
						  &si,
						  &pi) ) {
			make_err_str(GetLastError(),errbuf,128);
			say("Createprocess(%s) error %s: ",args,errbuf);
			xfree(args);
			return;
		}
	}
	pid = pi.hProcess;
	CloseHandle(pi.hThread);
	CloseHandle(si.hStdInput);
	CloseHandle(si.hStdOutput);
	CloseHandle(si.hStdError);
	/*
	add_process(name, logical, (int)pid, 
		_open_osfhandle((long)p0[1],_O_NOINHERIT), 
		_open_osfhandle((long)p1[0],_O_NOINHERIT), 
		_open_osfhandle((long)p2[0],_O_NOINHERIT), redirect,
		who, refnum);
	*/
	add_process(name, logical, (int)pid, 
		(long)p0[1], 
		(long)p1[0],
		(long)p2[0], redirect, who, refnum);

	add_to_proc_array(pid);
	numprocs++;
}
#ifdef DPRINTF
void
dprintf(char *format, ...)
{				/* } */
	va_list vl;
	char putbuf[4096];
	{
		va_start(vl, format);
		vsprintf(putbuf, format, vl);
		va_end(vl);
		OutputDebugString(putbuf);
	}
}
#endif DPRINTF
//
// convert path from getcwd to '/' delimited
//
char * forward_slash_get_cwd(char * path,int size) {

	char *ptemp;
	char * cwd  = _getcwd(path,size);
	ptemp=cwd;

	if (cwdnofix)
		return cwd;
		
	while(*ptemp) {
		if (*ptemp == '\\') *ptemp = '/';
		*ptemp++;
	}
	return cwd;
}
//
// This hack spawns a subshell, waits for it to exit before returning.
//
//
void do_nt_stop_irc(void) {

	char *shell;
	char errbuf[128];
	DWORD dwmode;
	HANDLE hstdin=GetStdHandle(STD_INPUT_HANDLE);
	STARTUPINFO si;
	PROCESS_INFORMATION pi;

	memset(&si,0,sizeof(si));
	memset(&pi,0,sizeof(pi));

	si.cb = sizeof(si);
	
	if ((shell = get_string_var(SHELL_VAR)) == (char *) 0) {
		if (gdwPlatform == VER_PLATFORM_WIN32_WINDOWS)
			shell = "command.com";
		else
			shell = "cmd.exe";
	}
	if (!GetConsoleMode(hstdin,&dwmode) ){
		say("GetConsoleMode failed(%d):Can't start shell",GetLastError());
		return;
	}
	if (!SetConsoleMode(hstdin, dwmode | ((ENABLE_LINE_INPUT
										|ENABLE_ECHO_INPUT
										|ENABLE_PROCESSED_INPUT)
										& ~ENABLE_WINDOW_INPUT) ) ){
		say("SetConsoleMode failed(%d):Can't start shell",GetLastError());
		return;
	}

	if(!CreateProcess(NULL, shell, NULL, NULL,
					  TRUE,
					  0,
					  NULL,
					  NULL,
					  &si,
					  &pi) ) {
		make_err_str(GetLastError(),errbuf,128);
		say("Createprocess(%s) error %s cant start shell: ",shell,errbuf);
		return;
	}
	CloseHandle(pi.hThread);
	WaitForSingleObject(pi.hProcess,INFINITE);
	CloseHandle(pi.hProcess);

	if (!SetConsoleMode(hstdin, dwmode ) ){
		say("SetConsoleMode failed(%d)",GetLastError());
		return;
	}

	refresh_screen(0,NULL);

}
