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
//
// proccrap.c: Self-explanatory process managment hacks.
//
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <io.h>


HANDLE proc_array[64];
int currcount;

void init_proc_critter(void) {

	int i;

//	InitializeCriticalSection(&proc_critter);

	for(i=0;i<64;i++) {
		proc_array[i] = INVALID_HANDLE_VALUE;
	}
	currcount=0;
}
void add_to_proc_array(HANDLE hproc) {
	
	int i;

	__try {
//		EnterCriticalSection(&proc_critter);

		for(i=0;i<64;i++) {
			if(proc_array[i] == INVALID_HANDLE_VALUE) {
				proc_array[i]=hproc;
				break;
			}
		}
		if (i == 64){
			fprintf(stderr,"\007\007You are either waiting for 64 processes, or this is a bug\n..I have to stop\n");	
			abort();
		}
		currcount++;

	}
	__finally{
//		LeaveCriticalSection(&proc_critter);
	}
}
int wait_any_child(HANDLE *hproc) {
	
	DWORD dwRet;
	int i,count=0,status = -1;
	HANDLE waiter[64];

	__try {

//		EnterCriticalSection(&proc_critter);

		if (currcount == 0)
			__leave;

		for(i=0;i<64;i++){
			if (proc_array[i] != INVALID_HANDLE_VALUE) 
				waiter[count++] = proc_array[i];
		}
		if (count == 0)	
			__leave;
		dwRet =	WaitForMultipleObjects(count,waiter,FALSE,50);
		if (dwRet == WAIT_FAILED)
			__leave;
		if(  (dwRet >=WAIT_OBJECT_0) && (dwRet <=(WAIT_OBJECT_0 +count -1))){
			for(i=0;i<64;i++) {
				if (proc_array[i] == waiter[dwRet-WAIT_OBJECT_0]){
					proc_array[i] = INVALID_HANDLE_VALUE;
					break;
				}
			}
			if (i == 64){
				fprintf(stderr,"\007\007A Child died..but can't I find it\n");
				abort();
			}
			if (!GetExitCodeProcess(waiter[dwRet-WAIT_OBJECT_0],&status))
				status= -1;
			else
				currcount--;
			*hproc = waiter[dwRet-WAIT_OBJECT_0];
		}
			
	}
	__finally {
//		LeaveCriticalSection(&proc_critter);
	}
	return status;
}
