#ifndef _WINNT_H
#define _WINNT_H

#define DLL_PROCESS_ATTACH 1    
#define DLL_PROCESS_DETACH 0    
#define DLL_THREAD_ATTACH  2    
#define DLL_THREAD_DETACH  3    

#define PROCESS_TERMINATE         0x0001
#define PROCESS_CREATE_THREAD     0x0002 
#define PROCESS_VM_OPERATION      0x0008  
#define PROCESS_VM_READ           0x0010  
#define PROCESS_VM_WRITE          0x0020  
#define PROCESS_DUP_HANDLE        0x0040  
#define PROCESS_CREATE_PROCESS    0x0080  
#define PROCESS_SET_QUOTA         0x0100  
#define PROCESS_SET_INFORMATION   0x0200  
#define PROCESS_QUERY_INFORMATION 0x0400  

#define STANDARD_RIGHTS_REQUIRED         0x000F0000
#define SYNCHRONIZE                      0x00100000

#define PROCESS_ALL_ACCESS        (STANDARD_RIGHTS_REQUIRED | SYNCHRONIZE | 0xFFF)

#ifdef __ANAL__
#define DECLARE_HANDLE(h) struct h##__ { int dummy; }; typedef struct h##__ *h
#else
#define DECLARE_HANDLE(h)  typedef void *h
#endif

DECLARE_HANDLE(HANDLE);


#endif
