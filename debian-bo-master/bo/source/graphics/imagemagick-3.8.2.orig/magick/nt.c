/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                                                                             %
%                                 N   N  TTTTT                                %
%                                 NN  N    T                                  %
%                                 N N N    T                                  %
%                                 N  NN    T                                  %
%                                 N   N    T                                  %
%                                                                             %
%                 Windows NT Utility Routines for ImageMagick.                %
%                                                                             %
%                                                                             %
%                               Software Design                               %
%                                 John Cristy                                 %
%                                December 1996                                %
%                                                                             %
%                                                                             %
%  Copyright 1997 E. I. du Pont de Nemours and Company                        %
%                                                                             %
%  Permission to use, copy, modify, distribute, and sell this software and    %
%  its documentation for any purpose is hereby granted without fee,           %
%  provided that the above Copyright notice appear in all copies and that     %
%  both that Copyright notice and this permission notice appear in            %
%  supporting documentation, and that the name of E. I. du Pont de Nemours    %
%  and Company not be used in advertising or publicity pertaining to          %
%  distribution of the software without specific, written prior               %
%  permission.  E. I. du Pont de Nemours and Company makes no representations %
%  about the suitability of this software for any purpose.  It is provided    %
%  "as is" without express or implied warranty.                               %
%                                                                             %
%  E. I. du Pont de Nemours and Company disclaims all warranties with regard  %
%  to this software, including all implied warranties of merchantability      %
%  and fitness, in no event shall E. I. du Pont de Nemours and Company be     %
%  liable for any special, indirect or consequential damages or any           %
%  damages whatsoever resulting from loss of use, data or profits, whether    %
%  in an action of contract, negligence or other tortious action, arising     %
%  out of or in connection with the use or performance of this software.      %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
*/

#if defined(WIN32)
/*
  Include declarations.
*/
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <windows.h>
#include "nt.h"

/*
  External declarations.
*/
extern "C" BOOL WINAPI
   DllMain(HINSTANCE hInst,DWORD wDataSeg,LPVOID lpvReserved);

/*
  Function closedir closes the named directory stream and frees the DIR
  structure.
*/
void closedir(DIR *entry)
{
  assert(entry != (DIR *) NULL);
  FindClose(entry->hSearch);
  free((char *) entry);
}
/*
  Function opendir opens the directory named by filename and associates
  a directory stream with it.
*/
DIR *opendir(char *path)
{

  char
    file_specification[2048];

  DIR
    *entry;

  assert(path != (char *) NULL);
  (void) strcpy(file_specification,path);
  (void) strcat(file_specification,"/*.*");
  entry=(DIR *) malloc(sizeof(DIR));
  if (entry != (DIR *) NULL)
    entry->hSearch=
      FindFirstFile(file_specification,&entry->Win32FindData);
  return(entry);
}
/*
  Function readdir returns a pointer to a structure representing the
  directory entry at the current position in the directory stream to
  which entry refers.
*/
struct dirent *readdir(DIR *entry)
{
  int
    status;

  static dirent
    file_info;

  status=FindNextFile(entry->hSearch,&entry->Win32FindData);
  if (status == 0)
    return((struct dirent *) NULL);
  (void) strcpy(file_info.d_name,entry->Win32FindData.cFileName);
  file_info.d_namlen=strlen(file_info.d_name);
  return(&file_info);
}
/*
  Function seekdir sets the position of the next readdir() operation
  on the directory stream.
*/
void seekdir(DIR *entry,long position)
{
  assert(entry != (DIR *) NULL);
}
/*
  Function systemNT executes the specified command and waits until it
  terminates.  The returned value is the exit status of the command.
*/
__declspec(dllexport) int systemNT(char *command)
{
  char
    local_command[2048];

  DWORD
    child_status;

  int
    status;

  PROCESS_INFORMATION
    process_info;

  STARTUPINFO
    startup_info;

  unsigned int
    background_process;

  if (command == (char *) NULL)
    return(-1);
  GetStartupInfo(&startup_info);
  startup_info.dwFlags=STARTF_USESHOWWINDOW;
  startup_info.wShowWindow=SW_SHOWMINNOACTIVE;
  (void) strcpy(local_command,command);
  background_process=command[strlen(command)-1] == '&';
  if (background_process)
    local_command[strlen(command)-1]='\0';
  if (command[strlen(command)-1] == '|')
    local_command[strlen(command)-1]='\0';
   else
     startup_info.wShowWindow=SW_SHOWDEFAULT;
  status=CreateProcess((LPCTSTR) NULL,local_command,
    (LPSECURITY_ATTRIBUTES) NULL,(LPSECURITY_ATTRIBUTES) NULL,(BOOL) FALSE,
    (DWORD) NORMAL_PRIORITY_CLASS,(LPVOID) NULL,(LPCSTR) NULL,&startup_info,
    &process_info);
  if (status == 0)
    return(-1);
  if (background_process)
    return(status == 0);
  status=WaitForSingleObject(process_info.hProcess,INFINITE);
  if (status != WAIT_OBJECT_0)
    return(status);
  status=GetExitCodeProcess(process_info.hProcess,&child_status);
  if (status == 0)
    return(-1);
  CloseHandle(process_info.hProcess);
  CloseHandle(process_info.hThread);
  return((int) child_status);
}
/*
  Function telldir returns the current location associated  with  the
  named directory stream.
*/
long telldir(DIR *entry)
{
  assert(entry != (DIR *) NULL);
}
/*
  Function Dllmain is the entry point for the ImageMagick DLL library.
*/
BOOL WINAPI DllMain(HINSTANCE hInst,DWORD wDataSeg,LPVOID lpReserved)
{
  switch(wDataSeg)
  {
     case DLL_PROCESS_ATTACH:
       return(1);
     case DLL_PROCESS_DETACH:
       break;
     default:
       return(1);
  }
  return(0);
}
#endif
