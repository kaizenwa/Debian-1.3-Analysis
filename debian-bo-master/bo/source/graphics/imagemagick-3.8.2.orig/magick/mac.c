/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                                                                             %
%                            M   M   AAA    CCCC                              %
%                            MM MM  A   A  C                                  %
%                            M M M  AAAAA  C                                  %
%                            M   M  A   A  C                                  %
%                            M   M  A   A   CCCC                              %
%                                                                             %
%                  Macintosh Utility Routines for ImageMagick.                %
%                                                                             %
%                                                                             %
%                               Software Design                               %
%                                 John Cristy                                 %
%                                September 1996                               %
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
%  The directory routines are strongly based on similiar routines written
%  by Steve Summit, scs@eskimo.com.  The Ghostscript launch code is strongly
%  based on Dave Schooley's Mac Gnuplot and provided by
%  schindall@wave14i.nrl.navy.mil.
%
%
*/

#if defined(macintosh)
/*
  Include declarations.
*/
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <AppleEvents.h>
#include <AERegistry.h>
#include <AEObjects.h>
#include <AEPackObject.h>
#include <Types.h>
#include <Files.h>
#include <Processes.h>
#include "mac.h"

/*
  Forward declaractions.
*/
static Boolean
  SearchForFile(OSType,OSType,FSSpec *,short);

/*
  Function closedir closes the named directory stream and frees the DIR
  structure.
*/
void closedir(DIR *entry)
{
  assert(entry != (DIR *) NULL);
  free((void *) entry);
}
/*
  Execute Ghostscript command.
*/
static OSErr MacGSExecuteCommand(const char *command,long length)
{
  AEAddressDesc
    event_descriptor;

  AEDesc
    reply = {typeNull, NULL};

  AppleEvent
    event = {typeNull, NULL};

  DescType
    descriptor_type;

  int
    error;

  OSType
    id = 'gsVR';

  Size
    actualSize;

  /*
    Send the Apple Event.
  */
  (void) AECreateDesc(typeApplSignature,&id,sizeof(id),&event_descriptor);
  (void) AECreateAppleEvent(id,'exec',&event_descriptor,-1,kAnyTransactionID,
    &event);
  (void) AEPutParamPtr(&event,keyDirectObject,typeChar,command,length);
  (void) AESend(&event,&reply,kAEWaitReply+kAENeverInteract,kAENormalPriority,
    kNoTimeOut,NULL,NULL);
  /*
    Handle the reply and exit.
  */
  (void) AEGetParamPtr(&reply,keyDirectObject,typeInteger,&descriptor_type,
    &error,sizeof(error),&actualSize);
  (void) AEDisposeDesc(&event_descriptor);
  (void) AEDisposeDesc(&event);
  if (reply.descriptorType != NULL)
    AEDisposeDesc(&reply);
  return((OSErr) error);
}
/*
  Launch Ghostscript.
*/
static OSErr MacGSLaunchApplicationCore(long flags)
{
  FSSpec
    file_info;

  LaunchParamBlockRec
    launch_info;

  OSErr
    error;

  ProcessSerialNumber
    serial_number;


  if (!SearchForFile('gsVR','APPL',&file_info,1))
    return(-43);
  launch_info.launchBlockID=extendedBlock ;
  launch_info.launchEPBLength=extendedBlockLen;
  launch_info.launchFileFlags=0 ;
  launch_info.launchControlFlags=launchContinue+launchNoFileFlags+flags;
  launch_info.launchAppSpec=(&file_info);
  launch_info.launchAppParameters=nil;
  error=LaunchApplication(&launch_info);
  return(error);
}
/*
  Launch Ghostscript.
*/
static OSErr MacGSLaunchApplication(void)
{
  return(MacGSLaunchApplicationCore(launchDontSwitch));
}
/*
  Move Ghostscript window to the foreground.
*/
static OSErr MacGSLaunchApplicationToFront(void)
{
  return(MacGSLaunchApplicationCore(0));
}
/*
  Quit Ghostscript.
*/
static void MacGSQuitApplication(void)
{
  AEAddressDesc
    event_descriptor;

  AEDesc
    reply = {typeNull, NULL};

  AppleEvent
    event = {typeNull, NULL};

  OSType
    id = 'GPLT';

  /*
    Send the Apple Event.
  */
  (void) AECreateDesc(typeApplSignature,&id,sizeof(id),&event_descriptor);
  (void) AECreateAppleEvent(typeAppleEvent,kAEQuitApplication,
    &event_descriptor,-1,kAnyTransactionID,&event);
  (void) AESend(&event,&reply,kAENoReply,kAENormalPriority,kNoTimeOut,NULL,
    NULL);
  /*
    Clean up and exit.
  */
  (void) AEDisposeDesc(&event_descriptor);
  (void) AEDisposeDesc(&event);
  if (reply.descriptorType != NULL)
    AEDisposeDesc(&reply);
}
/*
  Set Ghostscript working directory.
*/
static OSErr MacGSSetWorkingFolder(char *newFolder)
{
  AEDesc
    application_descriptor,
    event_descriptor,
    object,
    path_descriptor,
    type_descriptor,
    reply;

  AppleEvent
    event;

  DescType
    folder_type = 'wfdr';

  OSErr
    error;

  OSType
    id = 'GPLT';

  /*
    Send the Apple Event.
  */
  AECreateDesc(typeNull,NULL,0,&application_descriptor);
  AECreateDesc(typeChar,newFolder,strlen(newFolder),&path_descriptor);
  (void) AECreateDesc(typeType,&folder_type,sizeof(DescType),&type_descriptor);
  CreateObjSpecifier(cProperty,&application_descriptor,formPropertyID,
    &type_descriptor,0,&object);
  (void) AECreateDesc(typeApplSignature,&id,sizeof(id),&event_descriptor);
  (void) AECreateAppleEvent(kAECoreSuite,kAESetData,&event_descriptor,-1,
    kAnyTransactionID,&event);
  (void) AEPutParamDesc(&event,keyDirectObject,&object);
  (void) AEPutParamDesc(&event,keyAEData,&path_descriptor);
  error=AESend(&event,&reply,kAENoReply+kAENeverInteract,kAENormalPriority,
    kNoTimeOut,NULL,NULL);
  (void) AEDisposeDesc(&event);
  (void) AEDisposeDesc(&event_descriptor);
  (void) AEDisposeDesc(&object);
  (void) AEDisposeDesc(&type_descriptor);
  (void) AEDisposeDesc(&path_descriptor);
  (void) AEDisposeDesc(&application_descriptor);
  return(error);
}
/*
  Function opendir opens the directory named by filename and associates
  a directory stream with it.
*/
DIR *opendir(char *path)
{
  char
    pathname[2048];

  CInfoPBRec
    search_info;

  DIR
    *entry;

  int
    error;

  search_info.hFileInfo.ioNamePtr=0;
  if ((path != (char *) NULL) || (*path != '\0'))
    if ((path[0] != '.') || (path[1] != '\0'))
      search_info.hFileInfo.ioNamePtr=CtoPstr(strcpy(pathname,path));
  search_info.hFileInfo.ioCompletion=0;
  search_info.hFileInfo.ioVRefNum=0;
  search_info.hFileInfo.ioFDirIndex=0;
  search_info.hFileInfo.ioDirID=0;
  error=PBGetCatInfo(&search_info,0);
  if (error != noErr)
    {
      errno=error;
      return((DIR *) NULL);
    }
  entry=(DIR *) malloc(sizeof(DIR));
  if (entry == (DIR *) NULL)
    return((DIR *) NULL);
  entry->d_VRefNum=search_info.hFileInfo.ioVRefNum;
  entry->d_DirID=search_info.hFileInfo.ioDirID;
  entry->d_index=1;
  return(entry);
}
/*
  Function readdir returns a pointer to a structure representing the
  directory entry at the current position in the directory stream to
  which entry refers.
*/
struct dirent *readdir(DIR *entry)
{
  CInfoPBRec
    search_info;

  int
    error;

  static struct dirent
    dir_entry;

  static unsigned char
    pathname[2048];

  if (entry == (DIR *) NULL)
    return((struct dirent *) NULL);
  search_info.hFileInfo.ioCompletion=0;
  search_info.hFileInfo.ioNamePtr=pathname;
  search_info.hFileInfo.ioVRefNum=0;
  search_info.hFileInfo.ioFDirIndex=entry->d_index;
  search_info.hFileInfo.ioDirID=entry->d_DirID;
  error=PBGetCatInfo(&search_info,0);
  if (error != noErr)
    {
      errno=error;
      return((struct dirent *) NULL);
    }
  entry->d_index++;
  (void) strcpy(dir_entry.d_name,PtoCstr(search_info.hFileInfo.ioNamePtr));
  dir_entry.d_namlen=strlen(dir_entry.d_name);
  return(&dir_entry);
}
/*
  Search for file.
*/
static Boolean SearchForFile(OSType creator_type,OSType file_type,FSSpec *file,
  short count)
{
  char
    *buffer;

  CInfoPBRec
    search1_info,
    search2_info;

  FSSpec
    application;

  HParamBlockRec
    parameter_info;

  long
    buffer_size = 16384;

  OSErr
    error;

  ProcessInfoRec
    application_info;

  ProcessSerialNumber
    serial_number;

  serial_number.lowLongOfPSN=kCurrentProcess;
  serial_number.highLongOfPSN=0;
  application_info.processInfoLength=sizeof(ProcessInfoRec);
  application_info.processName=NULL;
  application_info.processAppSpec=(&application);
  GetProcessInformation(&serial_number,&application_info);
  buffer=NewPtr(buffer_size);
  if (buffer == (char *) NULL)
    return(false);
  parameter_info.csParam.ioCompletion=NULL;
  parameter_info.csParam.ioNamePtr=NULL;
  parameter_info.csParam.ioVRefNum=application.vRefNum;
  parameter_info.csParam.ioMatchPtr=file;
  parameter_info.csParam.ioReqMatchCount=count;
  parameter_info.csParam.ioSearchBits=fsSBFlFndrInfo;
  parameter_info.csParam.ioSearchInfo1=&search1_info;
  parameter_info.csParam.ioSearchInfo2=&search2_info;
  parameter_info.csParam.ioSearchTime=0;
  parameter_info.csParam.ioCatPosition.initialize=0;
  parameter_info.csParam.ioOptBuffer=buffer;
  parameter_info.csParam.ioOptBufSize=buffer_size;
  search1_info.hFileInfo.ioNamePtr=NULL;
  search1_info.hFileInfo.ioFlFndrInfo.fdType=file_type;
  search1_info.hFileInfo.ioFlFndrInfo.fdCreator=creator_type;
  search1_info.hFileInfo.ioFlAttrib=0;
  search1_info.hFileInfo.ioFlParID=0;
  search2_info=search1_info;
  search2_info.hFileInfo.ioFlAttrib=0x10;
  search2_info.hFileInfo.ioFlFndrInfo.fdCreator=creator_type;
  search2_info.hFileInfo.ioFlFndrInfo.fdType=(-1);
  search2_info.hFileInfo.ioFlFndrInfo.fdFlags=0;
  search2_info.hFileInfo.ioFlFndrInfo.fdLocation.h=0;
  search2_info.hFileInfo.ioFlFndrInfo.fdLocation.v=0;
  search2_info.hFileInfo.ioFlFndrInfo.fdFldr=0;
  search2_info.hFileInfo.ioFlParID=0;
  error=PBCatSearchSync((CSParamPtr) &parameter_info);
  DisposePtr(buffer);
  if (parameter_info.csParam.ioReqMatchCount ==
      parameter_info.csParam.ioActMatchCount)
    error=eofErr;
  if (parameter_info.csParam.ioActMatchCount == 0)
    error=0;
  return(error == eofErr);
}
/*
  Function seekdir sets the position of the next readdir() operation
  on the directory stream.
*/
void seekdir(DIR *entry,long position)
{
  assert(entry != (DIR *) NULL);
  entry->d_index=position;
}
/*
  Macintosh emulation of the UNIX system command.
*/
int systemMAC(const char * command)
{
  /*
    We only know how to launch Ghostscript.
  */
  if (MacGSLaunchApplicationToFront())
    return(-1);
  return(MacGSExecuteCommand(command,strlen(command)));
}
/*
  Function telldir returns the current location associated  with  the
  named directory stream.
*/
long telldir(DIR *entry)
{
  return(entry->d_index);
}
#endif
