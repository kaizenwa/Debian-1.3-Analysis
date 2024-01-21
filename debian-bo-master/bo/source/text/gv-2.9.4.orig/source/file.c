/*
**
** file.c
**
** Copyright (C) 1995-1997 Johannes Plass
** 
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 2 of the License, or
** (at your option) any later version.
** 
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
** 
** Author:   Johannes Plass (plass@thep.physik.uni-mainz.de)
**           Department of Physic
**           Johannes Gutenberg-University
**           Mainz, Germany
**
*/

/*
#define MESSAGES
*/
#include "message.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#ifdef VMS
#   include <stat.h>
#else  
#   include <sys/types.h>
#   include <sys/stat.h>
#endif

#include "paths.h"
#include INC_X11(Intrinsic.h)

#include "d_memdebug.h"
#include "file.h"
#include "main_resources.h"
#include "main_globals.h"

/*############################################################*/
/* file_getDirOfPath */
/*############################################################*/

char *
file_getDirOfPath(path)
   char *path;
{
   char *dir=NULL;
   char *pos;

   BEGINMESSAGE(file_getDirOfPath)
#ifdef VMS
   if (path) {
      dir=GV_XtNewString(path);
      pos = strrchr(dir,']');
      if (!pos) pos=strrchr(dir,':');
      if (pos) { pos++; *pos='\0'; }
   }
#else
   if (path) {
      dir=GV_XtNewString(path);
      pos = strrchr(dir,'/');
      if (pos) { pos++; *pos='\0'; }
   }
#endif
#ifdef MESSAGES
   if (!dir) { INFMESSAGE (### Warning: returning NULL) }
   else      { INFSMESSAGE(returning, dir) }
#endif
   ENDMESSAGE(file_getDirOfPath)
   return(dir);
}

/*############################################################*/
/* file_stripVersionNumber */
/*############################################################*/

int
file_stripVersionNumber(filename)
   char *filename;
{
   int strippedVersionNumber=0;

   BEGINMESSAGE(file_stripVersionNumber)
#ifdef VMS
   if (filename) {
      char *pos;
      pos = strrchr(filename,';');
      if (pos) {
         strippedVersionNumber=1;
         *pos='\0';
      }
   }
#endif
   ENDMESSAGE(file_stripVersionNumber)
   return(strippedVersionNumber);
}

/*############################################################*/
/* file_locateFilename */
/*############################################################*/

char *
file_locateFilename(path)
   char *path;
{
   char *tmp=NULL;
   BEGINMESSAGE(file_locateFileName)
   if (path) {
#     ifdef VMS
         tmp = strrchr(path,']');
         if (!tmp) tmp = strrchr(path,':');
#     else
         tmp = strrchr(path,'/');
#     endif
      if (!tmp) tmp=path;
      else tmp++;
      INFSMESSAGE(found,tmp)
   }
   ENDMESSAGE(file_locateFilename)
   return(tmp);
}

/*############################################################*/
/* file_getTmpFilename */
/* provide some temporary file name */
/*############################################################*/

char *
file_getTmpFilename(baseDirectory,baseFilename)
   char *baseDirectory;
   char *baseFilename;
{
   char tempFilename[256];
   char *tempFilenameP;
   char tmpNameBuf[256];
   char tmpDirBuf[256];
   char *tmpName;
   char *tmpExt;
   char *pos;

   BEGINMESSAGE(file_getTmpFilename)

   if (!baseDirectory) baseDirectory = app_res.scratch_dir;
   strcpy(tmpDirBuf,baseDirectory);
   pos = file_locateFilename(tmpDirBuf);
   if (pos) { ++pos; *pos='\0'; }
   else strcpy(tmpDirBuf,app_res.scratch_dir);

   if (!baseFilename) baseFilename= ".";
   strcpy(tmpNameBuf,baseFilename);
   file_stripVersionNumber(tmpNameBuf);
   pos = file_locateFilename(tmpNameBuf);
   if (pos) tmpName = pos;
   else     tmpName = tmpNameBuf;

   pos = strrchr(tmpName,'.');
   if (pos) { *pos='\0'; tmpExt = pos+1; }
   else tmpExt = "";

   if (strlen(tmpName)>20) *(tmpName+20) = '\0';
   if (strlen(tmpExt) >8)  *(tmpExt+8)   = '\0';
   {
      struct stat s;
      int no_such_file;
      int i=1;
      do {
#ifdef VMS
         sprintf(tempFilename,"%sgv_%s_%s_%ld_%d.tmp",tmpDirBuf,tmpName,tmpExt,time(NULL),i);
#else
         sprintf(tempFilename,"%sgv_%s.%s_%ld_%d.tmp",tmpDirBuf,tmpName,tmpExt,time(NULL),i);
#endif
         file_translateTildeInPath(tempFilename);
         no_such_file = stat(tempFilename,&s);
         i++;
      } while (!no_such_file);
   } 
   SMESSAGE(tempFilename)
   tempFilenameP = GV_XtNewString(tempFilename);
   ENDMESSAGE(file_getTmpFilename)
   return(tempFilenameP);
}

/*############################################################*/
/* file_translateTildeInPath */
/* Replaces tilde in string by user's home directory. */
/*############################################################*/

void
file_translateTildeInPath(path)
   char *path;
{
   char *pos;

   BEGINMESSAGE(file_translateTildeInPath)
#ifndef VMS
   if ((pos=strchr(path,'~'))) {
      char *home;
      char tmp[GV_MAX_FILENAME_LENGTH];
      home=getenv("HOME");
      if (strlen(home)+strlen(path)-1 < GV_MAX_FILENAME_LENGTH-1) {
         *pos='\0'; pos++;
         strcpy(tmp,path);
         strcat(tmp,home);
         strcat(tmp,pos);
         strcpy(path,tmp);
      }
   }
#endif /* end of not VMS */
   ENDMESSAGE(file_translateTildeInPath)
}

/*############################################################*/
/* file_fileIsNotUseful */
/*############################################################*/

int
file_fileIsNotUseful(fn)
   char *fn;
{
   struct stat s;
   int retval=0;
   BEGINMESSAGE(file_fileIsNotUseful)
   if (stat(fn,&s)  || (S_ISDIR(s.st_mode)) || s.st_size==0) {
      retval=1;
   }
   IMESSAGE(retval)
   ENDMESSAGE(file_fileIsNotUseful)
   return(retval);
}




