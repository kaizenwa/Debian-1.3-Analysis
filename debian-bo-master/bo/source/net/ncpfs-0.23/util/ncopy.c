/****************************************************************************
 *  ncopy.c
 *
 *  Copy file on a Netware server without Network Traffic
 *
 *  Copyright (C) 1996 by Brian Reid and Tom Henderson.
 * 
 *   Send bug reports for ncopy to "breid@tim.com"
 *
 *  Still to do: support recursive copy with two arguments
 *  Both must be directories. (similar to rcp -r)
 *
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <ctype.h>
#include <mntent.h>
#include <fcntl.h>
#include <signal.h>
#include "ncplib.h"


struct NCPMountRec
{
  char *mountDir;
  char *server;
  struct ncp_conn *conn;
};

/****************************************************************************
 * Globals:
 *
 */
const char *VersionStr = "0.1";
char * ProgramName;

struct NCPMountRec *NcpMountTable = NULL;
int ncpCount = 0;

/* (initialized) command options  */

int optVersion=0;               /* -V TRUE if just want version */
int optVerbose=0;               /* -v TRUE if want verbose output */
int optNice=0;                  /* -n TRUE if we are cooperative (nice) */
int optNiceFactorSel=0;         /* -s TRUE if we selected a nice factor */
int optNiceFactor=10;           /* -s arg, number of 100K blocks to copy 
                                           before sleeping for a second */
__u32 CopyBlockSize = 100000;   /* Size of the default block copy size */
unsigned int NiceSleepTime=1;   /* Number of seconds to sleep in Nice Mode */

int BlocksCopied=0;             /* Number of blocks copied */
int MaxNcopyRetries=25;         /* Maximum number of times to retry a failed
                                   copy before giving up  */

/* Globals needed for signal handlers */
int OutputOpen=0;               /* True if the ncp output file is open */
struct ncp_conn *CurrentConn = NULL;  /* Connection of output file */
struct ncp_file_info *CurrentFile = NULL; /* File info of output file */

/* Signal control structures */
static struct sigaction sHangupSig;
static struct sigaction sInterruptSig;
static struct sigaction sQuitSig;
static struct sigaction sTermSig;

/****************************************************************************
 *
 */
static void usage()
{
  fprintf(stderr,"usage: %s [-V]\n", ProgramName);
  fprintf(stderr,"       %s [-vn] [-s amt] sourcefile destinationfile|directory\n", ProgramName);
  fprintf(stderr,"       %s [-vn] [-s amt] sourcefile [...] directory\n", ProgramName);
}

/****************************************************************************
 * Return pointer to last component of the path.  
 * Returned string may have one or more "/" left on the end. 
 * ("/" returns pointer to "/", null returns pointer to null)
 * Return pointer to original string if no "/" in string. (except at end)
 */
static const char *myBaseName(const char *path) 
{
  const char *p;

  for(p = &path[strlen(path)]; p != path; p--) {  /* skip ENDING "/" chars */
    if(*p && *p != '/') break;
  }
  if(p==path) return p;
  for( ; p != path || *p == '/'; p--) {
    if(*p == '/') return ++p;
  }
  return p;
}

/****************************************************************************
 *
 */
static const char *notDir(const char *path)
{
  struct stat buf;
  static const char *notDirectory="not a directory";

  if(stat(path, &buf)) return strerror(errno); /* no permission? not exist? */
  if(!S_ISDIR(buf.st_mode)) return notDirectory;         /* not a directory */
  return (char *) 0;                                                  /* OK */
}

/****************************************************************************
 *
 */
static int handleOptions(const int argc, char * const argv[])
{
  int opt;

  while ((opt = getopt(argc, argv, "vVns:")) != EOF)
  {
    switch (opt) {

    case 'V':                /* Version */
      optVersion=1;
      break;

    case 'v':                /* Verbose output */
      optVerbose=1;
      break;

    case 'n':                /* Nice, cooperative copy */
      optNice=1;
      break;

    case 's':                /* Nice Factor */
      optNiceFactorSel=1;
      optNiceFactor=atoi(optarg);
      if (optNiceFactor < 1) {
        fprintf(stderr,"%s: -s option requires positive numeric argument > 0\n",
        ProgramName);
        return 1;
      }
      break;

    default:   /* invalid options or options without required arguments */
      return 1;
    }
    continue;
  }
  return 0;
}

/****************************************************************************
 * TODO: if recursive flag last MUST be a directory, even if only 2 args.
 */
static int validateFileArgs(const int argc, char * const argv[])
{
  const char *p;
  if (argc == 0) {
    fprintf(stderr,"%s: No arguments specified.\n", ProgramName);
    return 1;
  }
  if(argc == 1) {
    fprintf(stderr,"%s: No destination specified.\n", ProgramName);
    return 1;
  }
  if((argc > 2) && (p=notDir(argv[argc-1]))) { /* last arg MUST be dir */
    fprintf(stderr,"%s: %s: %s\n", ProgramName, argv[argc-1], p);
    return 1;
  }
  return 0;
}

/****************************************************************************
 * Duplicate a string.
 */
char *duplicateStr(const char *InStr)
{
  char *dup;
  if (!InStr) return NULL;
  dup = (char*)malloc(strlen(InStr)+1);
  if (dup)
    strcpy(dup,InStr);
  return dup;
}

/****************************************************************************
 * load a table of ncpfs mount points.
 */
int loadMountTable()
{
  FILE *mountedFile;
  struct mntent *mountEntry = NULL;
  ncpCount = 0; 
  if ( (mountedFile = fopen(MOUNTED,"r")) == NULL) {
    fprintf(stderr,"ncopy: cannot open %s, %s\n",MOUNTED,strerror(errno));
    return 1;
  }
  
  while ( (mountEntry = getmntent(mountedFile)) != NULL) {
    if (!strcmp(mountEntry->mnt_type,"ncpfs"))
      ncpCount++;
  }
  if (ncpCount) {
    NcpMountTable = (struct NCPMountRec*)
                 malloc(ncpCount * sizeof(struct NCPMountRec));
    if (!NcpMountTable) { 
      fprintf(stderr,"Out of memory\n");
      fclose(mountedFile);
      return 1;
    }
    fseek(mountedFile,0,SEEK_SET);
    ncpCount = 0;
    while ( (mountEntry = getmntent(mountedFile)) != NULL) {
      if (!strcmp(mountEntry->mnt_type,"ncpfs")) {
        NcpMountTable[ncpCount].mountDir = duplicateStr(mountEntry->mnt_dir);
        NcpMountTable[ncpCount].server = 
                   duplicateStr(mountEntry->mnt_fsname);
        NcpMountTable[ncpCount].conn = NULL;
        ncpCount++;
      }
    }
  }
  fclose(mountedFile);
  return 0;
}

/****************************************************************************
 * Releases the table of ncpfs mount points.
 */
void releaseMountTable()
{
  int loop;
  if (!ncpCount) return;
  for (loop = ncpCount; loop; loop--,ncpCount--) {
    if (NcpMountTable[loop-1].conn) {
      ncp_close(NcpMountTable[loop-1].conn);
      NcpMountTable[loop-1].conn = NULL;
    }
    free(NcpMountTable[loop-1].server);
    free(NcpMountTable[loop-1].mountDir);
  }
  free(NcpMountTable);
}

/****************************************************************************
 * Finds the index into the mount point table that enables ncp copy for
 * the file.
 * Returns -1 if the files do not reference the same server.
 */
int ncpIndex(const char *InputFile, const char *OutputFile)
{
  int loop;
  char *mountDir;
  if (!ncpCount) return -1;
  
  for (loop = 0; loop < ncpCount; loop++) {
    mountDir = NcpMountTable[loop].mountDir; 
    if (!strncmp(mountDir,InputFile,strlen(mountDir)) &&
        !strncmp(mountDir,OutputFile,strlen(mountDir))) return loop;
  }
  return -1;
}


/****************************************************************************
 * Does a regular buffered file copy.
 * This is used if we cannot use the Netware file copy.
 */
int normalFileCopy(const char *InputFile, const char *OutputFile,
                   char *Buffer,int BufferSize,
                   const char *paramInputFile,
                   const char *paramOutputFile)
{
  int fdIn, fdOut;
  long fileSize,totalSize;
  struct stat statBuf;
  fdIn = open(InputFile,O_RDONLY);
  if (fdIn == -1) {
    fprintf(stderr,"%s: Cannot open %s, %s\n",ProgramName,paramInputFile,
            strerror(errno));
    return 1;
  }
  if (fstat(fdIn,&statBuf)) {
    fprintf(stderr,"%s: Cannot stat %s, %s\n",ProgramName,paramInputFile,
            strerror(errno));
    close(fdIn);
    return 1;
  }
  if(S_ISDIR(statBuf.st_mode)) {
    close(fdIn);
    fprintf(stderr,"%s: %s: omitting directory\n",ProgramName,paramInputFile);
    return 0;           /* At this point, don't consider this a fatal error */
  }

  fdOut = open(OutputFile,O_CREAT | O_TRUNC | O_WRONLY,statBuf.st_mode);
  if (fdOut == -1) {
    fprintf(stderr,"%s: Cannot create %s, %s\n",ProgramName,paramOutputFile,
           strerror(errno));
    close(fdIn);
    return 1;
  }
  fileSize = lseek(fdIn,0,SEEK_END);
  if (fileSize < 0) {
    fprintf(stderr,"%s: lseek error on %s, %s\n",ProgramName,paramInputFile,
            strerror(errno));
    close(fdOut);
    close(fdIn);
    return 1;
  }
  lseek(fdIn,0,SEEK_SET);
  if (optVerbose) {
    printf("Normal copy: %s -> %s 0%%",paramInputFile,paramOutputFile);
    fflush(stdout);
  }
  totalSize = fileSize;
  while (fileSize) {
    int currentMove;
    int writeAmt;
    currentMove = (fileSize > BufferSize) ? BufferSize : fileSize;
    if (read(fdIn,Buffer,currentMove) != currentMove) {
      fprintf(stderr,"%s: Error reading %s, %s\n",ProgramName,paramInputFile,
              strerror(errno));
      close(fdIn);
      close(fdOut);
      return 1;
    }
    writeAmt = write(fdOut,Buffer,currentMove);
    if (writeAmt < 0) {
      fprintf(stderr,"%s: Error writing %s, %s\n",ProgramName,paramOutputFile,
              strerror(errno));
      close(fdIn);
      close(fdOut);
      return 1;
    } else if (writeAmt == 0) {
      fprintf(stderr,"%s: Out of space on destination device writing %s\n",
              ProgramName,OutputFile);
      close(fdIn);
      close(fdOut);
      return 1;
    }
    fileSize -= currentMove;
    if (optVerbose) {
      printf("\rNormal copy: %s -> %s %ld%%",paramInputFile,paramOutputFile,(100 - (fileSize * 100/totalSize)));
      fflush(stdout);
    }
  }
  close(fdOut);
  close(fdIn);
  if (optVerbose)
    printf("\n");
  return 0;
}

/****************************************************************************
 *  Converts a string to upper case.
 *  Netware file names need to be all upper case.
 */
char *upString(char *str)
{
  char *alias = str;
  while (*alias) {
    *alias = toupper(*alias);
    ++alias;
  }
  return str;
}

/****************************************************************************
 *  Locates the first occurrance of a single character in the input string.
 *  returns -1 if the character is not found.
 */
int stringPosition(const char *str,char token)
{
  const char *alias = str;
  while (*alias) {
    if (*alias == token) return alias - str;
    alias++;
  }
  return -1;
}

/****************************************************************************
 *  Walks up the directory path building info structures along the way
 *  in order to get a dir_handle.
 *  This will mangle the input "FileString", leaving just the file name
 *  component in it when it is finished.
 */
int getDirHandle(struct ncp_conn *conn, char *FileString, __u8 *NewDirHandle)
{
  struct nw_info_struct info1,info2;
  int currentLevel = 0;
  int k;
  struct nw_info_struct *parentInfo = NULL;
  struct nw_info_struct *currentInfo = NULL;

  while ( (k = stringPosition(FileString,'/')) >= 0) {
    FileString[k] = 0;
    if (!currentLevel) {
      parentInfo = NULL;
      currentInfo = &info1; 
    } else if (currentLevel % 2) {
      parentInfo = &info1;
      currentInfo = &info2;
    } else {
      parentInfo = &info2;
      currentInfo = &info1;
    }
    if (ncp_do_lookup(conn, parentInfo, FileString,
                      currentInfo) != 0) {
      fprintf(stderr,"%s: Ncp lookup failed on directory %s--%s\n", 
              ProgramName,FileString,strerror(errno));
      return 1;
    }
    ++currentLevel;
    memmove(FileString,FileString+k+1,strlen(FileString+k+1)+1);
  }
	
  if (ncp_alloc_short_dir_handle(conn, currentInfo, NCP_ALLOC_TEMPORARY, 
                                 NewDirHandle) != 0) {
    fprintf(stderr,"%s: Ncp alloc dir handle failed--%s\n", 
            ProgramName,strerror(errno));
    return 1;
  }
  return 0;
}


/****************************************************************************
 *  Interfaces with the ncplib to do the netware copy of the file.
 */
int netwareCopyFile(int ncpMountIndex, const char *sourcefile,
                    const char *destfile,
                    const char *paramInputFile,
                    const char *paramOutputFile)
{
   __u8 source_dir_handle;
   __u8 dest_dir_handle;
   struct ncp_file_info source_file;
   struct ncp_file_info dest_file;
   __u32 amountCopied;
   __u32 amtLeft;
   __u32 totalSize;
   __u32 sourceOff;
   __u32 thisMove;
   int stroffset;
   int retValue;
   char *sourceDup;
   char *destDup;
   struct ncp_conn *sourceconn;
   int retryCount;
   long err = 0;

   /* Establish a connection to a Netware mount point if
      one is not already established. */
   if (!NcpMountTable[ncpMountIndex].conn) {
     NcpMountTable[ncpMountIndex].conn = 
          ncp_open_mount(NcpMountTable[ncpMountIndex].mountDir,&err);
      if (err) {
        com_err(ProgramName,err,"opening ncp connection on mount point %s",
                NcpMountTable[ncpMountIndex].mountDir);
        return 2; 
      }
   }
   sourceconn = NcpMountTable[ncpMountIndex].conn;
 
   /* Duplicate and upper case the file names so we do not trample
      on the input strings */
   stroffset = strlen(NcpMountTable[ncpMountIndex].mountDir) + 1; 
   sourceDup = duplicateStr(sourcefile+stroffset);
   destDup = duplicateStr(destfile+stroffset);
   if (!sourceDup || !destDup) {
     fprintf(stderr,"%s: Malloc failed duplicating file names\n",
             ProgramName);
     return 2;
   }

   upString(sourceDup);
   upString(destDup);

   /* Get Handles to the input and output directories */
   if (getDirHandle(sourceconn,sourceDup,&source_dir_handle) || 
       getDirHandle(sourceconn,destDup,&dest_dir_handle)) {
     free(sourceDup);
     free(destDup);
     return 1;
   }

   /* Open the input and output files. */
   if (ncp_open_file(sourceconn, source_dir_handle, sourceDup,0,AR_READ,
                     &source_file) != 0) {
     fprintf(stderr,"%s: Cannot open %s--%s\n",
             ProgramName,paramInputFile,strerror(errno));
     free(sourceDup);
     free(destDup);
     return 1;
   }

   if (ncp_create_file(sourceconn, dest_dir_handle, destDup, 
                       source_file.file_attributes, &dest_file) != 0) {
     fprintf(stderr,"%s: Cannot create %s--%s\n",ProgramName, paramOutputFile,
             strerror(errno));
     ncp_close_file(sourceconn,source_file.file_id);
     free(sourceDup);
     free(destDup);
     return 1;
   }
   /* Set globals in case a signal happens while copying */
   CurrentConn = sourceconn;
   CurrentFile = &dest_file;
   OutputOpen = 1;

   free(sourceDup);
   free(destDup);

   retValue = 0;
   if (optVerbose) {
     printf("NetWare copy: %s -> %s 0%%",paramInputFile,paramOutputFile);
     fflush(stdout);
   }

   /* The main copy loop.  */

   amtLeft = totalSize = source_file.file_length;
   sourceOff = 0;
   retryCount = 0;

   while (amtLeft && retryCount < MaxNcopyRetries) {
     int ncopyRetValue;
     if (amtLeft > CopyBlockSize)
       thisMove = CopyBlockSize;
     else
       thisMove = amtLeft;
     /* If we are being nice and we've copied enough blocks, go to sleep */
     if (optNice) {
       if (BlocksCopied == optNiceFactor) {
         sleep(NiceSleepTime);
         BlocksCopied=0;            
       } else
        ++BlocksCopied;
     }
     ncopyRetValue = ncp_copy_file(sourceconn, source_file.file_id,
                                   dest_file.file_id, sourceOff,sourceOff,
                                   thisMove,&amountCopied);
     if (ncopyRetValue != 0) { 
          /* In my testing this only happens when you run out of space 
             on the server.
             Netware seems to wait a bit before reporting space recently
             free'd.  I will just wait a bit before bombin */
	sleep(1);                   /* Sleep for a second and try again */
	retryCount++;
        amountCopied = thisMove = 0;
     }
     if (amountCopied != thisMove) {
       fprintf(stderr,"%s: Warning, amountCopied (%u) != thisMove (%u)\n",
               ProgramName,(unsigned int)amountCopied,(unsigned int)thisMove);
     }
#ifdef NCOPY_DEBUG
     fprintf(stderr,"Copied %u (actual %u)\n",(unsigned int)thisMove,
             (unsigned int)amountCopied);
#endif
     amtLeft -= amountCopied;
     sourceOff += amountCopied;
     if (optVerbose) {
       printf("\rNetWare copy: %s -> %s %ld%%",paramInputFile,paramOutputFile,
              (100 - (long)((float)amtLeft /(float)totalSize * 100.0)));
       if (retryCount)
          printf("  %d retries",retryCount);
       fflush(stdout);
     }
   }
   if (retryCount >= MaxNcopyRetries)
     retValue = 1;
   if (optVerbose)
     printf("\n");
   if (ncp_close_file(sourceconn,dest_file.file_id) != 0) {
      fprintf(stderr,"%s: Close failed for %s\n",ProgramName,paramOutputFile);
      retValue = 1;
   }

   /* Clear signal handling globals */
   OutputOpen = 0;
   CurrentConn = NULL;
   CurrentFile = NULL;

   if (ncp_close_file(sourceconn,source_file.file_id) != 0) {
      fprintf(stderr,"%s: Close failed for %s\n",ProgramName,paramInputFile);
      retValue = 1;
   }

   if (ncp_dealloc_dir_handle(sourceconn, dest_dir_handle) != 0)
   {
      fprintf(stderr,"%s: Dealloc dir handle error for %s\n",ProgramName,
              paramOutputFile);
      retValue = 1;
   }
   if (ncp_dealloc_dir_handle(sourceconn, source_dir_handle) != 0)
   {
      fprintf(stderr,"%s: Dealloc dir handle error for %s\n",ProgramName,
              paramInputFile);
      retValue = 1;
   }
   return retValue;
}


/****************************************************************************
 *  Decides whether to use the traditional file copy or the netware remote
 *  file copy.
 */
int copyFiles(const char *realsource, const char *realdestination,
              const char *paraminputfile, const char *paramoutputfile)
{
  int oldUMask;
  char fileBuffer[24000];
  int retVal = 0;
  int ncpMountIndex = ncpIndex(realsource,realdestination);
#ifdef NCOPY_DEBUG
  printf("Real Source '%s'\n"
         "Real Dest   '%s'\n"
         "Param Src   '%s'\n"
         "Param Dest  '%s'\n",realsource,realdestination,paraminputfile,
         paramoutputfile);
#endif

  oldUMask = umask(0);
  if (ncpMountIndex < 0) 
    retVal = normalFileCopy(realsource,realdestination,fileBuffer,
                            sizeof(fileBuffer),
                            paraminputfile,paramoutputfile);
  else
    retVal = netwareCopyFile(ncpMountIndex,realsource,realdestination,
                             paraminputfile,paramoutputfile);
  umask(oldUMask);
  return retVal;
}


/****************************************************************************
 * 
 * HERE
 *
 * Brian may NEED "fake" path if he prints error messages?
 * or I may need a way to get his error messages so I can
 * print them with the "fake" path.
 * My current error messages are on the REAL path, which would be confusing...
 *
 * (1-source problem, 2-destination problem, 3-other fatal)
 * We need to decide when to exit or continue the loop, 
 * and what to return when we do exit the loop.
 * Is it failure if 3 files are to be copied, and 1 fails?
 * If one copy fails, we stay in the loop, right?
 * Is it failure if destination fails?
 * Do we Stay in the loop?
 */
static int copyRealPaths(const char *source, const char *destination)
{
  char realsource[MAXPATHLEN*2];
  char realdestination[MAXPATHLEN*2];
  char dirPart[MAXPATHLEN+1];
  char filePart[MAXPATHLEN+1];
  const char *p;

  if(realpath(source, realsource) == 0) { /* the source must at least exist */
    fprintf(stderr,"%s: %s: %s\n", 
    ProgramName, source, strerror(errno));
    return 1;                                /* indicate a "source" problem */
  }
  if(realpath(destination, realdestination) == 0) {/* dest file missing? OK */
    strncpy(dirPart, destination, MAXPATHLEN);   /* but "dirpart" must work */
    dirPart[MAXPATHLEN] = 0;
    p=myBaseName(dirPart);
    strcpy(filePart, p);
    dirPart[p - dirPart] = 0; /* isolates "directory" part from "file part" */
    if(realpath(dirPart, realdestination) == 0) { 
      fprintf(stderr,"%s: %s: %s\n", 
      ProgramName, dirPart, strerror(errno));
      return 2;                         /* indicate a "destination" problem */
    }
      if(*realdestination != '/' || *(realdestination+1)) strcat(realdestination, "/");
    strcat(realdestination, filePart);
  } 
  /* becomes prog exit code */

  /* Test Cases: (Where file/dir may or may not exist)
   *     "", file, file/, dir, dir/
   *     /, //, /dir, /dir/, /file, /file/, 
   *     /tmp/file, /tmp/file/, tmp/file, tmp/file/,
   *     /tmp/dir,  /tmp/dir/,  tmp/dir, tmp/dir/
   */
  return copyFiles(realsource, realdestination,source,destination); 
}

/****************************************************************************
 * guaranteed argc is at least 2 and
 * if argc > 2 last parameter is a directory
 * by validateFileArgs()
 */
static int handleFileArgs(int argc, char * const argv[])
{
  int loop;
  const char *destination;
  int copyStatus;
  int returnCode=0;                            /* default program exit code */
  const char *baseNamePtr;
  char destinationfile[MAXPATHLEN*2];

  destination=argv[argc-1];                            /* get LAST argument */
  for (loop = 0; loop < (argc-1); loop++) { /* all file arguments, but last */
    strncpy(destinationfile, destination, MAXPATHLEN);
    destinationfile[MAXPATHLEN]=0;
    if((argc > 2) || (!notDir(argv[argc-1]))) {     /* destination is a dir */
      if(*destinationfile != '/' || *(destinationfile+1)) strcat(destinationfile,"/");
      baseNamePtr=myBaseName(argv[loop]);              /* get the file name */
      strcat(destinationfile,baseNamePtr);    /* add it on end of directory */
    }
    copyStatus=copyRealPaths(argv[loop], destinationfile);   /* do the copy */
    if(copyStatus > 1) return copyStatus;             /* fatal failure? bye */
    if(copyStatus == 1) returnCode=1; /* a partial failure? we can continue */
  }
  return returnCode;           /* return what will be the program exit code */
}
/****************************************************************************
 * 
 */
static void handleSignals(int sigNumber) 
{
  /* Ignore Signal Handling while cleaning up */

  /* SIGHUP */
  sHangupSig.sa_handler=SIG_IGN; 
  if(sigaction(SIGHUP, &sHangupSig, NULL) == -1) { 
    fprintf(stderr,"%s: Reset to ignore SIGHUP signal failed: %s",
    ProgramName, strerror(errno));
  }
  /* SIGINT */
  sInterruptSig.sa_handler=SIG_IGN; 
  if(sigaction(SIGINT, &sInterruptSig, NULL) == -1) { 
    fprintf(stderr,"%s: Reset to ignore SIGINT signal failed: %s",
    ProgramName, strerror(errno));
  }
  /* SIGQUIT */
  sQuitSig.sa_handler=SIG_IGN; 
  if(sigaction(SIGQUIT, &sQuitSig, NULL) == -1) { 
    fprintf(stderr,"%s: Reset to ignore SIGQUIT signal failed: %s",
    ProgramName, strerror(errno));
  }
  /* SIGTERM */
  sTermSig.sa_handler=SIG_IGN; 
  if(sigaction(SIGTERM, &sTermSig, NULL) == -1) { 
    fprintf(stderr,"%s: Reset to ignore SIGTERM signal failed: %s",
    ProgramName, strerror(errno));
  }

  /* If we don't close the ncp output file, we have to ncpumount and
     ncpmount before we can get rid of it.  */
  if (OutputOpen) {
    /*  Issue a warning if we cannot close the file */
    /*  If an error occurs we probably have to umount/mount to 
        remove the file */
    if (ncp_close_file(CurrentConn,CurrentFile->file_id) != 0) {
      fprintf(stderr,"%s: unclean close of output file",ProgramName);
    }
    OutputOpen = 0;
  }

  exit(128 + sigNumber);
}

/****************************************************************************
 * We'll trap Hangup, Interrupt, Quit or Terminate
 */
static int trapSignals()
{
  if(sigaction(SIGHUP, NULL, &sHangupSig)) {  /* init structure fields */
    fprintf(stderr,"%s: Get HANGUP signal action failed: %s",
    ProgramName, strerror(errno));
    return 1;
  }
  sHangupSig.sa_handler = handleSignals;
  if(sigaction(SIGHUP, &sHangupSig, NULL) == -1) { 
    fprintf(stderr,"%s: Reset HANGUP signal action failed: %s",
    ProgramName, strerror(errno));
    return 1;
  }
  if(sigaction(SIGINT, NULL, &sInterruptSig)) {  /* init structure fields */
    fprintf(stderr,"%s: Get INTERRUPT signal action failed: %s",
    ProgramName, strerror(errno));
    return 1;
  }
  sInterruptSig.sa_handler = handleSignals;
  if(sigaction(SIGINT, &sInterruptSig, NULL) == -1) { 
    fprintf(stderr,"%s: Reset INTERRUPT signal action failed: %s",
    ProgramName, strerror(errno));
    return 1;
  }
  if(sigaction(SIGQUIT, NULL, &sQuitSig)) {  /* init structure fields */
    fprintf(stderr,"%s: Get QUIT signal action failed: %s",
    ProgramName, strerror(errno));
    return 1;
  }
  sQuitSig.sa_handler = handleSignals;
  if(sigaction(SIGQUIT, &sQuitSig, NULL) == -1) { 
    fprintf(stderr,"%s: Reset QUIT signal action failed: %s",
    ProgramName, strerror(errno));
    return 1;
  }
  if(sigaction(SIGTERM, NULL, &sTermSig)) {  /* init structure fields */
    fprintf(stderr,"%s: Get TERMINATE signal action failed: %s",
    ProgramName, strerror(errno));
    return 1;
  }
  sTermSig.sa_handler = handleSignals;
  if(sigaction(SIGTERM, &sTermSig, NULL) == -1) { 
    fprintf(stderr,"%s: Reset TERMINATE signal action failed: %s",
    ProgramName, strerror(errno));
    return 1;
  }
  return 0;
}

/****************************************************************************
 *
 */
int main(int argc, char * const argv[])
{
  int returnCode;
  ProgramName=argv[0];

  if(handleOptions(argc, argv)) {  /* bad option, missing option parameter */
    usage();
    return 1;
  }
  if(optVersion) {              /* only option not requiring any arguments */
    printf("%s version %s\n", ProgramName, VersionStr);
    return 0;
  }
  if(validateFileArgs(argc - optind, argv + optind)) {  
    usage();
    return 1;
  }
  if(trapSignals()) return 1;
  loadMountTable();
  returnCode = handleFileArgs(argc - optind, argv + optind);
  releaseMountTable();
  return returnCode; 
}
