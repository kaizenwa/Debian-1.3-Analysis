/*** analog 2.0 ***/
/* Please read Readme.html, or http://www.statslab.cam.ac.uk/~sret1/analog/  */

/*** macstuff.c; stuff only required for the Mac port ***/
/* The functions in this file inside #ifdef MAC_EVENTS are due to, and
   copyright, Jason Linhart (jason@summary.net), 1996. */
/* The functions in this file inside #ifdef MACDIRENT are due to
   Jason Linhart, January 1997. */
/* The functions in this file inside #ifndef NODNS are due to, and
   copyright, Jason Linhart and Stephan Somogyi, 1996, 1997.
   Version history:
   950531  SCS      First release
   960716  JTL      Switched to async OT calls and improved failure cases
                    to not recheck
   960927  JTL      Added MacTCP support and combined check and open into
                    OpenNetwork
*/

#include "analhea2.h"
#ifdef MAC_EVENTS

void MacInit(void)
{
  SIOUXSettings.asktosaveonclose=false;
  printf("Processing...\n");
}

void MacFini(void)
{
  extern char *commandname;
  extern long Mac_good_lines, Mac_bad_lines, Mac_tab_lines;  /* in sscanf.c */
  extern int total_succ_reqs;
  extern flag warnq, anywarns;

  if (!Mac_good_lines && Mac_tab_lines > 4 && total_succ_reqs < 5 && warnq) {
    fprintf(stderr,"%s: Warning: No valid log entries found!\n", commandname);
    fprintf(stderr,"WebStar logs must start with a LOG_FORMAT line something like:\n");
    fprintf(stderr,"!!LOG_FORMAT DATE TIME RESULT URL TRANSFER_TIME BYTES_SENT HOSTNAME REFERER\n");
    fprintf(stderr,"except with the fields listed in the order they actually occur.\n");
    fprintf(stderr,"WebSTAR 1.2.1 or higher should create this line automatically.\n\n");
    anywarns = ON;
  }
  if (!anywarns)
    SIOUXSettings.autocloseonquit=true;
  printf("Complete!\n");
#ifndef NODNS
  ResolverCleanup();
#endif
}

void MacIdle(void)
{
  static long time = 0;

  EventRecord myEvent;
  WindowPtr whichWindow;
  char theChar;

  if (TickCount()<time) return;
  time=TickCount()+6;
  SystemTask();
  if (WaitNextEvent(everyEvent, &myEvent, 1, nil)) {

    if (!SIOUXHandleOneEvent(&myEvent)) switch (myEvent.what) {

    case mouseDown:
      switch (FindWindow(myEvent.where,&whichWindow)) {

      case inMenuBar:
	MenuSelect(myEvent.where);
	break;
      case inSysWindow:
	SystemClick(&myEvent,whichWindow);
	break;
      case inContent:
	SelectWindow(whichWindow);
	break;
      case inDrag:
	DragWindow(whichWindow,myEvent.where,&qd.screenBits.bounds);
	break;
      }
      break;
    case keyDown:
      theChar = myEvent.message & charCodeMask;
      break;
    case updateEvt:
      BeginUpdate((WindowPtr) myEvent.message);
      EndUpdate((WindowPtr) myEvent.message);
      break;
    }
  }
}
#endif   /* (ifdef MAC_EVENTS) */

#ifdef MACDIRENT
/* Assume #ifdef MAC, so MacHeaders already loaded */
static int indx=0;
/* offset to current entry, >0 is open, 0 is closed, -1 is at end */
static CInfoPBRec finfo;
static unsigned char fname[257];

static void
CToPCpy(pstr,cstr)                 /* Convert a C string to a Pascal string */
        unsigned char *pstr;
        char *cstr;
{
        register char *dptr, len;

        len=0;
        dptr=(char *)pstr+1;
        while (len<255 && (*dptr++ = *cstr++)!=0) ++len;
        *pstr=len;
        }

DIR *
opendir (const char *name)           /* Open a directory stream on NAME. */
/* Return a DIR stream on the directory, or NULL if it could not be opened.  */
{
        WDPBRec pb;
        VolumeParam vpb;
        HVolumeParam hvpb;
        int error;

        if (indx) {
                closedir((DIR *)1);
                return((DIR *)0);
                }
        while (name[0]=='.') ++name;
        finfo.hFileInfo.ioCompletion=(IOCompletionUPP)NULL;
        finfo.hFileInfo.ioNamePtr=fname;
        finfo.hFileInfo.ioFVersNum=0;
        finfo.hFileInfo.ioVRefNum=0;

        CToPCpy(fname,name);
        if (fname[fname[0]]!=':') {
                fname[fname[0]+1]=':';
                ++fname[0];
                }

        if (*name) {
                if (*strchr(name,':')) {
                        if (name[0]!=':') {
                                hvpb.ioCompletion=(IOCompletionUPP)NULL;
                                hvpb.ioNamePtr=fname;
                                hvpb.ioVRefNum = -1;
                                hvpb.ioVolIndex = -1;
                                if (!PBHGetVInfo((HParmBlkPtr)&hvpb,FALSE))
				  pb.ioVRefNum=hvpb.ioVRefNum;
                                else return((DIR *)0);
                                CToPCpy(fname,name);
				if (fname[fname[0]]!=':') {
				  fname[fname[0]+1]=':';
				  ++fname[0];
				}
			      }
                        else pb.ioVRefNum=0;
                        pb.ioCompletion=(IOCompletionUPP)NULL;
                        pb.ioNamePtr=NULL/*fname*/;
                        pb.ioWDProcID='MAG^';
                        finfo.hFileInfo.ioFDirIndex=0;
                        finfo.hFileInfo.ioDirID=0;
                        if (!PBGetCatInfo(&finfo,FALSE)) {
                                pb.ioWDDirID=finfo.hFileInfo.ioDirID;
                                if (!PBOpenWD(&pb,FALSE))
				  finfo.hFileInfo.ioVRefNum=pb.ioVRefNum;
                                }
                        else return((DIR *)0);
                        }
                else {
                        vpb.ioNamePtr=hvpb.ioNamePtr=fname;
                        vpb.ioVRefNum=hvpb.ioVRefNum=-1;
                        hvpb.ioVolIndex=vpb.ioVolIndex=-1;
                        if ((!(error=PBHGetVInfo((HParmBlkPtr)&hvpb,FALSE))) &&
			    hvpb.ioVDrvInfo){
                                vpb.ioVRefNum=hvpb.ioVRefNum;
                                vpb.ioNamePtr=NULL;
                                if (!PBGetVInfo((ParmBlkPtr)&vpb,FALSE))
				  finfo.hFileInfo.ioVRefNum=vpb.ioVRefNum;
                                else return((DIR *)0);
                                }
                        else return((DIR *)0);
                        finfo.hFileInfo.ioNamePtr=fname;
                        }
                }
        indx=1;
        return((DIR *)1);
      }

int
closedir (DIR * dirp)
/* Close the directory stream DIRP. Return 0 if successful, -1 if not.  */
{
        if (indx) {
                PBCloseWD((WDPBPtr)&finfo,FALSE);
                indx=0;
                return(0);
                }
        return(-1);
        }

struct dirent *
readdir (DIR * dirp)                 /* Read a directory entry from DIRP. */
/* Return a pointer to a `struct dirent' describing the entry, or NULL for EOF
   or error.  The storage returned may be overwritten by a later readdir call
   on the same DIR stream.  */
{
        finfo.hFileInfo.ioFDirIndex=indx;
        finfo.hFileInfo.ioDirID=0;
        if (indx>0 && !PBGetCatInfo(&finfo,FALSE)) {
                fname[fname[0]+1]=0;
                ++indx;
                return((struct dirent *)(fname+1));
                }
        if (indx) indx = -1;
        return((struct dirent *)0);
        }

void
rewinddir (DIR * dirp)     /* Rewind DIRP to the beginning of the directory. */
{
        if (indx) indx=1;
        }

void
seekdir (DIR * dirp, off_t pos) /* Seek to position POS on DIRP.  */
{
        if (indx && pos>0) indx=pos;
        }

off_t
telldir (DIR * dirp)                 /* Return the current position of DIRP. */
{
        return((indx>0)?indx:-1);
        }

int
dirstat(const char *file_name, struct stat *buf)
/* Special version of stat that only works for most recent dir entry */
{
        if (indx) {
                buf->st_mode=finfo.hFileInfo.ioFlAttrib;
                return(0);
                }
        return(-1);
        }


#endif

#ifdef MAC
#ifndef NODNS
static long OpenNetwork(void);

/* Takes a string of an IP address in *hostname, resolves it to a domain name,
   and returns the name in *hostname.
   Returns nil if unresolvable (or something else went wrong), otherwise
   returns 1. */

/* URL processing and host lookup */

static long slNetChecked = 0, slNetPresent = 0, slNetSvcOpen = 0;
static ResultUPP gMacTCPDNRResultProcUPP = nil;

typedef struct {         /* Open Transport Internet services provider info */
  InetSvcRef ref;        /* provider reference */
  Boolean done;          /* true when asynch operation has completed */
  OTResult result;       /* result code */
  void *cookie;          /* cookie */
} SvcInfo;

static SvcInfo sSvcRef;

int IpAddr2Name(char *hostname)
{
  struct hostInfo hInfoMacTCP;
  OSStatus lStatus;
  OSErr err;
  InetHost lHostAddr;
  int cnt, tmp;
  char *cptr;
  Boolean done;

  if (!slNetChecked) {
    slNetPresent = OpenNetwork();
    slNetChecked = 1;
  }

  if (slNetPresent == 1) {

    /* turn ascii with periods into a long */
    lStatus = OTInetStringToHost(hostname, &lHostAddr);
    if (lStatus != noErr) return 0;

    /* turn the long into a reverse-resolved name */
    sSvcRef.done=false;
    lStatus=OTInetAddressToName(sSvcRef.ref,lHostAddr,hostname);
    if (!lStatus) {
      do {
	MacIdle();
      } while (!sSvcRef.done);
      lStatus=sSvcRef.result;
    }
    if (!lStatus) {
      if (hostname[strlen(hostname)-1]=='.') hostname[strlen(hostname)-1]=0;
      return(1);
    }
  }
  else if (slNetPresent==2) {
    lHostAddr=0;
    cptr=hostname;
    for (cnt=0; cnt<4; ++cnt) {
      if (!isdigit(*cptr)) return(0);
      tmp=atoi(cptr);
      if (tmp<0 || tmp>255) return(0);
      lHostAddr=(lHostAddr<<8)|tmp;
      while (isdigit(*cptr)) ++cptr;
      if (cnt!=3 && *cptr!='.') return(0);
      ++cptr;
    }
    memset(&hInfoMacTCP, 0, sizeof(hInfoMacTCP));
    done=false;
    err = AddrToName(lHostAddr, &hInfoMacTCP, gMacTCPDNRResultProcUPP,
		     (char*)&done);
    if (err == cacheFault) {
      while (!done) MacIdle();
      err = hInfoMacTCP.rtnCode;
    }
    if (err == noErr) {
      hInfoMacTCP.cname[254] = 0;
      strcpy(hostname, hInfoMacTCP.cname);
      if (hostname[strlen(hostname)-1]=='.') hostname[strlen(hostname)-1]=0;
      return(1);
    }
  }
  return 0;
} /* end IpAddr2Name() */


/*      Must call this before quitting app
*/
void
ResolverCleanup(void)
{

  if (slNetChecked && slNetSvcOpen) {
    if (slNetPresent==1) OTCloseProvider(sSvcRef.ref);
    else if (slNetPresent==2) CloseResolver();
  }
} /* end ResolverCleanup() */

/* #pragma mark - */

/*
        Check for availbility of OT/TCP 1.1 or later,
        or MacTCP and open the service.
        Return nil if it isn't.
*/

static pascal void
SvcNotifyProc(void *dataPtr,OTEventCode code,OTResult result,void *cookie)
{
  register SvcInfo *svcInfo;

  svcInfo=(SvcInfo *)dataPtr;
  svcInfo->done=true;
  svcInfo->result=result;
  svcInfo->cookie=cookie;
}

static Boolean
OpenInetServices(SvcInfo *svcInfo)
{
  OSStatus result;

  svcInfo->done=false;
  result=OTAsyncOpenInternetServices(kDefaultInternetServicesPath, 0,
				     SvcNotifyProc, svcInfo);
  if (!result) {
    do {
      MacIdle();
    } while (!svcInfo->done);
    result=svcInfo->result;
  }
  if (result) return(false);
  svcInfo->ref=(InetSvcRef)svcInfo->cookie;
  return(true);
}

static pascal void
MacTCPDNRResultProc (struct hostInfo *hInfoPtr, char *userDataPtr)
{
  *(Boolean*)userDataPtr = true;
}

static long
OpenNetwork(void)
{
  OSStatus lStatus;
  OSErr err;
  long lResponse, lCriteria;

  err = Gestalt(gestaltOpenTpt, &lResponse);
  if (err == noErr)       {
    /* Older OpenTransport Headers require that thenext line read:
       lCriteria = gestaltOpenTptPresent + gestaltOpenTptTCPPresent; */
    lCriteria = gestaltOpenTptPresentMask + gestaltOpenTptTCPPresentMask;
    lResponse = lCriteria & lResponse;
    if (lResponse == lCriteria)     {
      lStatus = InitOpenTransport();
      if (lStatus == noErr) {
	if (OpenInetServices(&sSvcRef)) {
	  slNetSvcOpen=1;
	  return(1);
	}
      }
      return(0);
      /* OT present, but won't open */
    }
  }
  else {
    gMacTCPDNRResultProcUPP = NewResultProc(MacTCPDNRResultProc);
    err = OpenResolver(nil);
    if (err == noErr) {
      slNetSvcOpen=1;
      return(2);
    }
  }
  return(0);
} /* end OpenNetwork() */
#endif
#endif
