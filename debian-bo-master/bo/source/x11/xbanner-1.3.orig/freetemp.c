#include "xb_config.h"

#include <stdio.h>
#ifndef vms
#include <unistd.h>
#else
#include <ssdef.h>
#endif
#include <string.h>
#include <time.h>

#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>

#include "xbanner.h"

/* for now I need this */
#undef DEBUG

typedef enum { CommandLineSpecified, Unspecified } DispType;

/* Timeout times */
#define TMOUT_ONE (LINGER_DELAY*4)
#define TMOUT_ALL (TMOUT_ONE*20)
                                
int main(int argc, char *argv[])
{
  Display       *disp;
  Atom		 xprop,proptype;
  char	         dispname[80];
  DispType	 dtype = Unspecified;
  Bool		 done=False,exited=False;
  Window	 root;
  unsigned long  items,bytesleft;
  unsigned char *propval;
  int		 last,format;
  unsigned long  waittime,totalwait;

  dispname[0]='\0';

  if(argc==3)
  {
    if(strcmpi("-display",argv[1])==0)
    {
      strcpy(dispname,argv[2]);
      dtype = CommandLineSpecified;
    }
    else
    {
      fprintf(stderr,"Freetemp %s - by Amit Margalit %s\nUsage: freetemp [-display <host>:<display>]\n",
      			VERSION,DATE);
      exit(1);
    }
  }
  else if(argc!=1)
  {
    fprintf(stderr,"Freetemp %s - by Amit Margalit %s\nUsage: freetemp [-display <host>:<display>]\n",
		VERSION,DATE);
    exit(1);
  }

  disp = XOpenDisplay(dtype==CommandLineSpecified ? dispname : NULL);
  if(disp==NULL)
  {
    fprintf(stderr,"%s %s - could not open the display\n",argv[0],VERSION);
    exit(1);
  }

  /* which is the root window ? */
  root = DefaultRootWindow(disp);

#if !defined(vms)

#ifdef DEBUG
  printf("Does prop exist?\n");
#endif
  xprop = XInternAtom(disp,XPROPERTY,True);
  if(xprop == None)		/* nobody's waiting for us... */
    done=True;
  else
    if(!XBPropExists(disp,root,xprop))
    {
      xprop = None;
      done=True;
    }

#ifdef DEBUG
  if(done)
    printf("Nope.\n");
  else
    printf("Yes.\n");
#endif

  /* initialize the overall timer */
  totalwait=0;

  while(!done)
  {
    XGetWindowProperty(disp,root,xprop,0,20,False,XA_STRING,&proptype,
  		&format,&items,&bytesleft,&propval);
#ifdef DEBUG
    printf("Got property: \"%s\"\n",propval);
#endif
    last = propval[strlen((char*)propval)-1] - '0';
#ifdef DEBUG
    printf("Last: %d\n",last);
#endif
    if(propval[0]=='D' && propval[1]==last+'0')
    {
#ifdef DEBUG
      printf("Exited, propval=\"%s\"\n",propval);
#endif
      if(last>0)
      {
#ifdef DEBUG
        printf("Remove one\n");
#endif
        XChangeProperty(disp,root,xprop,XA_STRING,8,PropModeReplace,
        		propval,strlen((char*)propval)-1);
        last--;
        XFree(propval);
        continue;
      }
      if(last<=0)
        done=True;
      XFree(propval);
      continue;
    }
    /* write 'X%d' */
    propval[0]='X'; 
    propval[1]=last+'0';
    XChangeProperty(disp,root,xprop,XA_STRING,8,PropModeReplace,
    		propval,strlen((char*)propval));
#ifdef DEBUG
    printf("Asked #%d to exit.\n",last);
#endif
    XFree(propval);
    exited = False;
    waittime = 0;
#ifdef DEBUG
    printf("Wait till he exits.\n");
#endif
    while(!exited && waittime <= TMOUT_ONE && totalwait <= TMOUT_ALL)
    {
      usleep(LINGER_DELAY);
      waittime+=LINGER_DELAY;
      totalwait+=LINGER_DELAY;
      XGetWindowProperty(disp,root,xprop,0,20,False,XA_STRING,&proptype,
  		&format,&items,&bytesleft,&propval);
#ifdef DEBUG
      printf("Got property: \"%s\"\n",propval);
#endif
      if(propval[0]=='D' && propval[1]==last+'0')
      {
        exited=True;
#ifdef DEBUG
        printf("Exited, propval=\"%s\"\n",propval);
#endif
      }
      else		/* otherwise we can't use it when the while() ends */
        XFree(propval);
    }
#ifdef DEBUG
    if(waittime>TMOUT_ONE)
      printf("Timed out waiting for #%d\n",last);

    printf("Exited=%s\n",(exited?"True":"False"));
#endif

    if(totalwait > TMOUT_ALL)
    {
#ifdef DEBUG
      printf("TotalWait > TMOUT_ALL!!!\n");
#endif
      break;
    }
    if(exited && last>0)
    {
#ifdef DEBUG
      printf("Remove one\n");
#endif
      XChangeProperty(disp,root,xprop,XA_STRING,8,PropModeReplace,
      		propval,strlen((char*)propval)-1);
      last--;
      XFree(propval);
      continue;
    }
    if(exited && last<=0)
      done=True;
    XFree(propval);
  }

  /* sleep as a safety margin */
  usleep(LINGER_DELAY);

  if(xprop!=None)
  {
#ifdef DEBUG
    printf("Del property\n");
#endif
    XDeleteProperty(disp,root,xprop);
  }

#endif /* !defined(vms) */

  /* now free up the temporary resources */
  XKillClient(disp,AllTemporary);
  XSync(disp,True);
  XCloseDisplay(disp);
#ifdef vms
  return SS$_NORMAL;
#else
  return 0;
#endif
}
