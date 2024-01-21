/* termios for WIN32.

   Written by Doug Evans and Steve Chamberlain of Cygnus Support

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */


#include "winsup.h"
#include "sys/termios.h"



int tcsendbreak (int, int )
{
  small_printf ("tcsendbreak\n");
  return 0;
}

int tcdrain (int)
{
  small_printf ("tcdrain\n");
  return 0;
}

int tcflush (int fd, int queue)
{
  int res = 0;

  if (NOT_OPEN_FD (fd))
    {
      set_errno ( EBADF);
      res = -1;
    }
  else
    {
      res =  u->self->hmap[fd].h->tcflush (queue);
    }

  syscall_printf ("%d = tcflush (%d, %d);\n", res, fd,queue);
  return res;
}


int tcflow (int , int)
{
  small_printf ("tcflow\n");
  return 0;
}

#if 0
static void
tdump (int)
{
  termios_printf ("fd %d rb %d wb %d len %d time %d\n",
		  fd,
		  this_procinfo ()->hmap[fd].r_binary,
		  this_procinfo ()->hmap[fd].w_binary,
		  this_procinfo ()->hmap[fd].vmin,
		  this_procinfo ()->hmap[fd].vtime);


}
#endif

#if 0
static void ds (char *when, DCB *s)
{

  termios_printf ("DCB state %s\n", when);
  termios_printf ("DCBlength %x\n", s->DCBlength);
  termios_printf ("BaudRate;    %d\n", s->BaudRate);   
  termios_printf ("fBinary;  %d\n", s->fBinary	 );
  termios_printf ("fParity;   %d\n", s->fParity	  );
  termios_printf ("fOutxCtsFlow	%d\n", s->fOutxCtsFlow);
  termios_printf ("fOutxDsrFlow	%d\n", s->fOutxDsrFlow);
  termios_printf ("fDtrControl	 %d\n", s->fDtrControl);
  termios_printf ("fDsrSensitivity	%d\n", s->fDsrSensitivity);
  termios_printf ("fTXContinueOnXoff	%d\n", s->fTXContinueOnXoff);
  termios_printf ("fOutX:1;      %d\n", s->fOutX	     );
  termios_printf ("fInX:1;       %d\n", s->fInX	      );
  termios_printf ("fErrorChar:1; %d\n", s->fErrorChar	);
  termios_printf ("fNull:1;      %d\n", s->fNull	     );
  termios_printf ("fRtsControl %d\n", s->fRtsControl);
  termios_printf ("fAbortOnError	 %d\n", s->fAbortOnError	);
  termios_printf ("fDummy2:	     %d\n", s->fDummy2);    
  termios_printf ("res1	%d\n", s->res1);
  termios_printf ("XonLim;          %d\n", s->XonLim);         
  termios_printf ("XoffLim;         %d\n", s->XoffLim);        
  termios_printf ("ByteSize;        %d\n", s->ByteSize);       
  termios_printf ("Parity;          %d\n", s->Parity);         
  termios_printf ("StopBits;        %d\n", s->StopBits);       
  termios_printf ("XonChar;         %d\n", s->XonChar);        
  termios_printf ("XoffChar;        %d\n", s->XoffChar);       
  termios_printf ("ErrorChar;       %d\n", s->ErrorChar);      
  termios_printf ("EofChar;         %d\n", s->EofChar);        
  termios_printf ("EvtChar;         %d\n", s->EvtChar);        
  termios_printf ("res2		%d\n", s->res2);

}
#endif


int tcsetattr (int fd, int a, const struct termios *t)
{
  int res = -1;

  if (NOT_OPEN_FD (fd))
    {
      set_errno ( EBADF);
    }
  else
    {
      res =  u->self->hmap[fd].h->tcsetattr (a, t);
    }

  syscall_printf ("%d = tcsetattr (%d, %d, %x);\n", res, fd, a, t);
  return res;
}


int tcgetattr (int fd, struct termios *t)
{
  int res = -1;
  
  if (NOT_OPEN_FD (fd))
    {
      set_errno ( EBADF);
    }
  else
    {
      res =  u->self->hmap[fd].h->tcgetattr (t);
    }

  syscall_printf ("%d = tcgetattr (%d, %x);\n", res, fd, t);
  return res;
}

int tcgetpgrp (int )
{
  return -1;
}
int tcsetpgrp (int , int )
{
  return -1;
}

int setpgid (int , int )
{
  return -1;
}
