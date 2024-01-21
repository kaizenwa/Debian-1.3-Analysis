#include "sliplogin.h"

struct speedtab {
   speed_t symbol;
   int real; 
} speeds[] = {
#ifdef B50
    { B50, 50 },
#endif
#ifdef B75
    { B75, 75 },
#endif
#ifdef B110
    { B110, 110 },
#endif
#ifdef B134
    { B134, 134 },
#endif
#ifdef B150
    { B150, 150 },
#endif
#ifdef B200
    { B200, 200 },
#endif
#ifdef B300
    { B300, 300 },
#endif
#ifdef B600
    { B600, 600 },
#endif
#ifdef B1200
    { B1200, 1200 },
#endif
#ifdef B1800
    { B1800, 1800 },
#endif
#ifdef B2000
    { B2000, 2000 },
#endif
#ifdef B2400
    { B2400, 2400 },
#endif
#ifdef B3600
    { B3600, 3600 },
#endif
#ifdef B4800
    { B4800, 4800 },
#endif
#ifdef B7200
    { B7200, 7200 },
#endif
#ifdef B9600
    { B9600, 9600 },
#endif
#ifdef B19200
    { B19200, 19200 },
#endif
#ifdef B38400
    { B38400, 38400 },
#endif
#ifdef EXTA
    { EXTA, 19200 },
#endif
#ifdef EXTB
    { EXTB , 38400 },
#endif
#ifdef B57600
    { B57600, 57600 },
#endif
#ifdef B115200
    { B115200, 115200 },
#endif
    { 0, 0 }
};

int translate_speed(speed_t speed)
{
struct speedtab *ptr;

for (ptr=speeds; ptr->real != 0; ptr++)
 if (speed == ptr->symbol) return(ptr->real);
return (0);
}

char *sigstr(int s)
{
 static char buf[32];

	switch (s) {
	case SIGHUP:	return("HUP");
	case SIGINT:	return("INT");
	case SIGQUIT:	return("QUIT");
	case SIGILL:	return("ILL");
	case SIGTRAP:	return("TRAP");
	case SIGIOT:	return("IOT");
	case SIGFPE:	return("FPE");
	case SIGKILL:	return("KILL");
	case SIGBUS:	return("BUS");
	case SIGSEGV:	return("SEGV");
	case SIGPIPE:	return("PIPE");
	case SIGALRM:	return("ALRM");
	case SIGTERM:	return("TERM");
	case SIGURG:	return("URG");
	case SIGSTOP:	return("STOP");
	case SIGTSTP:	return("TSTP");
	case SIGCONT:	return("CONT");
	case SIGCHLD:	return("CHLD");
	case SIGTTIN:	return("TTIN");
	case SIGTTOU:	return("TTOU");
	case SIGXCPU:	return("XCPU");
	case SIGXFSZ:	return("XFSZ");
	case SIGVTALRM:	return("VTALRM");
	case SIGPROF:	return("PROF");
	case SIGWINCH:	return("WINCH");
#ifdef SIGLOST
	case SIGLOST:	return("LOST");
#endif
	case SIGUSR1:	return("USR1");
	case SIGUSR2:	return("USR2");
	}
	(void)sprintf(buf, "sig %d", s);
	return(buf);
}
