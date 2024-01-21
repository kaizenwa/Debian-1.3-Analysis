#include <stdio.h>
#include <time.h>
#include "multinet_root:[multinet.include]errno.h"

static FILE    *Lfd = NULL;

syslog (pri, fmt, A, B, C, D, E, F, G, H)
int             pri;
char           *fmt;
{
	char           *p,
	               *q,
	                fmt_cpy[512];
	struct tm      *tm,
	               *localtime ();
	long            clock;

	time (&clock);
	tm = localtime (&clock);
	sprintf (fmt_cpy, "%d/%d/%d %2d:%02d:%02d ",
		 tm->tm_mday, tm->tm_mon + 1, tm->tm_year,
		 tm->tm_hour, tm->tm_min, tm->tm_sec);

	for (p = &fmt_cpy[strlen (fmt_cpy)], q = fmt; *q != '\0';)
	{
		if ((q[0] == '%') && (q[1] == 'm'))
		{
			sprintf (p, "%d", socket_errno);
			p = &fmt_cpy[strlen (fmt_cpy)];
			q += 2;
		}
		else
			*p++ = *q++;
	}

	*p = '\0';

	if (Lfd == NULL)
		openlog ();
	fprintf (Lfd, fmt_cpy, A, B, C, D, E, F, G, H);
	closelog ();
}

openlog ()
{
	Lfd = fopen ("TACACS_LOG", "a+");
}

closelog ()
{
	fclose (Lfd);
	Lfd = NULL;
}

setlogmask ()
{
}
