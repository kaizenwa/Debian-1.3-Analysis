/*  VER 014  TAB P   $Id: title.c,v 1.7 1996/11/22 15:45:42 src Exp $
 *
 *  set program title
 *
 *  copyright 1996 Egil Kvaleberg, egilk@sn.no
 *  the GNU General Public License applies
 */

#include "tune.h"

#ifdef PS_STATUS
static char	 ttl_buf[512];		/* program name */
static char	*ttl_arg;		/* status */
static char    **ttl_argv = 0;		/* pointer to argument vector */
static char	*ttl_lastargv = 0;	/* end of argv */
#endif

/*
 *  call this to use proctitle()
 *  does not destroy the argument list (yet)
 *  but they ARE destroyed when progtitle is called
 */
void begintitle(int argc, char **argv, char **envp)
{
#ifdef PS_STATUS
    /*
     *	set start and extent of argv for setproctitle
     *	does not use environment list 
     */
    ttl_argv = argv;
    ttl_lastargv = argv[argc - 1] + strlen(argv[argc - 1]);
#endif
}

/*
 *  set the program title 
 */
void settitle(char *name)
{
#ifdef PS_STATUS
    sprintf(ttl_buf,"%s ",name);
    ttl_arg = ttl_buf+strlen(ttl_buf);
#endif
}

/*
 *  set the program title
 */
void progtitle2(char *what,char *more)
{
#ifdef PS_STATUS
     char *p;
     int i;

     if (!ttl_argv) return;

     /* make heading */
     strcpy(ttl_arg,what);
     if (more) strcat(ttl_arg,more);

     i = strlen(ttl_buf);

     if (i > ttl_lastargv-ttl_argv[0]-2) {
	 i = ttl_lastargv-ttl_argv[0]-2;
	 ttl_buf[i] = '\0';
     }
     strcpy(ttl_argv[0], ttl_buf);
     /* pad remainder */
     p = &ttl_argv[0][i];
     while (p < ttl_lastargv)
	 *p++ = '\0';
#endif
}

/*
 *  set the program title
 */
void progtitle(char *what)
{
    progtitle2(what,"");
}

