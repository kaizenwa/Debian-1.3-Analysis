/*
 * mail.c: Ok, so I gave in.  I added mail checking.  So sue me. 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 */


#include "irc.h"

#ifdef NeXT		/* ugly hack 'cause configure don't grok it -phone */
# define SYSDIR
#endif

#if defined(HAVE_DIRENT_H) || defined(_POSIX_SOURCE)
# include <dirent.h>
# define NLENGTH(d) (strlen((d)->d_name)
#else /* DIRENT || _POSIX_SOURCE */
# define dirent direct
# define NLENGTH(d) ((d)->d_namlen)
# ifdef HAVE_SYS_NDIR_H
#  include <sys/ndir.h>
# endif /* HAVE_SYS_NDIR_H */
# ifdef HAVE_SYS_DIR_H
#  include <sys/dir.h>
# endif /* HAVE_SYS_DIR_H */
# ifdef HAVE_NDIR_H
#  include <ndir.h>
# endif /* HAVE_NDIR_H */
#endif /* HAVE_DIRENT_H || _POSIX_VERSION */

#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#endif /* HAVE_FCNTL_H */

#ifdef ESIX
# include <lan/net_types.h>
#endif /* !ESIX */

/* #if (defined(ISC) || defined(POSIX) || defined (UNICOS)) && !defined(_IBMR2) */
#ifdef HAVE_SYS_FCNTL_H
# include <sys/fcntl.h>
#endif /* HAVE_SYS_FCNTL_H */

#include <sys/stat.h>

#include "mail.h"
#include "lastlog.h"
#include "hook.h"
#include "vars.h"
#include "ircaux.h"
#include "output.h"
#include "window.h"
#include "status.h"
#include "misc.h"

static	char	*mail_path = NULL;

static	void	init_mail _((void));


/* init_mail: this initialized the path to the users mailbox */
static	void
init_mail()
{
	char	buffer[BIG_BUFFER_SIZE+1];
	char	*tmp_mail_path;

	if ((tmp_mail_path = getenv("MAIL")) != NULL)
		strmcpy(buffer, tmp_mail_path, BIG_BUFFER_SIZE);
				/*then use it - Goodi */
	else
	{
		strmcpy(buffer, UNIX_MAIL, BIG_BUFFER_SIZE);
		strmcat(buffer, "/", BIG_BUFFER_SIZE);
		strmcat(buffer, username, BIG_BUFFER_SIZE);
	}
	malloc_strcpy(&mail_path, buffer);
}

/*
 * check_mail_status: returns 0 if mail status has not changed, 1 if mail
 * status has changed 
 */
int
check_mail_status()
{

	struct	stat	stat_buf;
	static	time_t	old_stat = 0L;

	if (!get_int_var(MAIL_VAR))
	{
		old_stat = 0L;
		return (0);
	}
	if (!mail_path)
		init_mail();
	if (stat_file(mail_path, &stat_buf) == -1)
		return (0);
	if (stat_buf.st_ctime > old_stat)
	{
		old_stat = stat_buf.st_ctime;
		return (1);
	}
	return (0);
}

/*
 * check_mail: This here thing counts up the number of pieces of mail and
 * returns it as static string.  If there are no mail messages, null is
 * returned. 
 */

char	*check_mail _((void))
{
#ifndef UNIX_MAIL
	return	NULL;
#else
	static	time_t	old_stat = 0L;
	static	char	ret_str[8] = "";
	struct	stat	stat_buf;
	static  int	i = 0;
	static	int	first_time = 1;
	int	lastlog_level;

	if (!mail_path)
		init_mail();

	if (stat_file(mail_path, &stat_buf) == -1)
		return NULL;
	lastlog_level = set_lastlog_msg_level(LOG_CRAP);
	message_from(NULL, LOG_CRAP);
	if (stat_buf.st_ctime > old_stat)
	{
		char this[] = "\\|/-";
		old_stat = stat_buf.st_ctime;
		if (!first_time)
		{
			if (get_int_var(MAIL_VAR) && do_hook(MAIL_LIST, "%s %s", "Mail", "Yes"))
				put_it("%s", convert_output_format(get_string_var(FORMAT_MAIL_VAR), "%s %s %s", update_clock(GET_TIME), "Mail", "Yes"));
			if (i == 4)
				i = 0;
			sprintf(ret_str, "%c", this[i++]);
		} else
			first_time = 0;
	}
	set_lastlog_msg_level(lastlog_level);
	return (*ret_str? ret_str: NULL);
#endif /* !defined(UNIX_MAIL) */
}
