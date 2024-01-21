/*                             -*- Mode: C++-C -*- 
 * 
 *		 Copyright 1994 Christopher B. Liebman
 *
 *     Permission to use, copy, modify, distribute, and sell this software
 *     and its documentation for any purpose is hereby granted without fee,
 *     provided that the above copyright notice appear in all copies and that
 *     both that copyright notice and this permission notice appear in
 *     supporting documentation, and that the name Christopher B. Liebman not
 *     be used in advertising or publicity pertaining to distribution of this
 *     software without specific, written prior permission.
 *
 *    THIS SOFTWARE IS PROVIDED `AS-IS'.  CHRISTOPHER B. LIEBMAN, DISCLAIMS
 *    ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 *    LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 *    PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL CHRISTOPHER
 *    B. LIEBMAN, BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING SPECIAL,
 *    INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA, OR
 *    PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 *    WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF
 *    OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author          : Chris Liebman
 * Created On      : Tue Jan 11 14:11:30 1994
 * Last Modified By: Chris Liebman
 * Last Modified On: Mon Jan 31 23:10:42 1994
 * Update Count    : 3
 * Status          : Released
 * 
 * HISTORY
 * 31-Jan-1994		Chris Liebman	
 *    Last Modified: Tue Jan 11 14:29:22 1994 #1 (Chris Liebman)
 *    Modified MailCheck to remove code now in CheckMailNow().
 * 
 * PURPOSE
 * 	Check the users mailbox for new mail.
*/

#ifndef lint
static char *RCSid = "$Id: mail_check.c,v 1.7 1994/03/12 21:18:56 liebman Exp $";
#endif

#include "faces.h"
#include <sys/stat.h>

/*
 * Status information about the mail file when we last
 * checked.
*/

static struct stat LastStat;

/*
 * Check for any changes to the mail spool file.
*/

void
MailCheck()
{
    struct stat cur_stat;
    
    /*
     * First we stat the mail file to see if there is any
     * change.
    */
    
    if (stat(TheFacesResources.spool_file, &cur_stat) == -1)
    {
#ifdef MAILCHECK_DEBUG
	fprintf(stderr, "MailCheck: spoolfile non existant: %s\n",
		TheFacesResources.spool_file);
#endif
	bzero((char *) &LastStat, sizeof(struct stat));
	return;
    }
    
#ifdef MAILCHECK_DEBUG
    fprintf(stderr, "MailCheck: last mtime: %d current: %d\n",
	    LastStat.st_mtime, cur_stat.st_mtime);
    fprintf(stderr, "MailCheck: last ctime: %d current: %d\n",
	    LastStat.st_ctime, cur_stat.st_ctime);
    fprintf(stderr, "MailCheck: last size: %d current: %d\n",
	    LastStat.st_size, cur_stat.st_size);
#endif
    
    /*
     * If there has been no change to the mail file then
     * just get out.  Note that we check the ctime as well
     * cause the elm mailer uses utime() to set the mtime
     * field back to what it was when elm read it!  We check the
     * size as an added precaution!
    */
    
#if defined(sun) && defined(SYSV)
    if (LastStat.st_mtim.tv_sec == cur_stat.st_mtim.tv_sec &&
        LastStat.st_mtim.tv_nsec == cur_stat.st_mtim.tv_nsec)
#else
    if ((LastStat.st_mtime == cur_stat.st_mtime) &&
	(LastStat.st_ctime == cur_stat.st_ctime) &&
	(LastStat.st_size  == cur_stat.st_size))
#endif
    {
	/*
	 * Mark all current mail items as in use!
	*/
	
	MailBoxUnClear();
	return;
    }
    
    /*
     *    Save the new stats.
    */
    
    bcopy((char *) &cur_stat, (char *) &LastStat, sizeof(struct stat));
    
    /*
     * Parse the mail spool file and build the mail item list and 
     * display things.
    */
    
    MailBoxParse();
    
    return;
}
