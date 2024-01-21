#include "kiss.h"

/****************************************************************************
From cdr@sme.siemens.comThu Jul  6 09:31:51 1995
Date: Wed, 05 Jul 1995 16:12:50 EDT
From: CD Rasmussen <cdr@sme.siemens.com>
To: Karel Kubat <karel@bambix.icce.rug.nl>
Subject: kiss mount/umount that supports mtab and df

Karel,
In a previous message you mentioned that I was welcome to add the mtab
code myself to support df so I did.  If you find this acceptable, I am
honored to contribute to your project.  I have only tested these
changes with the bkiss but assume the others will work.

I am including the sources to domount.c and doumount.c.  Please let me
know what you think.  This makes df work on my system.

Cheers,
  Constantine Rasmussen      508-750-7500 x7020     cdr@sme.siemens.com
**************************************************************************/

/* This code relies on a single space field delimiter written by mount -cdr
   does not handle /proc
 */
static void mtabUpdate(char* name)
{
    FILE
	*mtab = fopen(MTAB, "r");

    if (mtab)
    {
	int
	    found = 0;
	FILE
	    *tmp = fopen("/etc/mtab-tmp", "w");

	if (! tmp)
	{
	    fclose(mtab);
	    warning ("/etc/mtab not updated");
        }
	else
	{
	    char
		buf[BUFSIZ],
		*ptr;
	    int
		nlen = strlen(name),
		not_dev = strncmp (name, "/dev/", 5);

	    while ( (ptr = fgets (buf, sizeof (buf), mtab)) )
	    {
		if (not_dev)
		    if ((ptr = strchr(ptr, ' ')))
			++ptr;
		    else
			ptr = buf;

		if (! strncmp(name, ptr, nlen) && ptr[nlen] == ' ')
		    found = 1;
		else
		    fputs(buf, tmp);
	    }

	    fclose(tmp);
	    fclose(mtab);

	    if (found)
	    {                           /* I'm taking a chance here -cdr */
		unlink("/etc/mtab");
		link("/etc/mtab-tmp", "/etc/mtab");
	    }

	    unlink("/etc/mtab-tmp");
	}
    }
}

int doumount (Stringstack s)
{
    if (s.nstr != 2 || getopt (s.nstr, s.str, "h") != -1)
	error ("Bad commandline.\n"
	       "Usage: %s device-or-directory\n"
	       , progname);

    if (umount (s.str [1]))
	error ("problem unmounting \"%s\"", s.str [1]);

    mtabUpdate (s.str [1]);

    return (0);
}
