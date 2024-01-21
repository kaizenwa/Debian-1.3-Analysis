/*
 * article creation and destruction
 */
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include "fixerrno.h"

#include <sys/types.h>
#include "libc.h"
#include "news.h"
#include "headers.h"
#include "relay.h"

void
artinit(art)
register struct article *art;
{
	static long uniqno = 0;
	static struct article zart;

	*art = zart;
	hdrinit(&art->h);
	art->a_status = ST_OKAY;
	art->a_id = uniqno++;
}

void
artfree(art)
register struct article *art;
{
	freeheaders(&art->h);
	/* a_haccum is currently not malloced */
	art->a_hptrs = NULL;		/* don't free a_hptrs; see hdrsave() */
	nnfree(&art->a_files);
	if (art->a_artf != NULL) {
		errno = 0;
		canthappen(art, 'i', "a_artf still open in artfree()", "");
	}
	nnfclose(art, &art->a_artf, art->a_tmpf);
	nnfree(&art->a_tmpf);
}
