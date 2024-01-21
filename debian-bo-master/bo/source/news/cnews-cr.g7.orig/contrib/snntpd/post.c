#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "libcnews.h"
#include "config.h"
#include "fgetfln.h"
#include "history.h"

#include "batch.h"
#include "msgs.h"
#include "debug.h"
#include "netdata.h"
#include "log.h"

#define NULLSTR		((char *) NULL)

/* NOTE: x *MUST* be an array or you'll only get 3 chars */
#define SAY(x, y) \
	if (net_ackwrite((x), sizeof (x) - 1, (y), stdout) == 0) \
		; \
	else \
		unlock(), error("net_ackwrite(%s) failed", (x))

/* imports */
extern char *mktemp();
extern int postarts, postfail;
extern unsigned int debug;
extern FILE *dfp;

void
postarticle(fp, ndp)
FILE *fp;
struct netdata *ndp;
{
	FILE *inewsfp;
	int ret;
	static char *inewscmd, *inewserrfile;

	SAY(NNTP_POST, NULLSTR);
	if (net_getdata(fp, ndp) != 0) {
		SAY(NNTP_POSTFAIL, NULLSTR);
		postfail++;
		return;
	}
	if (inewscmd == NULL) {
		char *tmp1 = str3save("HOME=", fullartfile(NULLSTR),
				      "/in.coming/bad PATH=");
		char *tmp2 = str3save(newspath(), " ", INEWS);
		char *tmp3 = str3save(tmp1, tmp2, " > ");

		inewserrfile = strsave("in.coming/post.XXXXXX");
		(void) mktemp(inewserrfile);
		if (*inewserrfile == '\0') {
			SAY(NNTP_POSTFAIL, NULLSTR);
			postfail++;
			return;
		}
		inewscmd = str3save(tmp3, inewserrfile, " 2>&1");
		free(tmp1);
		free(tmp2);
		free(tmp3);
	}
	ddprintf(dfp, "popening `%s'\n", inewscmd);
	inewsfp = popen(inewscmd, "w");
	if (inewsfp == NULL) {
		SAY(NNTP_POSTFAIL, NULLSTR);
		postfail++;
		return;
	}
	if (batchwrite(inewsfp, ndp->nd_buf, ndp->nd_bufcnt, ndp->nd_fp,
	    ndp->nd_spilled) != 0) {
		SAY(NNTP_POSTFAIL, NULLSTR);
		postfail++;
		/* could we have a partial article here? */
		(void) pclose(inewsfp);
		return;
	}
	ret = pclose(inewsfp);
	if (ret == 0) {
		SAY(NNTP_POSTOK, NULLSTR);
		postarts++;
	} else {
		struct stat stbuf;
		
		ddprintf(dfp, "inews returned %x\n", ret);
		if (stat(inewserrfile, &stbuf) < 0)
			warning("stat(%s) failed", inewserrfile);
		else if (stbuf.st_size != 0)  {
			char *cp;
			sizeint len;
			FILE *fp = efopen(inewserrfile, "r");

			SAY(NNTP_POSTFAILC, NULLSTR);
			while ((cp = fgetline(fp, &len)) != NULL) {
				if (cp[--len] == '\n')
					cp[len] = '\0';
				else
					len++;	/* shouldn't happen */
				net_ackwrite(cp, len, NULLSTR, stdout);
			}
			(void) fclose(fp);
		}
		SAY(NNTP_POSTFAIL, itos("inews returned %d", ret));
		postfail++;
	}
	if (unlink(inewserrfile))
		warning("unlink(%s) failed", inewserrfile);
}
