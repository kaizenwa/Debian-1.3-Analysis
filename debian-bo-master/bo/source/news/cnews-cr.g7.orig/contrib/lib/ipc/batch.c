/* routines to manage batch file that snntpd saves accepted articles in. */
#include <stdio.h>
#include <assert.h>
#include <errno.h>
#include <sys/types.h>	/* for time_t */

#include "libcnews.h"
#include "debug.h"
#include "history.h"
#include "batch.h"
#include "log.h"

/* imports */
extern int errno;
extern time_t time(/* time_t *tloc */);
extern char *ctime(/* time_t *tloc */);
extern char *mktemp();
extern long spacefor();

/*
 * !! allow this to be specified on the command line, or per host?  We
 * may want POST to be higher grade, for instance?
 */
static char grade[3] = "";	/* 3 = digit, period, NUL */

/*
 * For most nntpds, maxtime is the important one -- if the other end
 * isn't feeding us quickly, we may as well hand relaynews what we have
 * so far, so that it propagates to our downstream quickly.  maxarts and
 * maxbytes help keep the batch sizes under control.  Don't change these
 * defaults unless you update the manual page.
 */
sizeint bt_maxtime = 5*60;		/* 5 mins */
sizeint bt_maxarts = 500;
sizeint bt_maxbytes = 1024*1024;	/* 1 Mb */

sizeint bt_rej, bt_fail;

static sizeint bt_arts, bt_bytes;
static time_t bt_start_time;
static char *bt_name;
static FILE *bt_fp;

/* 
 * calls batchrename if batch conditions are exceeded, these are set by
 * batchstart?.  Generalize to handle multiple batches.  Returns 0 for
 * success, -1 for failure.
 */
int
batchadd(buf, buflen, artsize, tmpfp, tmpbytes)
char *buf;
sizeint buflen, artsize, tmpbytes;
FILE *tmpfp;
{
	if (bt_fp == NULL)
		batchinit((FILE *) NULL);
	fprintf(bt_fp, "#! rnews %d\n", artsize);
	if (batchwrite(bt_fp, buf, buflen, tmpfp, tmpbytes) != 0) {
		warning("error writing batchfile `%s', ending it\n", bt_name);
		batchend();
		return -1;
	}
	bt_arts++;
	bt_bytes += artsize;
	if (bt_arts >= bt_maxarts || bt_bytes >= bt_maxbytes)
		batchend();
	return 0;
}

/*
 * flushes/closes the current batch.  Starts a new one if we're using
 * temporary file batches.
 */
void
batchend()
{
	int age;
	
	if (bt_name == NULL || bt_fp == NULL)
		return;
	histclose();
	if (*bt_name == '\0') {
		ddprintf(dfp, "flushed batch fp\n");
		if (fflush(bt_fp) != 0)
			error("error flushing unnamed batch file", "");
		return;
	}
	if (fclose(bt_fp) != 0)
		error("error closing batchfile %s", bt_name);
	bt_fp = NULL;
	if (bt_arts == 0) {
		ddprintf(dfp, "ended empty batch\n");
		(void) unlink(bt_name);
		return;
	}
	age = time((time_t *)NULL) - bt_start_time;
	ddprintf(dfp, "ending batch of %d arts, %d bytes, %d seconds old\n",
		 bt_arts, bt_bytes, age);
	log3int("ihave_stats accepted %d rejected %d failed %d",
		bt_arts, bt_rej, bt_fail);
	logtimes("times");
	log3int("batch size %d bytes, %d seconds old", bt_bytes, age, 0);
	bt_arts = bt_bytes = 0;
	bt_start_time = 0;
	if (mkinperm(bt_name, grade, ".t")) {
		ddprintf(dfp, "spooled %s\n", bt_name);
		(void) free(bt_name);
		bt_name = NULL;
	} else {
		bt_name = NULL;	/* avoid infinite mutual recursion. */
		error("mkinperm(%s, \"\", \".t\") failed", bt_name);
	}
	while (spacefor(1L, "/usr/spool/news/in.coming", 5000L, 1000L, 250000L)
	    <= 0)
		sleep(30);		/* wait for space to appear */
}

/*
 * If fp is passed in, it is used as a permanant batch file on which the
 * batch is written.  It will be periodically flushed by batchend(), but
 * never closed.  Otherwise, a temporary batch is created, and it is
 * periodically closed and renamed so that newsrun will process it.
 */
void
batchinit(fp)
FILE *fp;
{
	static char *bt_template = "in.coming/nntp.XXXXXX";

	assert(bt_arts == 0 && bt_bytes == 0 && bt_start_time == 0);
	assert(bt_name == NULL && bt_fp == NULL);
	if (fp != NULL) {
		bt_fp = fp;
		bt_name = strsave("");
	} else {
		bt_name = strsave(bt_template);
		(void) mktemp(bt_name);
		bt_fp = efopen(bt_name, "w");
	}
	bt_start_time = time((time_t *) 0);
	ddprintf(dfp, "started batch `%s', %s\n", bt_name,
		 ctime(&bt_start_time) + 4);
}

int
batchwrite(fp, buf, buflen, tmpfp, tmpbytes)
char *buf;
FILE *tmpfp, *fp;
sizeint buflen, tmpbytes;
{
	register int cc = 1;
	char tmpbuf[8192];	/* good for even filesystems with big blocks */

	if (tmpbytes != 0) {
		/* usually false, we hope */
		sizeint n, cnt;

		rewind(tmpfp);
		for (cc = cnt = 0, n = sizeof tmpbuf; cnt < tmpbytes;cnt+=cc) {
			if (tmpbytes - cnt < n)
				n = tmpbytes - cnt;
			cc = fread(tmpbuf, 1, n, tmpfp);
			if (cc <= 0)
				break;
			ddprintf(stderr, "copying %d from tmp to batch\n", cc);
			(void) fwrite(tmpbuf, 1, cc, fp);
		}
	}
	if (cc <= 0 || ferror(fp) ||
	    (buflen > 0 && fwrite(buf, 1, buflen, fp) != buflen) ||
	    fflush(fp) == EOF) {
		return EOF;
	}
	return 0;
}
