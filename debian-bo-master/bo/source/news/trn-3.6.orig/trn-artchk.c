/* $Id: trn-artchk.c,v 3.5 1993/04/18 03:09:32 davison Trn $
*/
/* The authors make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

/* A program to check an article's validity and print warnings if problems
** are found.
**
** Usage: trn-artchk <article> <maxLineLen> <newsgroupsFile> <activeFile>
*/

#include "EXTERN.h"
#include "common.h"
#ifdef USE_NNTP
#include "nntpclient.h"
#endif

#define MAXNGS 100

char *safemalloc _((MEM_SIZE));

#ifdef USE_NNTP
int server_connection _((void));
void finalize _((int));
char nntp_handle_timeout _((bool_int));

int debug = 0;

#ifdef USE_GENAUTH
char *loginName;
#endif

#endif /* USE_NNTP */

int
main(argc, argv)
int argc;
char *argv[];
{
    FILE *fp, *fp_active = NULL, *fp_ng = NULL;
    bool check_active = FALSE, check_ng = FALSE;
    char buff[LBUFLEN], *cp, *cp2;
    char *ngptrs[MAXNGS];
    int nglens[MAXNGS];
    int foundactive[MAXNGS];
    int i, col, max_col_len, line_num = 0, ngcnt = 0, ngleft;
    int found_newsgroups = 0;

    if (argc != 5 || !(max_col_len = atoi(argv[2]))) {
	fprintf(stderr, "\
Usage: trn-artchk <article> <maxLineLen> <newsgroupsFile> <activeFile>\n");
	exit(1);
    }

    if ((fp = fopen(argv[1], "r")) == NULL) {
	fprintf(stderr, "trn-artchk: unable to open article `%s'.\n", argv[1]);
	exit(1);
    }

    /* Check the header for proper format and report on the newsgroups */
    while (fgets(buff, LBUFLEN, fp)) {
	line_num++;
	buff[strlen(buff)-1] = '\0';
	if (!*buff)
	    break;
	if (*buff == ' ' || *buff == '\t')
	    continue;
	if (!(cp = index(buff, ':'))) {
	    printf("\nERROR: line %d is an invalid header line:\n%s\n",
		   line_num, buff);
	    break;
	}
	if (cp[1] != ' ' && cp[1] != '\0') {
	    printf("\n\
ERROR: header on line %d does not have a space after the colon:\n%s\n",
		   line_num, buff);
	}
	if (cp - buff == 10 && strnEQ(buff, "Newsgroups", 10)) {
	    found_newsgroups = 1;
	    for (cp = buff + 11; *cp == ' '; cp++)
		;
	    if (index(cp, ' ')) {
		printf("\n\
ERROR: the \"Newsgroups:\" line has spaces in it that MUST be removed. The\n\
only allowable space is the one separating the colon (:) from the contents.\n\
Use a comma (,) to separate multiple newsgroup names.\n");
		continue;
	    }
	    while (*cp) {
		if (!(cp2 = index(cp, ',')))
		    cp2 = cp + strlen(cp);
		else
		    *cp2++ = '\0';
		if (ngcnt < MAXNGS) {
		    nglens[ngcnt] = strlen(cp);
		    foundactive[ngcnt] = 0;
		    ngptrs[ngcnt] = safemalloc(nglens[ngcnt]+1);
		    strcpy(ngptrs[ngcnt], cp);
		    ngcnt++;
		}
		cp = cp2;
	    }
	    if (!ngcnt) {
		printf("\n\
ERROR: the \"Newsgroups:\" line lists no newsgroups.\n");
		continue;
	    }
	}
    }
    if (!found_newsgroups) {
	printf("\nERROR: the \"Newsgroups:\" line is missing from the header.\n");
    }

    /* Check the body of the article for long lines */
    while (fgets(buff, LBUFLEN, fp)) {
	line_num++;
	col = strlen(buff)-1;
	if (buff[col] != '\n')
	    printf("\n\
Warning: line %d has no trailing newline character and may get lost.\n",
		   line_num);
	else
	    buff[col] = '\0';
	col = 0;
	for (cp = buff; *cp; cp++) {
	    if (*cp == '\t')
		col += 8 - (col%8);
	    else
		col++;
	}
	if (col > max_col_len) {
	    printf("\n\
Warning: posting exceeds %d columns.  Line %d is the first long one:\n%s\n",
		   max_col_len, line_num, buff);
	    break;
	}
    }
    if (ngcnt) {
	struct stat st;
	if (stat(argv[3], &st) != -1)
	    check_ng = st.st_size > 0 && (fp_ng = fopen(argv[3], "r")) != NULL;
#ifdef USE_NNTP
	else if (server_connection())
	    check_ng = TRUE;
#endif
	if (stat(argv[4], &st) != -1)
	    check_active = st.st_size > 0 && (fp_active = fopen(argv[4], "r")) != NULL;
#ifdef USE_NNTP
	else if (server_connection())
	    check_active = TRUE;
#endif
    }
    if (ngcnt && (check_ng || check_active)) {
	/* Print a note about each newsgroup */
	printf("\nYour article's newsgroup%s:\n", ngcnt == 1? "" : "s");
	if (!check_active) {
	    for (i = 0; i < ngcnt; i++) {
		foundactive[i] = 1;
	    }
	}
	else if (fp_active != NULL) {
	    ngleft = ngcnt;
	    while (fgets(buff, LBUFLEN, fp_active)) {
		if (!ngleft)
		    break;
		for (i = 0; i < ngcnt; i++) {
		    if (!foundactive[i]) {
			if ((buff[nglens[i]] == '\t' || buff[nglens[i]] == ' ')
			  && strnEQ(ngptrs[i], buff, nglens[i])) {
			    foundactive[i] = 1;
			    ngleft--;
			}
		    }
		}
	    }
	    fclose(fp_active);
	}
#ifdef USE_NNTP
	else {
	    int listactive_works = 1;
	    for (i = 0; i < ngcnt; i++) {
		if (listactive_works) {
		    sprintf(ser_line, "list active %s", ngptrs[i]);
		    nntp_command(ser_line);
		    if (nntp_check(FALSE) == NNTP_CLASS_OK) {
			while (nntp_gets(ser_line, sizeof ser_line) >= 0) {
			    if (NNTP_LIST_END(ser_line))
				break;
			    foundactive[i] = 1;
			}
		    }
		    else if (*ser_line == NNTP_CLASS_FATAL) {
			listactive_works = FALSE;
			i--;
		    }
		}
		else {
		    sprintf(ser_line, "GROUP %s", ngptrs[i]);
		    nntp_command(ser_line);
		    if (nntp_check(FALSE) == NNTP_CLASS_OK)
			foundactive[i] = 1;
		}
	    }
	}
	if (check_ng && fp_ng == NULL) {
	    fp_ng = fopen(argv[3], "w+");
	    unlink(argv[3]);
	    if (fp_ng != NULL) {
		for (i = 0; i < ngcnt; i++) {
		    /* issue a description list command */
		    sprintf(ser_line, "XGTITLE %s", ngptrs[i]);
		    nntp_command(ser_line);
		    /*$$ use list newsgroups if this fails...? */
		    if (nntp_check(FALSE) == NNTP_CLASS_OK) {
			/* write results to fp_ng */
			while (nntp_gets(ser_line, sizeof ser_line) >= 0) {
			    if (NNTP_LIST_END(ser_line))
				break;
			    fprintf(fp_ng, "%s\n", ser_line);
			}
		    }
		}
		fseek(fp_ng, 0L, 0);
	    }
	}
#endif
	if (fp_ng != NULL) {
	    ngleft = ngcnt;
	    while (fgets(buff, LBUFLEN, fp_ng)) {
		if (!ngleft)
		    break;
		for (i = 0; i < ngcnt; i++) {
		    if (foundactive[i] && ngptrs[i]) {
			if ((buff[nglens[i]] == '\t' || buff[nglens[i]] == ' ')
			  && strnEQ(ngptrs[i], buff, nglens[i])) {
			    cp = &buff[nglens[i]];
			    *cp++ = '\0';
			    while (*cp == ' ' || *cp == '\t')
				cp++;
			    if (cp[0] == '?' && cp[1] == '?')
				cp = "[no description available]\n";
			    printf("%-23s %s", buff, cp);
			    free(ngptrs[i]);
			    ngptrs[i] = 0;
			    ngleft--;
			}
		    }
		}
	    }
	    fclose(fp_ng);
	}
	for (i = 0; i < ngcnt; i++) {
	    if (!foundactive[i]) {
		printf("%-23s ** invalid news group -- check spelling **\n",
		   ngptrs[i]);
		free(ngptrs[i]);
	    }
	    else if (ngptrs[i]) {
		printf("%-23s [no description available]\n", ngptrs[i]);
		free(ngptrs[i]);
	    }
	}
    }
    return 0;
}

#ifdef USE_NNTP
int
server_connection()
{
    static int server_stat = 0;
    if (!server_stat) {
	if (nntp_connect(0))
	    server_stat = 1;
	else
	    server_stat = -1;
    }
    return server_stat == 1;
}

char
nntp_handle_timeout(strict)
bool_int strict;
{
    nntp_error("\n503 Server timed out.\n");
    if (strict)
	finalize(1);
    return NNTP_CLASS_FATAL;
}
#endif

void
finalize(num)
int num;
{
#ifdef USE_NNTP
    nntp_close(TRUE);
#endif
    exit(num);
}

static char nomem[] = "trn: out of memory!\n";

/* paranoid version of malloc */

char *
safemalloc(size)
MEM_SIZE size;
{
    char *ptr;

    ptr = malloc(size ? size : (MEM_SIZE)1);
    if (ptr == Nullch) {
	fputs(nomem,stdout) FLUSH;
	finalize(1);
    }
    return ptr;
}

/* paranoid version of realloc.  If where is NULL, call malloc */

char *
saferealloc(where,size)
char *where;
MEM_SIZE size;
{
    char *ptr;

    ptr = realloc(where, size ? size : (MEM_SIZE)1);
    if (!ptr) {
	fputs(nomem,stdout) FLUSH;
	finalize(1);
    }
    return ptr;
}
