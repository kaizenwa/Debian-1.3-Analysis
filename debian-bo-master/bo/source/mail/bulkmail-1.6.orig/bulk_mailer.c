/*
 * bulk mailer.
 * sorts a recipient list by reversed domain and splits it up into
 * several envelopes so sendmail can deliver it in parallel.
 *
 * written March 1994
 * by Keith Moore <moore@cs.utk.edu>
 *
 * You can use this if you want to, but there's no warranty.
 * So don't blame me if something goes wrong.
 */

/*
 * NOTES:
 * - Recipients go one-recipient-per-line in the recipient list file.
 *   bulk_mailer knows how to ignore comments and prefix phrases.
 *
 * - Mail to programs doesn't work.  This is a feature.
 *
 * - If message does not contain a To, Cc, or Bcc header, bulk_mailer
 *   adds one.  Otherwise sendmail would add an Apparently-To: header
 *   containing each recipient's address.
 * - adds a Received header,
 * - unconditionally nukes content-length, errors-to, return-receipt-to
 * - nukes return-path header (shouldn't be there anyway)
 * - adds message-id if there wasn't one there already
 * - nukes Precedence (but sysadmin can add it back)
 * - sysadmin can optionally add a reply-to
 */

/*
 * TODO (maybe):
 *
 * - support passing recipients to sendmail on the command-line
 *   (rather than via SMTP; sendmail may refuse SMTP connections
 *   if the load average is too high).
 * - support writing queue files directly into sendmail's mailq
 *   directory for efficiency.
 * - sort addresses by primary MX rather than by domain.
 * - allow splitting up the load into several different mail queues;
 *   arrange things so that mail for domain 'x.y' always goes in
 *   the same queue.  That way, the site can control the number
 *   of sendmail "worker bees".
 * - allow options to be specified in a configuration file, rather
 *   than having to put them all on the command-line.
 * - require approval for any messages not from a particular set of
 *   users
 * - accept majordomo-style 'approved: password' header.
 * - add some checks to prevent nondelivery reports from going to
 *   the list.
 */

/*
 * change log:
 *
 * September 28, 1995 by Keith Moore
 * - allow setting of max_domains from the command-line
 * - allow setting default domain from the command-line
 * - prefrobnicate addresses to remove comments, white space,
 *   'phrase' before address in angle-brackets.
 * - add support for various kinds of header munging
 * 
 * April 14, 1995 by Keith Moore, moore@cs.utk.edu
 * - if input doesn't contain a To/Cc/Bcc header, add a dummy To header.
 * - new functions malloc_or_else and realloc_or_else simplify flow.
 *
 * May 27, 1994 by Paul DuBois, dubois@primate.wisc.edu
 * - Don't uppercase anything in the original, just the reversed domain
 *   that's used for sorting.  This leaves the original addresses alone.
 * - Fixed bug in argument processing.  (Specifying -v resulted in unknown
 *   flag message due to missing "else")
 * - If DEFAULT_DOMAIN is defined, it's added to local address so they
 *   have a @thing part.  Otherwise addresses with no @thing part are rejected
 *   as originally.
 * - Check all malloc() return values for NULL.
 * - Add exit(EX_OK) at end of main() so explicit status gets returned.
 * - Added "void" to definitions of a few functions that were missing it.
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sysexits.h>
#include <errno.h>
#include <time.h>
#include "patchlevel.h"

#ifndef PIPECOMMAND
#define PIPECOMMAND "/usr/lib/sendmail -bs %s"
#endif

#ifndef HAS_STRERROR
extern int sys_nerr;
extern char *sys_errlist[];
#define strerror(e) ((e) < 0 || (e) >= sys_nerr ? "(unknown)" : sys_errlist[e])
#endif


#ifndef errno
extern int errno;
#endif

#ifndef HAS_STRDUP
char *strdup();
#endif
char *strchr();
char *strrchr();
char *malloc ();
char *realloc ();
char *mktemp ();

/*
 * max different domains per envelope.
 * if the worst-case delay is 30 minutes per delivery attempt,
 * then the worst-case time to try to deliver to everyone will
 * be 30 minutes * MAX_DOMAINS.  Usually it will be much better.
 */
#define MAX_DOMAINS 20

int debug_flag = 0;		/* don't mail; spit SMTP to stdout */
char *envelope_from;		/* outgoing SMTP from address */
int force_reply_to = 0;		/* override sender's reply-to hdr */
char *list_owner = NULL;	/* address of owner of mailing list */
char *local_domain = NULL;	/* local domain name */
int max_domains = MAX_DOMAINS;	/* max # of domains / envelope */
int max_size = 0x7fffffff;	/* max message size before bouncing */
char *precedence = NULL;	/* generate precedence hdr with this value */
char *reply_to = NULL;		/* generate reply-to hdr with this value  */
char *sendmail_flags = "";	/* call sendmail with these flags */
int verbose_flag = 0;		/* be verbose */

/*
 * structure used to hold an address while sorting
 */

struct address {
    char *address;		/* entire address */
    char *niamod;		/* reversed domain (for sorting) */
};

static struct address *address_list = NULL;
static int num_addrs = 0;
static int num_addr_slots = 0;

/*
 * discard an address (maybe print an error message)
 */

static void
discard (addr, msg)
char *addr;
char *msg;
{
    if (verbose_flag)
	fprintf (stderr, "%s discarded (%s)\n", addr, msg);
}

/*
 * upper case a string in place
 */
static char *
strupper (s)
char *s;
{
    char *result = s;

    while (*s) {
	if (*s >= 'a' && *s <= 'z')
	    *s += 'A' - 'a';
	++s;
    }
    return result;
}

/*
 * attempt to malloc memory.  if it fails, exit with EX_TEMPFAIL
 *
 * XXX instead of exiting, should clean up.
 */
static char *
malloc_or_else (size)
int size;
{
    char *result;

    if ((result = malloc (size)) == NULL) {
	fprintf (stderr, "out of memory\n");
	exit (EX_TEMPFAIL);
    }
    return result;
}

/*
 * attempt to realloc memory.  if it fails, exit with EX_TEMPFAIL
 *
 * XXX instead of exiting, should clean up.
 */

static char *
realloc_or_else (oldbuf, size)
char *oldbuf;
int size;
{
    char *result;

    if ((result = realloc (oldbuf, size)) == NULL) {
	fprintf (stderr, "out of memory\n");
	exit (EX_TEMPFAIL);
    }
    return result;
}

/*
 * reverse the characters in a domain and put the result in malloc'ed memory
 */

static char *
niamod (domain)
char *domain;
{
    int length;
    char *result;
    int i;

    length = strlen (domain);
    result = malloc_or_else (length + 1);
    for (i = 0; i < length; ++i)
        result[i] = domain[length - 1 - i];
    result[length] = '\0';
    return strupper (result);
}

#ifndef HAS_STRDUP
/*
 * copy a string into malloc'ed memory
 * (most systems have this built-in, but not ultrix 4.3.)
 */

static char *
strdup (str)
char *str;
{
    int length;
    char *result;

    length = strlen (str) + 1;
    result = malloc_or_else (length);
    strcpy (result, str);
    return result;
}
#endif


/*
 * copy the first 'len' characters of a string into malloc'ed
 * memory, appending a NUL to the copy.
 */

static char *
strndup (str, len)
{
    char *result = malloc_or_else (len + 1);

    strncpy (result, str, len);
    result[len] = '\0';
    return result;
}

/*
 * sort by case-folded reversed domain
 */

static int
compare (a1, a2)
struct address *a1, *a2;
{
    return strcmp (a1->niamod, a2->niamod);
}

static void
sort_addrs ()
{
    qsort (address_list, num_addrs, sizeof (struct address), compare);
}

/*
 * save an address in the sort buffer
 *
 * buffer is automagically grown 1000 addresses at a time.
 */

static void
save (addr)
char *addr;
{
    char *at;
    char *ptr;
    char *domain;
    char tempbuf[1024];
    char c;

    /*
     * make sure there's room in the buffer.
     */
    if (num_addrs >= num_addr_slots) {
	struct address *new;
	
	num_addr_slots += 1000;
	if (address_list == NULL)
	    address_list = (struct address *)
		malloc_or_else (num_addr_slots * sizeof (struct address));
	else
	    address_list = (struct address *)
		realloc_or_else (address_list,
				 num_addr_slots * sizeof (struct address));
    }

    address_list[num_addrs].address = strdup (addr);

    /*
     * find the next-hop domain for sorting.  this will always be the
     * domain name following the leftmost '@' sign.  It is terminated
     * by one of { ':' ',' '\0'}.
     */
    at = strchr (addr, '@');
    if (at == NULL) {
	discard (addr, "missing '@'");
	return;
    }
    for (ptr = at; *ptr != '\0' && *ptr != ':' && *ptr != ','; ++ptr);
    *ptr = '\0';
    strcpy (tempbuf, at + 1);
    /*
     * uppercase the domain to make sorting easier
     */
    strupper (tempbuf);
    address_list[num_addrs].niamod = niamod (tempbuf);
    ++num_addrs;
}

/*
 * take an address and remove any extraneous bits. e.g:
 *
 * Keith Moore <moore@cs.utk.edu> => moore@cs.utk.edu
 * moore@cs.utk.edu (Keith Moore) => moore@cs.utk.edu
 */

static char *
prefrobnicate (buf)
char *buf;
{
    static char result[1024];
    char *s, *d;
    int state = 0;
    int level = 0;

    s = buf;
    d = result;
    while (*s) {
	if (d >= result+sizeof(result)-1) {
	    fprintf (stderr, "prefrobnicate: line too long \"%s\"\n", buf);
	    return "";
	}
	switch (state) {
	case 0:			/* outside */
	    if (*s == '<') {
		d = result;
		state = 3;
	    }
	    else if (*s == '(') {
		level++;
		state = 1;
	    }
	    else if (*s == '"') {
		state = 2;
		*d++ = *s;
	    }
	    else if (*s == '\\') {
		if (s[1])
		    *d++ = *s++;
		*d++ = *s;
	    }
	    else if (*s == ' ' || *s == '\t' || *s == '\r' || *s == '\n')
		;
	    else
		*d++ = *s;
	    break;
	case 1:			/* inside comment */
	case 4:			/* comment inside <> */
	    /* don't copy comments to output */
	    if (*s == '(')
		++level;
	    else if (*s == ')') {
		if (--level <= 0)
		    state = (state == 1) ? 0 : 3;
	    }
	    else if (*s == '\\') {
		if (s[1])
		    ++s;
	    }
	    break;
	case 2:			/* inside quotes */
	case 5:			/* quotes inside <> */
	    if (*s == '"') {
		state = (state == 2) ? 0 : 3;
		*d++ = *s;
	    }
	    else if (*s == '\\') {
		if (s[1])
		    *d++ = *s++;
		*d++ = *s;
	    }
	    else
		*d++ = *s;
	    break;
	case 3:			/* inside <>  */
	    if (*s == '>') {
		*d = '\0';
		return result;
	    }
	    else if (*s == '(')
		state = 4;
	    else if (*s == '"') {
		*d++ = *s;
		state = 5;
	    }
	    else if (*s == '\\') {
		if (s[1])
		    *d++ = *s++;
		*d++ = *s;
	    }
	    else if (*s == ' ' || *s == '\t' || *s == '\r' || *s == '\n')
		;
	    else
		*d++ = *s;
	    break;
	}
	++s;
    }
    *d = '\0';
    return result;
}

/*
 * do some rough canonicalization of the address, then save for later
 */

static void
process_addr (buf)
char *buf;
{
    int length = strlen (buf);
    char *at;
    char *ptr;

    if (length == 0)
	return;

    /* delete newline at end of buf */
    if (buf[length-1] == '\n')
	buf[--length] = '\0';

    /*
     * strip out phrases, white space, comments
     */
    if ((ptr = prefrobnicate (buf)) == NULL)
	return;
    strcpy (buf, ptr);

    /*
     * find last '@' in the address.  if there isn't one, it's
     * bogus.
     */
    if ((at = strrchr (buf, '@')) == NULL) {
	if (local_domain) {
	    at = buf + strlen (buf);
	    *at++ = '@';
	    (void) strcpy (at--, local_domain);
	}
	else {
	    discard (buf, "no '@' in address");
	    return;
	}
    }
    if (at == buf) {
	discard (buf, "no local-part");
	return;
    }

    save (buf);
}

static void
process (fp)
FILE *fp;
{
    char linebuf[1024];

    while (fgets (linebuf, sizeof (linebuf), fp) != NULL)
	process_addr (linebuf);
}

/*
 * open a new envelope.
 *
 * this could be done in lots of ways.  SMTP has the advantage
 * that it doesn't have any hard limit on the number of recipients,
 * unlike a command line.  (note that the code to limit the number
 * of domains per envelope does nothing to limit the number of
 * recipients)
 *
 * on the other hand, this implementation assumes that the SMTP server
 * doesn't limit the number of recipients, which is not true for all
 * SMTP implementations.  and there are bugs in some sendmail versions
 * that crop up when you feed them SMTP from a pipe.
 */

static FILE *
open_envelope ()
{
    FILE *fp;
    char command_buf[32*1024];

    if (debug_flag)
	fp = stderr;
    else {
	sprintf (command_buf, PIPECOMMAND, sendmail_flags);

	if ((fp = popen (command_buf, "w")) == NULL) {
	    fprintf (stderr, "can't open pipe to sendmail: %s\n",
		     strerror (errno));
	    exit (EX_OSERR);
	}
    }
    fprintf (fp, "HELO localhost\n");
    /*
     * Some versions of sendmail fork when they see MAIL FROM.
     * Sendmail also uses stdio to read the SMTP socket.
     * If there are already commands in the stdio buffer following
     * the MAIL FROM, both the parent and child sendmail will see them,
     * and the parent complains about bogus SMTP commands.
     * The ONEX command inhibits forking, preventing this.
     */
    fprintf (fp, "ONEX\n");	/* grrr */
    fprintf (fp, "MAIL FROM:<%s>\n", envelope_from);
    return fp;
}

static void
close_envelope (fp)
FILE *fp;
{
    fprintf (fp, "QUIT\n");
    if (fp != stderr)
	pclose (fp);
}

static void
add_recipient (fp, rcpt)
FILE *fp;
char *rcpt;
{
    fprintf (fp, "RCPT TO:<%s>\n", rcpt);
}

/*
 * copy a message to an SMTP stream, taking care to handle dot quoting
 * also add the initial DATA and the terminating '.'
 */

static void
enclose_msg (fp, file)
FILE *fp;
char *file;
{
    int c;
    int bol = 1;
    FILE *infp;

    if ((infp = fopen (file, "r")) == NULL) {
	fprintf (stderr, "can't open %s (%s)\n", file, strerror (errno));
	exit (EX_NOINPUT);
    }
    fprintf (fp, "DATA\n");
    while ((c = getc (infp)) != EOF) {
	if (bol && c == '.')
	    putc ('.', fp);
	putc (c, fp);
	bol = (c == '\n');
    }
    if (!bol)
	putc ('\n', fp);
    fprintf (fp, ".\n");
    fclose (infp);
}


/*
 * forward a message .
 */

static void
forward_msg (fp, file, to, subject)
FILE *fp;
char *file;
char *to, *subject;
{
    int c;
    int bol = 1;
    FILE *infp;

    if ((infp = fopen (file, "r")) == NULL) {
	fprintf (stderr, "can't open %s (%s)\n", file, strerror (errno));
	exit (EX_NOINPUT);
    }
    fprintf (fp, "DATA\n");
    fprintf (fp, "To: %s\n", to);
    fprintf (fp, "Subject: %s\n", subject);
    fprintf (fp, "Content-type: message/rfc822\n");
    fprintf (fp, "MIME-Version: 1.0\n");
    fprintf (fp, "\n");

    while ((c = getc (infp)) != EOF) {
	if (bol && c == '.')
	    putc ('.', fp);
	putc (c, fp);
	bol = (c == '\n');
    }
    if (!bol)
	putc ('\n', fp);
    fprintf (fp, ".\n");
    fclose (infp);
}

/*
 * split a message up across several envelopes
 */

static void
dump_addrs (msgfile)
char *msgfile;
{
    int i;
    int domains_this_envelope = 0;
    FILE *fp;

    if (num_addrs <= 0)
	return;

    fp = open_envelope ();

    for (i = 0; i < num_addrs; ++i) {
	/*
	 * see if this a new domain before adding recipient.
	 * if it's not a new domain, go ahead and add her.
	 * otherwise, make sure we have room first.
	 *
	 * BITNET is special, since we want to send all BITNET
	 * mail to the same gateway even if the node is different.
	 */
	if (domains_this_envelope == 0) {
	    add_recipient (fp, address_list[i].address);
	    domains_this_envelope = 1;
	}
	else if (strncmp (address_list[i].niamod, "TENTIB.", 7) == 0 &&
	    strncmp (address_list[i-1].niamod, "TENTIB.", 7) == 0) {
	    add_recipient (fp, address_list[i].address);
	}
	else if (strcmp (address_list[i].niamod,
			 address_list[i-1].niamod) == 0) {
	    add_recipient (fp, address_list[i].address);
	}
	else {
	    /*
	     * new domain, so it might overflow the envelope
	     * if this envelope is full, seal it and open another one
	     */
	    if (domains_this_envelope >= max_domains) {
		enclose_msg (fp, msgfile);
		close_envelope (fp);
		fp = open_envelope ();
		domains_this_envelope = 0;
	    }
	    add_recipient (fp, address_list[i].address);
	    ++domains_this_envelope;
	}
    }
    enclose_msg (fp, msgfile);
    close_envelope(fp);
}

/*
 * return 1 iff 's' points to a To, Cc, or Bcc header
 */
static int
istohdr (s)
char *s;
{
    if (strncasecmp (s, "to", 2) == 0)
	s += 2;
    else if (strncasecmp (s, "cc", 2) == 0)
	s += 2;
    else if (strncasecmp (s, "bcc", 3) == 0)
	s += 3;
    while (*s == ' ' || *s == '\t' || *s == '\r' || *s == '\n')
	++s;
    return (*s == ':');
}

static int
contains (buf, str)
char *buf, *str;
{
    register char c1, c2;
    register char *ptr;
    int length = strlen (str);

    c1 = *str;
    c2 = isupper (c1) ? tolower (c1) : toupper (c1);
    
    for (ptr = buf; *ptr; ++ptr) {
	if (*ptr == c1 || *ptr == c2) {
	    if ((ptr == buf || ptr[-1] == ' ' || ptr[-1] == '\t') &&
		strncasecmp (buf, str, length) == 0 &&
		(ptr[length] == ' ' || ptr[length] == '\t' ||
		 ptr[length] == '\n'))
		return 1;
	}
    }
    return 0;
}

static int
match_command (buf, str)
char *buf, *str;
{
    int len = strlen (str);

    while (*buf == ' ' || *buf == '\t')
	++buf;
    return (strncasecmp (buf, str, len) == 0 &&
	    (buf[len] == ' ' || buf[len] == '\t' || buf[len] == '\n'));
}

/*
 * return 1 iff 's' is a header field with name 'name'
 */
static int
match_hdr (name, s)
char *name, *s;
{
    int nl = strlen (name);

    if (strncasecmp (s, name, nl) == 0) {
	s += nl;
	while (*s == ' ' || *s == '\t' || *s == '\r' || *s == '\n')
	    ++s;
	return (*s == ':');
    }
    return 0;
}


static int
match_prefix (buf, str)
char *str, *buf;
{
    return (strncasecmp (str, buf, strlen (str)) == 0);
}


/*
 * return 1 if 's' points to a blank line
 */

static int
isblankline (s)
char *s;
{
    while (*s == ' ' || *s == '\t')
	++s;
    return (*s == '\r' || *s == '\n' || *s == '\0');
}

/*
 * read in a header field, including continuation lines
 */

char *
get_hdr_field (buf, size, fp)
char *buf;
int size;
FILE *fp;
{
    char *d = buf;
    int c;
    int bol = 1;

    while ((c = getc (fp)) != EOF) {
	/*
	 * if we're at the beginning of a line, and the buffer
	 * is empty (we're not at the start of a header),
	 * and the first character of the line is NOT a SPACE
	 * or HTAB, this is a new field.  So put the character
	 * back on the input stream and return.
	 */
	if (bol && (d - buf) > 0 && c != ' ' && c != '\t') {
	    ungetc (c, fp);
	    *d = '\0';
	    return buf;
	}
	/*
	 * if the buffer is empty and we immediately see a
	 * newline, we've hit the blank line at the end of
	 * the header.
	 */
	if ((d - buf) == 0 && c == '\n')
	    return NULL;

	/* stuff the character in the buffer */
	*d++ = c;
	bol = (c == '\n');

	/* size check */
	if (d - buf >= size - 1) {
	    buf[size-1] = '\0';
	    return buf;
	}
    }
    /* at EOF, either return what's already in the buffer, or NULL */
    if (d == buf)
	return NULL;
    else {
	*d = 0;
	return buf;
    }
}

static char *
arpadate (t)
time_t *t;
{
    struct tm gmt;
    struct tm *lt;
    static char datebuf[100];
    int gmtoff;
    char sign;
    static char *months[] = {
	"Jan", "Feb", "Mar", "Apr", "May", "Jun",
	"Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
    };
    static char *wdays[] = {
	"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat",
    };
    
    /*
     * "I'm ashamed of this."  - SEK
     *
     * there's no portable function to get the offset between local time
     * and gmt.  so we call both localtime() and gmtime() with the same
     * time clock and calculate the difference.   also, gmtime() and
     * localtime() share the same static return structure, so we have to
     * copy the result of one before we call the other.  yeech.
     */
    gmt = *gmtime (t);
    lt = localtime (t);
    gmtoff = (lt->tm_hour - gmt.tm_hour) * 60 + lt->tm_min - gmt.tm_min;
    if (lt->tm_year != gmt.tm_year)
	gmtoff += (lt->tm_year - gmt.tm_year) * 24 * 60;
    else
	gmtoff += (lt->tm_yday - gmt.tm_yday) * 24 * 60;

    sign = '+';
    if (gmtoff < 0) {
	sign = '-';
	gmtoff = -gmtoff;
    }
    sprintf (datebuf, "%s, %d %s %04d %02d:%02d:%02d %c%02d%02d",
	     wdays[lt->tm_wday], lt->tm_mday, months[lt->tm_mon], lt->tm_year + 1900,
	     lt->tm_hour, lt->tm_min, lt->tm_sec,
	     sign,
	     gmtoff / 60,
	     gmtoff % 60);

    return datebuf;
}

/*
 * copy a mesasge from 'in' to 'out', doing minimal header munging 
 * on-the-fly.  Return true if it appears that the message has
 * a subscribe command.
 */

#define MESSAGE_OKAY		0
#define HAS_EMBEDDED_COMMAND	1
#define LOOKS_LIKE_BOUNCE	2

static int
copy_message (out, in)
FILE *out, *in;
{
    int c;
    char linebuf[32*1024];
    int has_valid_approved_hdr = 0;
    int has_resent_to_hdr = 0;
    int has_to_hdr = 0;
    int has_reply_to_hdr = 0;
    int has_message_id_hdr = 0;
    int bol = 1;
    time_t t = time ((time_t *) NULL);
    int lines = 0;
    int saw_command = 0;
    int looks_like_bounce = 0;

    /*
     * add any prefix headers we want.
     */
    fprintf (out, "Received: by %s (bulk_mailer v1.%d); %s\n",
	     local_domain, PATCHLEVEL, arpadate (&t));

    /*
     * copy header, checking for various fields
     *
     * - content-length, errors-to, and return-receipt-to are bogus
     *   and nonstandard fields, and are deleted with extreme prejudice.
     *
     * - return-path shouldn't be in the input (it should only appear
     *   at final delivery), so we nuke it.
     *
     * - If a message-id field does not appear in the input, we
     *   add one.  This way all of the redistributed messages
     *   have the same message-id.
     *
     * - the nonstandard precedence header in the input gets unconditionally
     *   deleted, but the sysadmin can add one.
     *
     * - the sysadmin can add a reply-to if he wants to.  depending on
     *   which option was used ("-reply-to" or "+reply-to"), this might
     *   or might not cause an existing reply-to header to be overridden.
     */
    while (get_hdr_field (linebuf, sizeof(linebuf), in) != NULL) {
	if (bol) {
	    if (strncmp (linebuf, "From ", 5) == 0)
		goto delete_header;
	    else if (istohdr (linebuf))
		has_to_hdr = 1;
	    else if (match_hdr ("approved", linebuf)) {
		/* XXX need to actually check password */
		has_valid_approved_hdr = 1;
		goto delete_header;
	    }
	    else if (match_hdr ("content-type", linebuf)) {
		char *colon = strchr (linebuf, ':');

		while (*colon == ' ' || *colon == '\t')
		    ++colon;
		if (strncmp (colon, "multipart/report", 16) == 0)
		    looks_like_bounce = 1;
	    }
	    else if (match_hdr ("content-length", linebuf))
		goto delete_header;
	    else if (match_hdr ("errors-to", linebuf))
		goto delete_header;
	    else if (match_hdr ("message-id", linebuf))
		has_message_id_hdr = 1;
	    else if (match_hdr ("precedence", linebuf))
		goto delete_header;
	    else if (match_hdr ("reply-to", linebuf)) {
		if (force_reply_to)
		    goto delete_header;	/* override existing reply-to */
		has_reply_to_hdr = 1;
	    }
	    else if (match_hdr ("resent-to", linebuf))
		has_resent_to_hdr = 1;
	    else if (match_hdr ("return-path", linebuf)) {
		char *colon = strchr (linebuf, ':');
		char *return_path = prefrobnicate (colon+1);

		if (*return_path == '\0' ||
		    match_prefix (return_path, "mailer-daemon") ||
		    match_prefix (return_path, "mmdf") ||
		    match_prefix (return_path, "uucp"))
		    looks_like_bounce = 1;
		else
		    goto delete_header;
	    }
	    else if (match_hdr ("return-receipt-to", linebuf))
		goto delete_header;
	    else if (match_hdr ("subject", linebuf)) {
		char *colon = strchr (linebuf, ':');

		if (match_command (colon+1, "subscribe") ||
		    match_command (colon+1, "unsubscribe") ||
		    match_command (colon+1, "help"))
		    saw_command = 1;
		else if (contains (colon+1, "automatic log from mail gateway") ||
			 contains (colon+1, "delivery confirmation") ||
			 contains (colon+1, "delivery error") ||
			 contains (colon+1, "delivery failure") ||
			 contains (colon+1, "delivery problems") ||
			 contains (colon+1, "delivery report") ||
			 contains (colon+1, "delivery status") ||
			 contains (colon+1, "failed mail") ||
			 contains (colon+1, "mail failure") ||
			 contains (colon+1, "mail system problem") ||
			 contains (colon+1, "message delayed") ||
			 contains (colon+1, "non-delivery of:") ||
			 contains (colon+1, "non-delivery notification") ||
			 contains (colon+1, "non-delivery report") ||
			 contains (colon+1, "nondelivery notification") ||
			 contains (colon+1, "rcpt:") ||
			 contains (colon+1, "receipt confirmation") ||
			 contains (colon+1, "returned mail") ||
			 contains (colon+1, "service message") ||
			 contains (colon+1, "undeliverable mail") ||
			 contains (colon+1, "undeliverable message") ||
			 contains (colon+1, "undelivered mail") ||
			 contains (colon+1, "waiting mail") ||
			 contains (colon+1, "warning from uucp") ||
			 contains (colon+1, "warning message") ||
			 contains (colon+1, "warning: cannot send mail"))
		    looks_like_bounce = 1;
	    }
	    else if (match_hdr ("x-confirm-reading-to", linebuf))
		goto delete_header;		/* pegasus mail brain damage */
	    else if (match_hdr ("x-pmrqc", linebuf))
		goto delete_header;		/* pegasus mail brain damage */
	    else if (match_hdr ("x-report-type", linebuf)) {
	 	char *colon = strchr (linebuf, ':');

		if (contains (colon+1, "nondelivery"))
		    looks_like_bounce = 1;
	    }
	    else if (isblankline (linebuf)) {
		break;
	    }
	}
	fputs (linebuf, out);
    delete_header:
	bol = (linebuf[strlen(linebuf)-1] == '\n');
    }

    /*
     * End of header in input.  Add any headers that need to appear
     * at the end.
     */

    if (precedence != NULL)
	fprintf (out, "Precedence: %s\n", precedence);

    /* If we haven't seen a To, Cc, or Bcc Field, add a dummy one. */
    if (has_to_hdr == 0)
	fprintf (out, "To: undisclosed-recipients:;\n");

    /* add a reply-to header if required */
    if (reply_to != NULL &&
	(force_reply_to == 1 || has_reply_to_hdr == 0)) {
	fprintf (out, "Reply-To: %s\n", reply_to);
    }
    
    /* add a message-id header if there's not one there already */
    if (has_message_id_hdr == 0) {
	struct tm *tm = localtime (&t);
	
	fprintf (out, "Message-ID: <bulk.%d.%4d%02d%02d%02d%02d%02d@%s>\n",
		 getpid(), tm->tm_year + 1900, tm->tm_mon + 1, tm->tm_mday,
		 tm->tm_hour, tm->tm_min, tm->tm_sec, local_domain);
    }
    fprintf (out, "\n");

    /*
     * copy message body, notice if we've got an unsubscribe command
     */
    lines = 0;
    while (fgets (linebuf, sizeof (linebuf), in) != NULL) {
	if (lines < 5 &&
	    /*
	     * these often occur in English-text unsubscribe requests
	     */
	    contains (linebuf, "delete me") ||
	    contains (linebuf, "remove me") ||
	    contains (linebuf, "subscribe") ||
	    contains (linebuf, "unsubscribe"))
	    saw_command = 1;
	++lines;
	fputs (linebuf, out);
    }
    /*
     * if we've seen an administrative command, and the message is short,
     * and we didn't see an approved or resent-to header, return
     * a special code so that our caller will know to forward us to
     * the human.
     */
    if (has_valid_approved_hdr == 0 && looks_like_bounce)
	return LOOKS_LIKE_BOUNCE;
    else if (has_valid_approved_hdr == 0 && saw_command > 0 && lines <= 7)
	return HAS_EMBEDDED_COMMAND;
    else
	return MESSAGE_OKAY;
}

static int
size_of_file (fd)
int fd;
{
    struct stat sbuf;

    if (fstat (fd, &sbuf) < 0)
	return 0x7fffffff;
    return sbuf.st_size;
}

static int
digits_only (s)
char *s;
{
    do {
	if (!isdigit (*s))
	    return 0;
    } while (*++s);
    return 1;
}

static char *
get_local_domain_name ()
{
    static char buf[1024];
    int i;

    if (gethostname (buf, sizeof (buf)) == 0) {
	if (strchr (buf, '.'))
	    return buf;
	else {
	    struct hostent *hp = gethostbyname (buf);

	    if (hp) {
		if (strchr (hp->h_name, '.')) {
		    strcpy (buf, hp->h_name);
		    return buf;
		}
		else {
		    for (i = 0; hp->h_addr_list[i]; ++i) {
			if (strchr (hp->h_addr_list[i], '.')) {
			    strcpy (buf, hp->h_addr_list[i]);
			    return buf;
			}
		    }
		}
	    }
	}
    }
    return NULL;
}

/*
 * majordomo 'resend' options we might want to support someday:
 *
 *      -C <config-file>        specify alternate config file (must be first!)
 *      -l <list-name>          REQUIRED: specify list name
 *      -h <host-name>          REQUIRED: specify host name
 *      -f <from-addr>          specify "sender" (default <list-name>-request)
 *      -m <sendmail-flags>     specify special sendmail flags
 *      -M <max-msg-length>     specify max message length to forward
 *      -p <precedence>         add "Precedence: <precedence>" header
 *      -r <reply-to>           add "Reply-To: <reply-to>" header
 *      -I <file-list>          Bounce messages from users not listed in file
 *                                      in colon-separated <file-list>
 *      -a <passwd>             approval password
 *      -A                      moderate list (require "Approved:" for posting)
 *      -R                      delete "Received:" lines
 *      -s                      enable "administrivia" checks
 *      -d                      debug; say it, but don't do it
 */

int
main (argc, argv)
int argc;
char *argv[];
{
    int i;
    FILE *fp;
    FILE *tmp;
    static char template[] = "/tmp/blkXXXXXX";
    char *tempname;
    int c;
    char buf[1024];

    while (argc > 1 && (*argv[1] == '-' || *argv[1] == '+')) {
	if (strcmp (argv[1], "-debug") == 0)
	    ++debug_flag;
	else if (strcmp (argv[1], "-domain") == 0 && argc > 2) {
	    if (strchr (argv[2], '.') == NULL) {
		fprintf (stderr,
			 "Illegal argument to -domain: %s is not a fully-qualified domain name\n",
			 argv[2]);
		exit (EX_USAGE);
	    }
	    local_domain = argv[2]; /* done */
	    --argc;
	    ++argv;
	}
	else if (strcmp (argv[1], "-maxdomains") == 0 && argc > 2) {
	    max_domains = atoi (argv[2]);
	    if (max_domains <= 0 || !digits_only (argv[2])) {
		fprintf (stderr, "illegal value for -maxdomains: %s\n",
			 argv[2]);
		exit (EX_USAGE);
	    }
	    --argc;
	    ++argv;
	}
	else if (strcmp (argv[1], "-maxsize") == 0 && argc > 2) {
	    max_size = atoi (argv[2]);
	    if (max_size <= 0 || !digits_only (argv[2])) {
		fprintf (stderr, "illegal value for -maxsize: %s\n",
			 argv[2]);
		exit (EX_USAGE);
	    }
	    --argc;
	    ++argv;
	}
	else if (strcmp (argv[1], "-owner") == 0 && argc > 2) {
	    list_owner = argv[2];
	    --argc;
	    ++argv;
	}
	else if (strcmp (argv[1], "-precedence") == 0 && argc > 2) {
	    precedence = argv[2];
	    --argc;
	    ++argv;
	}
	else if (strcmp (argv[1], "-reply-to") == 0 && argc > 2) {
	    reply_to = argv[2];
	    --argc;
	    ++argv;
	}
	else if (strcmp (argv[1], "+reply-to") == 0 && argc > 2) {
	    reply_to = argv[2];
	    force_reply_to = 1;
	    --argc;
	    ++argv;
	}
	else if (strcmp (argv[1], "-sendmail") == 0 && argc > 2) {
	    sendmail_flags = argv[2];
	    --argc;
	    ++argv;
	}
	else if (strcmp (argv[1], "-v") == 0)
	    ++verbose_flag;
	else {
	    fprintf (stderr, "unknown flag %s\n", argv[1]);
	    exit (EX_USAGE);
	}
	--argc;
	++argv;
    }
    if (argc != 3) {
	fprintf (stderr,
		 "usage: bulk_mailer [options] envelope_from recipient_list_file\n");
	fprintf (stderr, "(message appears on stdin)\n");
	fprintf (stderr, "options are (NOTE: NR = NOT RECOMMENDED):\n");
	fprintf (stderr, "\
    -debug                 display SMTP output on stderr; don't send msg\n\
    -domain xx.yy.zz       set domain name to 'xx.yy.zz' (overrides default)\n\
    -maxdomains ###        set max # of distinct domains per envelope\n\
    -maxsize ####          set maximum message size\n\
    -owner user@domain     set name of list owner\n\
    -precedence keyword    add a \"Precedence: keyword\" header field (NR)\n\
    -reply-to user@domain  add a reply-to header if there's not one there\n\
    +reply-to user@domain  force reply-to header even if there already (NR)\n\
    -sendmail flags        pass 'flags' to sendmail\n\
    -v                     turn on verbose mode\n");
	exit (EX_USAGE);
    }
    envelope_from = argv[1];

    if (list_owner == NULL)
	list_owner = envelope_from;
    if (local_domain == NULL)
	local_domain = get_local_domain_name ();
    if (local_domain == NULL) {
	fprintf (stderr, "bulk_mailer: can't find local domain\n");
	exit (EX_OSFILE);
    }

    tempname = mktemp (template);
    tmp = fopen (template, "w");
    switch (copy_message (tmp, stdin)) {

    case HAS_EMBEDDED_COMMAND:
	fclose (tmp);
	tmp = open_envelope ();
	add_recipient (tmp, list_owner);
	forward_msg (tmp, tempname, list_owner,
		     "message appears to have a list server command");
	close_envelope (tmp);
	unlink (tempname);
	exit (EX_OK);

    case LOOKS_LIKE_BOUNCE:
	fclose (tmp);
	tmp = open_envelope ();
	add_recipient (tmp, list_owner);
	forward_msg (tmp, tempname, list_owner,
		     "this looks like a bounced message");
	close_envelope (tmp);
	unlink (tempname);
	exit (EX_OK);

    case MESSAGE_OKAY:
	if (size_of_file (fileno (tmp)) > max_size) {
	    fprintf (stderr,
		     "bulk_mailer: message size (%d bytes) exceeds administrative limit\n",
		     size_of_file (fileno (tmp)));
	    fclose (tmp);
	    unlink (tempname);
	    exit (EX_NOPERM);
	}
	fclose (tmp);
	
	if ((fp = fopen (argv[2], "r")) == NULL) {
	    fprintf (stderr,
		     "bulk_mailer: can't open recipient list %s (%s)\n",
		     argv[2], strerror (errno));
	    unlink (tempname);
	    exit (EX_NOINPUT);
	}
	process (fp);
	fclose (fp);
	
	sort_addrs ();
	dump_addrs (tempname);
	unlink (tempname);
	
	exit (EX_OK);
    }
}
