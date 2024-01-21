/*
 * http.c
 *
 * This file contains an implementation of the HTTP/1.0 http protocol.
 * Note, it does not implement the LINK and UNLINK methods, which are
 * part of HTTP/1.0.
 *
 * Still left to do:
 *
 * - Implement an asynchronous interface.
 *
 * - Bindings should be sorted to allow specific bindings to match
 *   always before specific bindings. An example:
 *
 *   http bind /AA get {}
 *   http bind /A* get {}
 *
 *   In this case, the second rule fires which is perhaps not what
 *   we want. However, it is not easy to determine the most general
 *   binding. Ideas welcome.
 *
 * - RFC 1521 base64 encodings (during server processing)
 * - More %-escapes: 
 *   %V Arguments split using the URL argument rules.
 *   %W The password of the user.
 *   %U The user name.
 *
 * Copyright (c) 1994, 1995
 *
 * J. Schoenwaelder
 * TU Braunschweig, Germany
 * Institute for Operating Systems and Computer Networks
 *
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that this copyright
 * notice appears in all copies.  The University of Braunschweig
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 *
 */

#include "scotty.h"
#include <time.h>
#include <sys/stat.h>

static char *proxy = NULL;
static int proxyport = 80;
static char *httpVersion = "HTTP/1.0";

static int watch = 0;

#define HTTP_OK			200
#define HTTP_CREATED		201
#define HTTP_ACCEPTED		202
#define HTTP_PARTIAL		203
#define HTTP_NORESPONSE		204
#define HTTP_DELETED		205
#define HTTP_MODIFIED		206

#define HTTP_MOVED		301
#define HTTP_FOUND		302
#define HTTP_METHOD		303
#define HTTP_NOTMODIFIED	304

#define HTTP_BADREQUEST		400
#define HTTP_UNAUTH		401
#define HTTP_PAYFORIT		402
#define HTTP_FORBIDDEN		403
#define HTTP_NOTFOUND		404
#define HTTP_NOTALLOWED		405
#define HTTP_NONEACCEPTABLE	406
#define HTTP_PROXYAUTHREQUIRED	407
#define HTTP_REQUESTTIMEOUT	408

#define HTTP_INTERNAL		500
#define HTTP_NOTYET		501
#define HTTP_BADGATEWAY		502
#define HTTP_SERVICEUNAVAIL	503
#define HTTP_GATEWAYTIMEOUT	504

/*
 * Used to build a table of error codes and their readable 
 * error description (see below). This is based on the http
 * internet draft issued 19 December 1994.
 */

typedef struct HttpStatus {
    int code;
    char *phrase;
} HttpStatus;

static HttpStatus httpStatusTable[] = {
    { HTTP_OK,			"OK" },
    { HTTP_CREATED,		"Created" },
    { HTTP_ACCEPTED,		"Accepted" },
    { HTTP_PARTIAL,		"Provisional Information" },
    { HTTP_NORESPONSE,		"No Response" },
    { HTTP_DELETED,		"Deleted" },
    { HTTP_MODIFIED,		"Modified" },

    { HTTP_MOVED,		"Moved Permanently" },
    { HTTP_FOUND,		"Moved Temporarily" },
    { HTTP_METHOD,		"Method" },
    { HTTP_NOTMODIFIED,		"Not Modified" },

    { HTTP_BADREQUEST,		"Bad Request" },
    { HTTP_UNAUTH,		"Unauthorized" },
    { HTTP_PAYFORIT,		"Payment Required" },
    { HTTP_FORBIDDEN,		"Forbidden" },
    { HTTP_NOTFOUND,		"Not Found" },
    { HTTP_NOTALLOWED,		"Method Not Allowed" },
    { HTTP_NONEACCEPTABLE,	"None Acceptable" },
    { HTTP_PROXYAUTHREQUIRED,	"Proxy Authentication Required" },
    { HTTP_REQUESTTIMEOUT,	"Request Timeout" },

    { HTTP_INTERNAL,		"Internal Error" },
    { HTTP_NOTYET,		"Not Implemented" },
    { HTTP_BADGATEWAY,		"Bad Gateway" },
    { HTTP_SERVICEUNAVAIL,	"Service Unavailable" },
    { HTTP_GATEWAYTIMEOUT,	"Gateway Timeout" },

    { 0, NULL },
};

/*
 * A structure to describe a HTTP handle.
 */

typedef struct HttpToken {
    int sock;
    Tcl_Interp *interp;
    FILE *file;
} HttpToken;

/*
 * The event types currently in use. More to come soon ;-)
 */

#define HTTP_GET_EVENT		1
#define HTTP_HEAD_EVENT		2
#define HTTP_POST_EVENT		3
#define HTTP_PUT_EVENT		4
#define HTTP_DELETE_EVENT	5
#define HTTP_LINK_EVENT		6
#define HTTP_UNLINK_EVENT	7

typedef struct HttpEvent {
    int type;
    char *name;
} HttpEvent;

static HttpEvent httpEvents[] = {
    { HTTP_GET_EVENT,		"get" },
    { HTTP_HEAD_EVENT,		"head" },
    { HTTP_POST_EVENT,		"post" },
    { HTTP_PUT_EVENT,		"put" },
    { HTTP_DELETE_EVENT,	"delete" },
    { HTTP_LINK_EVENT,		"link" },
    { HTTP_UNLINK_EVENT,	"unlink" },
    { 0,			NULL },
};

/*
 * A structure to hold a binding for a given URL.
 */

typedef struct HttpBinding {
    int eventType;			/* Event that triggers binding. */
    char *pattern;			/* URL pattern to match.	*/
    char *command;			/* Tcl command to evaluate.     */
    struct HttpBinding *nextPtr;	/* Next binding in our list.    */
} HttpBinding;

static HttpBinding *bindList = (HttpBinding *) NULL;

/*
 * The struct HttpUrl is used to represent a URL. 
 */

typedef struct HttpUrl {
    char *host;
    int port;
    char *path;
    char *auth;
} HttpUrl;

/*
 * The following two hash tables are used to hold the content-type and
 * content-encoding mappings. They are indexed by file extensions.
 */

static Tcl_HashTable mimeTypeTable;

/*
 * Forward declarations for procedures defined later in this file:
 */

static char*
Base64Encode		_ANSI_ARGS_((char *string));

static char*
Base64Decode		_ANSI_ARGS_((char *string));

static struct servent *
HttpGetServent		_ANSI_ARGS_((char *name));

static struct hostent*
HttpGetHostent		_ANSI_ARGS_((char *name));

static HttpUrl*
HttpSplitUrl		_ANSI_ARGS_((char *str));

static void
HttpRequestLine		_ANSI_ARGS_((FILE *f, char *method, char *path));

static void
HttpStatusLine		_ANSI_ARGS_((FILE *f, int code));

static void
HttpGeneralHeader	_ANSI_ARGS_((FILE *f));

static void
HttpRequestHeader	_ANSI_ARGS_((FILE *f, char *auth));

static void
HttpResponseHeader	_ANSI_ARGS_((FILE *f));

static void
HttpObjectHeader	_ANSI_ARGS_((FILE *f, char *contentType, 
				     char *contentEncoding, 
				     int contentLength));
static void
HttpEndHeader		_ANSI_ARGS_((FILE *f));

static void
HttpSendError		_ANSI_ARGS_((FILE *f, int code));

static void
HttpSendObject		_ANSI_ARGS_((FILE *f, FILE *obj));

static int
HttpEvalCallback	_ANSI_ARGS_((Tcl_Interp *interp, char *callback,
				     char *addr, char *url));
static FILE*
HttpConnect		_ANSI_ARGS_((Tcl_Interp *interp, char *host, 
				     int port));
static FILE*
HttpOpen		_ANSI_ARGS_((char *fileName, char **contentType, 
				     char **contentEncoding, 
				     int *contentLength));
static void
HttpClose		_ANSI_ARGS_((FILE *f));

static void
HttpAcceptProc		_ANSI_ARGS_((ClientData clientData, int mask));

static int
HttpRecvHeader		_ANSI_ARGS_((Tcl_Interp *interp, FILE *f));

static int
HttpRecvBody		_ANSI_ARGS_((FILE *src, FILE *dst));

static int
HttpProxy		_ANSI_ARGS_((Tcl_Interp *interp, 
				     int argc, char **argv));
static int
HttpHead		_ANSI_ARGS_((Tcl_Interp *interp, 
				     int argc, char **argv));
static int
HttpGet			_ANSI_ARGS_((Tcl_Interp *interp, 
				     int argc, char **argv));
static int
HttpPost		_ANSI_ARGS_((Tcl_Interp *interp, 
				     int argc, char **argv));
static int
HttpPut			_ANSI_ARGS_((Tcl_Interp *interp, 
				     int argc, char **argv));
static int
HttpDelete		_ANSI_ARGS_((Tcl_Interp *interp, 
				     int argc, char **argv));
static int
HttpServer		_ANSI_ARGS_((Tcl_Interp *interp, 
				     int argc, char **argv));
static int
HttpBind		_ANSI_ARGS_((Tcl_Interp *interp, 
				     int argc, char **argv));
static int
HttpMime		_ANSI_ARGS_((Tcl_Interp *interp, 
				     int argc, char **argv));
static int
HttpWatch		_ANSI_ARGS_((Tcl_Interp *interp, 
				     int argc, char **argv));

/*
 * The following defines are used to implement base64 en/decoding.
 * See RFC 1521 for a description of base64 en/decoding.
 */

#define valid(x) \
	((x >= 'A' && x <= 'Z') || (x >= 'a' && x <= 'z') || \
	 (x >= '0' && x <= '9') || x == '+' || x == '/')

#define val(x) \
	(x >= 'A' && x <= 'Z' ? x - 'A' : \
	 (x >= 'a' && x <= 'z' ? x - 'a' + 26 : \
	  (x >= '0' && x <= '9' ? x - '0' + 52 : \
	   (x == '+' ? 62 : (x == '/' ? 63 : 0)))))

#define lav(x) \
	(x <= 25 ? x + 'A' : \
	 (x >= 26 && x <= 51 ? x + 'a' - 26 : \
	  (x >= 52 && x <= 61 ? x + '0' - 52 : \
	   (x == 62 ? '+' : (x == 63 ? '/' : '?')))))

/*
 * Base64Encode() encodes the string into a base64 encoded string.
 * The result is a private buffer and not the property of the caller.
 */

static char *
Base64Encode (in)
     char *in;
{
    static char *ret = NULL;
    static int size = 0;
    char *p;

    if ((strlen (in) + 4) * 2 >= size) {
	size = (strlen (in) + 4) * 2;
	if (ret) ckfree (ret);
	ret = ckalloc (size);
    }

    p = ret;

    for (;;) {
	unsigned char c [3];
	unsigned int d;
	int i, pad;

	if (! *in) break;

	i = 0, pad = -1;
	while (i < 3) {
	    if (*in) {
		c [i++] = *in++;
	    } else {
		c [i++] = 0, pad++;
	    }
	}
	  
	d = c[0] >> 2;
	*p++ = lav(d);

	if (pad < 2) {
	    d = ((c[0] & 0x3) << 4) | (c[1] >> 4);
	    *p++ = lav(d);
	} else {
	    *p++ = '=';
	}
	 
	if (pad < 1) {
	    d = ((c[1] & 0xf) << 2) | (c[2] >> 6);
	    *p++ = lav(d);
	} else {
	    *p++ = '=';
	}
	  
	if (pad < 0) {
	    d = c [2] & 0x3f;
	    *p++ = lav(d);
	} else  {
	    *p++ = '=';
	}
	  
	if (pad != -1) break;
    }

    *p = 0;
    
    return ret;
}

/*
 * Base64Decode() decodes the a base64 encoded string.
 * The result is a private buffer and not the property of the caller.
 */

static char *
Base64Decode (in)  
     char *in;
{
    static char *ret = NULL;
    static int size = 0;
    char *p;

    if ((strlen (in) + 4) * 2 >= size) {
	size = (strlen (in) + 4) * 2;
	if (ret) ckfree (ret);
	ret = ckalloc (size);
    }

    p = ret;

    for (;;) {
	int c [5];
	int i, d, pad;

	i = 0, pad = -1;
	while (i < 4) {
	    c [i] = *in++;
	    if (c [i] && valid (c [i])) {
		i++;
		continue;
	    }
	    if (c [i] && c [i] != '=') {
		continue;
	    }
	    while ((! c [i] || c [i] == '=') && i < 4) {
		pad++, c [i] = 0, i++, c[i] = '=';
	    }
	}
	  
	d = val (c[0]) << 18 | val (c[1]) << 12 | val (c[2]) << 6 | val (c[3]);
	  
	if (pad < 2)
	  *p++ = (d & 0xff0000) >> 16;
	if (pad < 1)
	  *p++ = (d & 0xff00) >> 8;
	if (pad < 0)
	  *p++ = d & 0xff;
	
	if (c [4] == '=') {
	    *p = 0;
	    break;
	}
    }
    
    return ret;
}

/*
 * Get a pointer to a servent structure. Used to map service names
 * and numbers. According to the assigned numbers RFC, a service name
 * may not start with a digit. So it should be save to look at the first
 * byte to decide if its a service name or not.
 */

static struct servent *
HttpGetServent(name)
     char *name;
{
    struct servent *servent;
    static struct servent _servent;
    
    if (isdigit(*name)) {
        _servent.s_port = htons (atoi(name));
        _servent.s_proto = "tcp";
	servent = (_servent.s_port == -1) ? NULL : &_servent;
    } else {
	servent = getservbyname (name, "tcp");
    }

    return servent;
}

/*
 * Get a pointer to a hostent structure. First try gethostbyname.
 * If this fails, try inet_addr and fake a hostent structure.
 */

static struct hostent *
HttpGetHostent(name)
     char *name;
{
    struct hostent *hostent;
    static struct hostent _hostent;
    static int hostaddr, hostaddrPtr[2];
	    
    hostent = gethostbyname (name);
    if (hostent != NULL) return hostent;

    hostaddr = inet_addr (name);
    if (hostaddr == -1) return NULL;

    _hostent.h_addr_list = (char **) hostaddrPtr;
    _hostent.h_addr_list[0] = (char *) &hostaddr;
    _hostent.h_addr_list[1] = NULL;
    _hostent.h_length = sizeof(hostaddr);
    _hostent.h_addrtype = AF_INET;
    return &_hostent;
}

/*
 * HttpSplitUrl() splits a given URL into various parts according to
 * RFC 1738. This functions returns a pointer into private memory.
 * The caller has top make sure to make a copy if it must call this
 * function more than once.
 */

static HttpUrl*
HttpSplitUrl (str)
     char *str;
{
    static HttpUrl url = { 0, 0, 0, 0 };
    char *hbuf;
    char *p, *q, *r = NULL;

    if (url.auth) {
	ckfree (url.auth);
	url.auth = NULL;
    }
    if (url.host) {
	ckfree (url.host);
	url.host = NULL;
    }
    if (url.path) {
	ckfree (url.path);
	url.path = NULL;
    }

    if (proxy) {
	url.host = ckstrdup (proxy);
	url.port = proxyport;
	url.path = ckstrdup (str);
	return &url;
    }
    
    hbuf = ckstrdup (str);

    /*
     * Strip off the beginning and check if we have a trivial
     * path on the local host.
     */

    p = hbuf;
    if (strncmp (p, "http://", 7) == 0) {
	p += 7;
    } else if (strncmp (p, "//", 2) == 0) {
	p += 2;
    } else {
	url.path = ckstrdup (p);
    }

    if (url.path) {
	url.host = ckstrdup ("localhost");
	url.port = 80;
	ckfree (hbuf);
	return &url;
    }

    /*
     * Now we know that we start with at least a host name. First get
     * the path before we start to extract the various optional host
     * specific informations.
     */

    q = strchr (p, '/');
    if (!q) {
	url.path = ckstrdup ("/");
    } else {
	url.path = ckstrdup (q);
	*q = '\0';
    }

    /*
     * Next, lets see if we have a user and password field.
     */

    q = strchr (p, '@');
    if (q) {
	r = p;
	*q = '\0';
	p = ++q;
    }

    q = strchr (p, ':');
    if (q) {
	*q = '\0';
	url.host = ckstrdup (p);
	url.port = atoi (++q);
    } else {
	url.host = ckstrdup (p);
	url.port = 80;
    }

    /*
     * Split up the user and password fields if we have one.
     */

    if (r) {
	url.auth = ckstrdup (r);
    }

    ckfree (hbuf);
    return &url;
}

/*
 * HttpRequestLine() sends a request line to a server.
 */

static void
HttpRequestLine (f, method, path)
     FILE *f;
     char *method;
     char *path;
{
    fprintf (f, "%s %s %s\r\n", method, path, httpVersion);
}

/*
 * HttpStatusLine() sends a status line to a client.
 */

static void
HttpStatusLine (f, code)
     FILE *f;
     int code;
{
    HttpStatus *he = httpStatusTable;

    while (he->code && he->code != code) he++;

    if (he->code) {
	fprintf (f, "%s %d %s\r\n", httpVersion, he->code, he->phrase);
    } else {
	fprintf (f, "%s 500 Unknown Internal Error\r\n", httpVersion);
    }
}

/*
 * HttpGeneralHeader() sends the general header portion as defined
 * in the HTTP draft standard.
 */

static void
HttpGeneralHeader (f)
     FILE *f;
{
    static char *weekdays[] = {
	"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"
    };
    static char *months[] = {
	"Jan", "Feb", "Mar", "Apr", "May", "Jun",
	"Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
    };

    time_t clock = time(0);
    struct tm *tm = gmtime (&clock);

    fprintf (f, "Date: %s, %2d %s 19%2d %02d:%02d:%02d\r\n",
	     weekdays[tm->tm_wday], tm->tm_mday, 
	     months[tm->tm_mon], tm->tm_year,
	     tm->tm_hour, tm->tm_min, tm->tm_sec);
    fputs ("MIME-Version: 1.0\r\n", f);
}

/*
 * HttpRequestHeader() sends a request header to a server.
 */

static void
HttpRequestHeader (f, auth)
     FILE *f;
     char *auth;
{
    fprintf (f, "User-Agent: scotty/%s\r\n", SCOTTY_VERSION);
    if (auth) {
	fprintf (f, "Authorization: Basic %s\r\n", Base64Encode (auth));
    }    
    fputs ("Accept: text/plain; text/html\r\n", f);
    fputs ("Accept-Encoding: \r\n", f);
}

/*
 * HttpResponseHeader() sends a response header to a client.
 */

static void
HttpResponseHeader (f)
     FILE *f;
{
    fprintf (f, "Server: scotty/%s\r\n", SCOTTY_VERSION);
}

/*
 * HttpObjectHeader() sends an object header. Used by clients
 * and the server.
 */

static void
HttpObjectHeader (f, contentType, contentEncoding, contentLength)
     FILE *f;
     char *contentType;
     char *contentEncoding;
     int contentLength;
{
    fprintf (f, "Content-Type: %s\r\n", contentType);
    if (contentEncoding) {
	fprintf (f, "Content-Encoding: %s\r\n", contentEncoding);
    }
    fprintf (f, "Content-Length: %d\r\n", contentLength);
}

/*
 * HttpEndHeader() terminates a header section. Very simple.
 */

static void
HttpEndHeader (f)
     FILE *f;
{
    fputs ("\r\n", f);
    fflush (f);
    rewind (f);
}

/*
 * HttpSendError() writes an error message to the output stream.
 */

static void
HttpSendError (f, code)
     FILE *f;
     int code;
{
    char *msg = "Nice error messages are not yet implemented!";

    HttpStatusLine (f, code);
    HttpGeneralHeader (f);
    HttpResponseHeader (f);
    fprintf (f, "Content-Type: text/plain\r\n");
    fprintf (f, "Content-Length: %d\r\n", (int) strlen (msg));
    fputs ("\r\n", f);
    fputs (msg, f);
}

/* 
 * HttpSendObject() writes the object given by obj to the stream.
 */

static void
HttpSendObject (f, obj)
     FILE *f;
     FILE *obj;
{
    char buf[1024];
    int n, t = 0;

    while ((n = fread (buf, 1, sizeof (buf), obj)) > 0) {
        fwrite (buf, 1, n, f);
	if (watch) fprintf (stderr, "%d ", t += n);
    }
    if (watch) fprintf (stderr, "\n");

    fclose (obj);
    fflush (f);
}

/*
 * Evaluate a Tcl callback script. The command string is modified
 * according to the % escapes before evaluation. The list of supported
 * escapes is %A = address, %P = path, %S = searchpart.
 */

static int
HttpEvalCallback (interp, callback, addr, path)
     Tcl_Interp *interp;
     char *callback;
     char *addr;
     char *path;
{
    char buf[20];
    int code;
    Tcl_DString tclCmd;
    char *startPtr, *scanPtr;
    char *url, *search = NULL;

    /*
     * Split the path into a url and a search part. Make our own copy
     * to save the original path.
     */

    url = ckstrdup (path);
    for (scanPtr = url; *scanPtr != '\0'; scanPtr++) {
	if (!search && (*scanPtr == '?')) {
	    *scanPtr = '\0';
	    search = ++scanPtr;
	}
    }

    Tcl_DStringInit (&tclCmd);
    startPtr = callback;
    for (scanPtr = startPtr; *scanPtr != '\0'; scanPtr++) {
	if (*scanPtr != '%') {
	    continue;
	}
	Tcl_DStringAppend (&tclCmd, startPtr, scanPtr - startPtr);
	scanPtr++;
	startPtr = scanPtr + 1;
	switch (*scanPtr) {
	  case 'A':
	    if (addr) {
		Tcl_DStringAppend (&tclCmd, addr, -1);
	    }
	    break;
	  case 'P':
	    Tcl_DStringAppend (&tclCmd, url, -1);
	    break;
	  case 'S':
	    if (search) {
		Tcl_DStringAppend (&tclCmd, search, -1);
	    }
	    break;
	  default:
	    sprintf (buf, "%%%c", *scanPtr);
	    Tcl_DStringAppend (&tclCmd, buf, -1);
	}
    }
    Tcl_DStringAppend (&tclCmd, startPtr, scanPtr - startPtr);

    /*
     * Now evaluate the callback function and issue a background
     * error if the callback fails for some reason. Return the
     * original error message and code to the caller.
     */
    
    Tcl_AllowExceptions (interp);
    code = Tcl_GlobalEval (interp, Tcl_DStringValue (&tclCmd));
    Tcl_DStringFree (&tclCmd);

    if (code == TCL_ERROR) {
	char *errorMsg = ckstrdup (interp->result);
	Tcl_AddErrorInfo (interp, "\n    (http callback)");
        Tk_BackgroundError (interp);
	Tcl_SetResult (interp, errorMsg, TCL_DYNAMIC);
    }

    ckfree (url);
    
    return code;
}


/*
 * Open a tcp connection to the host using the given port. Errors 
 * are kept in the interpreter and a NULL pointer is returned.
 */

static FILE *
HttpConnect (interp, host, port)
     Tcl_Interp *interp;
     char *host;
     int port;
{
    int sock;
    FILE *f;
    struct hostent *hp;
    struct servent servent;
    struct sockaddr_in name;

    hp = HttpGetHostent (host);
    if (hp == NULL) {
	Tcl_AppendResult (interp, "no such host \"", host,
			  "\"", (char *) NULL);
	return (FILE *) NULL;
    }

    servent.s_port = htons (port);
    servent.s_proto = "tcp";

    sock = socket (PF_INET, SOCK_STREAM, 0);
    if (sock < 0) {
        Tcl_AppendResult (interp, "could not create socket: ", 
			  Tcl_PosixError (interp), (char *) NULL);
	return (FILE *) NULL;
    }
    
    memcpy ((char *) &name.sin_addr, (char *) hp->h_addr, hp->h_length);
    name.sin_family = AF_INET;
    name.sin_port = servent.s_port;
     
    if (connect (sock, (struct sockaddr *) &name, sizeof(name)) < 0) {
	Tcl_AppendResult (interp, "can not connect to host \"", host,
			  "\": ", Tcl_PosixError (interp), (char *) NULL);
	close (sock);
	return (FILE *) NULL;
    }
    
    if ((f = fdopen (sock, "w+")) == NULL) {
        Tcl_AppendResult (interp, "couldn't open file: ",
			  Tcl_PosixError (interp), (char *) NULL);
	close (sock);
	return (FILE *) NULL;
    }

    return f;
}

/*
 * HttpOpen() opens a local file and returns the contentType,
 * contentEncoding and contentLength. A NULL pointer is returned in
 * case of an error.
 */

static FILE *
HttpOpen (fileName, contentType, contentEncoding, contentLength)
     char *fileName;
     char **contentType;
     char **contentEncoding;
     int *contentLength;
{
    FILE *f;
    struct stat st;
    char *dot = NULL;
    int n;

    if ((f = fopen (fileName, "r")) == NULL) {
	return NULL;
    }

    if (fstat (fileno (f), &st) < 0) {
        return NULL;
    }

    *contentEncoding = NULL;
    *contentType = NULL;
    *contentLength = (int) st.st_size;

    /*
     * Guess the content encoding and the content type. We only support
     * x-compress and x-gzip encoding. This is a quick hack.
     */
    
    for (n = strlen (fileName) - 1; n >= 0; n--) {
	if (fileName[n] == '.' && *contentEncoding == NULL) {
	    if (strcmp (fileName+n, ".gz") == 0) {
		*contentEncoding = "x-gzip";
		dot = fileName + n;
		*dot = '\0';
	    } else if (strcmp (fileName+n, ".Z") == 0) {
		*contentEncoding = "x-compress";
		dot = fileName + n;
                *dot = '\0';
	    }
	}
	if (fileName[n] == '.') {
	    Tcl_HashEntry *entryPtr;
	    entryPtr = Tcl_FindHashEntry (&mimeTypeTable, fileName+n+1);
	    if (entryPtr) {
		*contentType = (char *) Tcl_GetHashValue (entryPtr);
	    }
	    break;
	}
    }
    if (dot) {
	*dot = '.';
    }

    if (! *contentType) {
	*contentType = "text/plain";
    }

    return f;
}

/*
 * Close a http file handle. Flush the buffer, shutdown the tcp
 * connection and close the file handle.
 */

static void
HttpClose (f)
     FILE *f;
{
    fflush (f);
    shutdown (fileno (f), 2);
    fclose (f);
}

/*
 * HttpAcceptProc() is called from the event loop to accept a http
 * request. We just read the first bytes send from the client to see
 * what we are supposed to do.
 */

static void
HttpAcceptProc (clientData, mask)
     ClientData clientData;
     int mask;
{
    HttpToken *h = (HttpToken *) clientData;
    HttpBinding *bindPtr;
    int eventType;
    struct sockaddr_in sockaddr;
    int len = sizeof(sockaddr);
    int fd, rc;
    FILE *f;
    char buf[512];
    char *method = NULL, *path = NULL, *version = NULL, *p;

    fd = accept (h->sock, (struct sockaddr *) &sockaddr, &len);
    if (fd < 0) {
	return;
    }

    if ((f = fdopen (fd, "w+")) == NULL) {
        close (fd);
        return;
    }

    /*
     * Turn off buffering. Otherwise, we run into nasty interaction
     * problems with gets/puts/read and our transmission commands below.
     * Problem noticed on Linux machines.
     */

    setbuf (f, NULL);

    if (fgets (buf, 512, f) == NULL) {
	goto done;
    }

    /* 
     * parse the request line - ugly and simple version
     */
    
    p = buf;
    while (*p) {
	
	while (*p && isspace(*p)) p++;
	for (method = p; *p && !isspace(*p); p++) ;
	if (*p) *p++ = '\0';
	
	while (*p && isspace(*p)) p++;
	for (path = p; *p && !isspace(*p); p++) ;
	if (*p) *p++ = '\0';
	
	while (*p && isspace(*p)) p++;
	for (version = p; *p && !isspace(*p); p++) ;
	if (*p) *p++ = '\0';
	
	while (*p && isspace(*p)) p++;
    }

    rewind (f);

    if (method == NULL || path == NULL) {
	HttpSendError (f, HTTP_INTERNAL);
	goto done;
    }

    if (strcmp (method, "GET") == 0) {
        eventType = HTTP_GET_EVENT;
    } else if (strcmp (method, "HEAD") == 0) {
        eventType = HTTP_HEAD_EVENT;
    } else if (strcmp (method, "POST") == 0) {
        eventType = HTTP_POST_EVENT;
    } else if (strcmp (method, "PUT") == 0) {
        eventType = HTTP_PUT_EVENT;
    } else if (strcmp (method, "DELETE") == 0) {
        eventType = HTTP_DELETE_EVENT;
    } else {
	HttpSendError (f, HTTP_NOTYET);
	goto done;
    }

    for (bindPtr = bindList; bindPtr; bindPtr = bindPtr->nextPtr) {
        if (eventType == bindPtr->eventType &&
	    Tcl_StringMatch (path, bindPtr->pattern)) break;
    }
    if (! bindPtr) {
        HttpSendError (f, HTTP_NOTFOUND);
	goto done;
    }

    rc = HttpEvalCallback (h->interp, bindPtr->command, 
			   inet_ntoa (sockaddr.sin_addr), path);
    if (rc == TCL_OK) {
	char *contentType, *contentEncoding;
	int contentLength;
	FILE *obj = HttpOpen (h->interp->result, 
			      &contentType, &contentEncoding, &contentLength);
	if (obj) {
	    HttpStatusLine (f, HTTP_OK);
	    HttpGeneralHeader (f);
	    HttpResponseHeader (f);
	    HttpObjectHeader (f, contentType, contentEncoding, contentLength);
	    HttpEndHeader (f);
	    HttpSendObject (f, obj);
	} else {
	    HttpSendError (f, HTTP_INTERNAL);
	}
    } else {
	HttpStatus *he;
	for (he = httpStatusTable; he->code; he++) {
	    if (strcmp (h->interp->result, he->phrase) == 0) break;
	}
	HttpSendError (f, he->code ? he->code : HTTP_INTERNAL);
    }

  done:
    HttpClose (f);
}

/*
 * Receive the header returned from an http server.
 */

static int
HttpRecvHeader (interp, f)
     Tcl_Interp *interp;
     FILE *f;
{
    char buf[512];
    char *p;
    char *code;
    int error;
    int len;

    rewind (f);

    if (fgets (buf, 512, f) == NULL) {
	Tcl_SetResult (interp, "connection closed by peer", TCL_STATIC);
	return TCL_ERROR;
    }
    
    len = strlen (buf);
    while (len > 0 && isspace(buf[len-1])) {
	len --;
	buf[len]='\0';
    }

    /*
     * extract the error code
     */

    for (p = buf; *p && !isspace(*p); p++) ;   /* skip the server id */
    while (*p && isspace(*p)) p++;

    code = p;
    while (*p && isdigit(*p)) p++;
    *p++ = '\0';

    error = atoi (code);
    if (error != HTTP_OK) {
        HttpStatus *he;
	for (he = httpStatusTable; he->code; he++) {
	    if (he->code == error) {
	        Tcl_SetResult (interp, he->phrase, TCL_STATIC);
		return TCL_ERROR;
		break;
	    }
	}
	Tcl_SetResult (interp, p, TCL_VOLATILE);
	return TCL_ERROR;
    }

    while (fgets(buf, 512, f) != NULL) {
	len = strlen (buf);
	while (len > 0 && isspace(buf[len-1])) {
	    len --;
	    buf[len]='\0';
	}
	if (len == 0) {
	    return TCL_OK;
	}
	Tcl_AppendElement (interp, buf);
    }

    return TCL_OK;
}

/*
 * Get the body of the message from src and write it to dst.
 */

static int
HttpRecvBody (src, dst)
     FILE *src;
     FILE *dst;
{
    char buf[1024];
    int n, t = 0;
    
    while ((n = fread (buf, 1, sizeof(buf), src)) > 0) {
	if (watch) fprintf (stderr, "%d ", t += n);
	if (fwrite (buf, 1, n, dst) != n) {
	    return TCL_ERROR;
	}
    }
    if (watch) fprintf (stderr, "\n");
    
    return TCL_OK;
}

#if 0
/*
 * HttpProc() gets called from the event dispatcher when we receive
 * a reply from a server. It reads the result header.
 */

static void
HttpProc (clientData, mask)
     ClientData clientData;
     int mask;
{
    HttpToken *token = (HttpToken *) clientData;
    int code;

    code = HttpRecvHeader (token->interp, token->file);
    HttpClose (token->file);
    fprintf (stderr, token->interp->result);

    Tk_DeleteFileHandler (fileno (token->file));
    ckfree ((char *) token);
}
#endif

/*
 * Set the proxy host. A previously set host will be removed if
 * the argument to the proxy option is empty.
 */

static int
HttpProxy (interp, argc, argv)
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
    if (argc < 2 && argc > 3) {
	Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			  " proxy host\"", (char *) NULL);
	return TCL_ERROR;
    }
    
    if (argc == 3) {
	if (strlen (argv[2]) == 0) {
	    if (proxy) {
		ckfree (proxy);
		proxy = NULL;
		proxyport = 80;
	    }
	} else {
	    char *p = strchr(argv[2], ':');
	    if (p) {
		*p = '\0';
		if (Tcl_GetInt (interp, ++p, &proxyport) != TCL_OK)
			return TCL_ERROR;
		proxy = ckstrdup (argv[2]);
	    } else {
		proxy = ckstrdup (argv[2]);
		proxyport = 80;
	    }
	}
    }
    
    if (proxy) {
	Tcl_SetResult (interp, proxy, TCL_STATIC);
    }
    
    return TCL_OK;
}

/*
 * HttpHead() implements the http head option as decsribed in the
 * scotty man page.
 */

static int
HttpHead (interp, argc, argv)
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
    FILE *src;
    HttpUrl *url;
    int code;

    if (argc != 3) {
	Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			  " head url\"", (char *) NULL);
	return TCL_ERROR;
    }

    url = HttpSplitUrl (argv[2]);

    if ((src = HttpConnect (interp, url->host, url->port)) == NULL) {
	return TCL_ERROR;
    }

    HttpRequestLine (src, "HEAD", url->path);
    HttpGeneralHeader (src);
    HttpRequestHeader (src, url->auth);
    HttpEndHeader (src);

#if 0
    if (argc == 4) {
	HttpToken *token = (HttpToken *) ckalloc (sizeof (HttpToken));
	token->sock = -1;
	token->interp = interp;
	token->file = src;
	Tk_CreateFileHandler (fileno(src), TK_READABLE, HttpProc, 
			      (ClientData) token);
	return TCL_OK;
    }
#endif
    
    code = HttpRecvHeader (interp, src);
    HttpClose (src);
    return code;
}

/*
 * HttpGet() implements the http get option as decsribed in the
 * scotty man page.
 */

static int
HttpGet (interp, argc, argv)
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
    FILE *src, *dst;
    HttpUrl *url;
    int code;

    if (argc != 4) {
	Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			  " get url fileName\"", (char *) NULL);
	return TCL_ERROR;
    }

    if ((dst = fopen (argv[3], "w")) == NULL) {
	Tcl_AppendResult (interp, "couldn't open \"", argv[3],
			  "\": ", Tcl_PosixError (interp), (char *) NULL);
	return TCL_ERROR;
    }

    url = HttpSplitUrl (argv[2]);

    if ((src = HttpConnect (interp, url->host, url->port)) == NULL) {
	fclose (dst);
	return TCL_ERROR;
    }

    HttpRequestLine (src, "GET", url->path);
    HttpGeneralHeader (src);
    HttpRequestHeader (src, url->auth);
    HttpEndHeader (src);
    
    code = HttpRecvHeader (interp, src);
    if (code == TCL_OK) {
	code = HttpRecvBody (src, dst);
    }
    
    HttpClose (src);
    fclose (dst);
    return code;
}

/*
 * HttpPost() implements the http post option as decsribed in the
 * scotty man page.
 */

static int
HttpPost (interp, argc, argv)
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
    FILE *src, *dst, *obj;
    HttpUrl *url;
    char *contentType, *contentEncoding;
    int code, contentLength;

    if (argc != 5) {
	Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			  " post url text fileName\"", (char *) NULL);
	return TCL_ERROR;
    }

    url = HttpSplitUrl (argv[2]);

    if ((dst = fopen (argv[4], "w")) == NULL) {
	Tcl_AppendResult (interp, "couldn't open \"", argv[4],
			  "\": ", Tcl_PosixError (interp), (char *) NULL);
	return TCL_ERROR;
    }

    if ((src = HttpConnect (interp, url->host, url->port)) == NULL) {
	fclose (dst);
	return TCL_ERROR;
    }

    obj = HttpOpen (argv[3], &contentType, &contentEncoding, &contentLength);
    if (! obj) {
        HttpClose (src);
	fclose (dst);
        Tcl_AppendResult (interp, "can not read \"", argv[3], "\": ",
                          Tcl_PosixError (interp), (char *) NULL);
        return TCL_ERROR;
    }

    HttpRequestLine (src, "POST", url->path);
    HttpGeneralHeader (src);
    HttpRequestHeader (src, url->auth);
    HttpObjectHeader (src, contentType, contentEncoding, contentLength);
    HttpEndHeader (src);
    HttpSendObject (src, obj);

    code = HttpRecvHeader (interp, src);
    if (code == TCL_OK) {
	code = HttpRecvBody (src, dst);
    }
    HttpClose (src);
    fclose (dst);
    return code;
}

/*
 * HttpPut() implements the http put option as decsribed in the
 * scotty man page.
 */

static int
HttpPut (interp, argc, argv)
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
    FILE *src, *obj;
    HttpUrl *url;
    char *contentType, *contentEncoding;
    int code, contentLength;

    if (argc != 4) {
	Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			  " put url fileName\"", (char *) NULL);
	return TCL_ERROR;
    }

    url = HttpSplitUrl (argv[2]);

    if ((src = HttpConnect (interp, url->host, url->port)) == NULL) {
	return TCL_ERROR;
    }

    obj = HttpOpen (argv[3], &contentType, &contentEncoding, &contentLength);
    if (! obj) {
	HttpClose (src);
	Tcl_AppendResult (interp, "can not read \"", argv[3], "\": ",
			  Tcl_PosixError (interp), (char *) NULL);
	return TCL_ERROR;
    }

    HttpRequestLine (src, "PUT", url->path);
    HttpGeneralHeader (src);
    HttpRequestHeader (src, url->auth);
    HttpObjectHeader (src, contentType, contentEncoding, contentLength);
    HttpEndHeader (src);
    HttpSendObject (src, obj);

    code = HttpRecvHeader (interp, src);
    HttpClose (src);
    return code;
}

/*
 * HttpDelete() implements the http delete option as decsribed in the
 * scotty man page.
 */

static int
HttpDelete (interp, argc, argv)
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
    FILE *src;
    HttpUrl *url;
    int code;

    if (argc != 3) {
	Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			  " delete url\"", (char *) NULL);
	return TCL_ERROR;
    }

    url = HttpSplitUrl (argv[2]);

    if ((src = HttpConnect (interp, url->host, url->port)) == NULL) {
	return TCL_ERROR;
    }

    HttpRequestLine (src, "DELETE", url->path);
    HttpGeneralHeader (src);
    HttpRequestHeader (src, url->auth);
    HttpEndHeader (src);
    
    code = HttpRecvHeader (interp, src);
    HttpClose (src);
    return code;
}

/*
 * Become a http server. This command creates a listening socket
 * and registers a callback in the event loop to handle incoming
 * requests.
 */

static int
HttpServer (interp, argc, argv)
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
    static int sock = -1;
    static int port;
    struct servent *servent;
    struct sockaddr_in name;
    static HttpToken token;

    if (argc < 2 && argc > 3) {
	Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			  " server ?port?\"", (char *) NULL);
	return TCL_ERROR;
    }

    if (argc == 2) {
	if (sock > 0) {
	    sprintf (interp->result, "%d", port);
	}
	return TCL_OK;
    }

    if (sock > 0) {
	Tk_DeleteFileHandler (sock);
	close (sock);
	sock = -1;
    }

    /*
     * An empty port number just closes a server and is no error.
     */

    if (argv[2][0] == '\0') {
	return TCL_OK;
    }

    servent = HttpGetServent (argv[2]);
    if (servent == NULL) {
        Tcl_AppendResult (interp, "no such service \"", argv[3],
                          "\"", (char *) NULL);
        return TCL_ERROR;
    }
    port = ntohs (servent->s_port);

    sock = socket (PF_INET, SOCK_STREAM, 0);
    if (sock < 0) {
        Tcl_AppendResult (interp, "could not create socket: ", 
			  Tcl_PosixError (interp), (char *) NULL);
        return TCL_ERROR;
    }
    
    name.sin_addr.s_addr = INADDR_ANY;
    name.sin_family = AF_INET;
    name.sin_port = htons (port);
     
    if (bind (sock, (struct sockaddr *) &name, sizeof(name)) < 0) {
	Tcl_AppendResult (interp, "can not listen on port \"", argv[2],
			  "\": ", Tcl_PosixError (interp), (char *) NULL);
	close (sock);
	sock = -1;
	return TCL_ERROR;
    }

    if (listen (sock, 5) < 0) {
	Tcl_AppendResult (interp, "can not listen on port \"", argv[2],
			  "\": ", Tcl_PosixError (interp), (char *) NULL);
	close (sock);
	sock = -1;
	return TCL_ERROR;
    }
    
    token.sock = sock;
    token.interp = interp;
    token.file = NULL;

    Tk_CreateFileHandler (sock, TK_READABLE, HttpAcceptProc, 
			  (ClientData) &token);

    sprintf (interp->result, "%d", ntohs (name.sin_port));
    return TCL_OK;
}

/*
 * Create a new binding which will be used to map incoming URLs 
 * to Tcl scripts.
 */

static int
HttpBind (interp, argc, argv)
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
    int eventType = 0;
    HttpBinding *bindPtr;
    HttpEvent *event;

    if (argc < 4 || argc > 5) {
	Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			  " bind pattern method ?script?\"", (char *) NULL);
	return TCL_ERROR;
    }

    /*
     * Lookup the method name and convert it in our 
     * internal representation.
     */

    for (event = httpEvents; event->type; event++) {
	if (strcmp(argv[3], event->name) == 0) {
	    eventType = event->type;
	    break;
	}
    }

    if (! eventType) {
        Tcl_AppendResult (interp, "unknown method \"", argv[3],
			  "\": use one of", (char *) NULL);
	for (event = httpEvents; event->type; event++) {
	    Tcl_AppendResult (interp, event == httpEvents ? " " : ", ",
			      event->name, (char *) NULL);
	}
	return TCL_ERROR;
    }

    /*
     * Search for an already existing binding for this URL.
     */

    for (bindPtr = bindList; bindPtr; bindPtr = bindPtr->nextPtr) {
        if (eventType == bindPtr->eventType &&
	    (strcmp(bindPtr->pattern, argv[2]) == 0)) break;
    }

    if (argc == 4) {
        if (bindPtr) {
	    Tcl_SetResult (interp, bindPtr->command, TCL_STATIC);
        }
	return TCL_OK;
    }

    if (bindPtr) {

        ckfree (bindPtr->command);
	bindPtr->command = ckstrdup (argv[4]);

    } else {

	HttpBinding **bindPtrPtr = &bindList;
	int len = strlen (argv[2]);

        bindPtr = (HttpBinding *) ckalloc (sizeof (HttpBinding));
	bindPtr->eventType = eventType;
	bindPtr->pattern = ckstrdup (argv[2]);
	bindPtr->command = ckstrdup (argv[4]);

	while (*bindPtrPtr && strlen ((*bindPtrPtr)->pattern) > len) {
	    bindPtrPtr = &(*bindPtrPtr)->nextPtr;
	}

	bindPtr->nextPtr = *bindPtrPtr;
	*bindPtrPtr = bindPtr;
    }

    Tcl_SetResult (interp, bindPtr->command, TCL_STATIC);
    return TCL_OK;
}

/*
 * HttpMime() add or returns the current mime types known to the http
 * server. The mime types are used to map file name endings into MIME
 * Content-Type header fields.
 */

static int
HttpMime (interp, argc, argv)
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
    Tcl_HashTable *tablePtr = &mimeTypeTable;
    Tcl_HashEntry *entryPtr;
    Tcl_HashSearch search;
    Tcl_DString dst;
    Tcl_DStringInit (&dst);

    if (argc != 2 && argc != 4) {
	Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			  " mime ?type extension?\"", (char *) NULL);
	return TCL_ERROR;
    }

    if (argc == 4) {
	int isNew;

	entryPtr = Tcl_CreateHashEntry (tablePtr, ckstrdup (argv[3]), &isNew);
	Tcl_SetHashValue (entryPtr, (ClientData) ckstrdup (argv[2]));
    }

    entryPtr = Tcl_FirstHashEntry (tablePtr, &search);
    while (entryPtr) {
	Tcl_DStringStartSublist (&dst);
	Tcl_DStringAppendElement (&dst, Tcl_GetHashKey (tablePtr, entryPtr));
	Tcl_DStringAppendElement (&dst, (char *) Tcl_GetHashValue (entryPtr));
	Tcl_DStringEndSublist (&dst);
	entryPtr = Tcl_NextHashEntry (&search);
    }
    Tcl_DStringResult (interp, &dst);

    return TCL_OK;
}

/*
 * HttpWatch() is a debugging utility to see if there is still
 * something going on.
 */

static int
HttpWatch (interp, argc, argv)
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
    if (argc < 2 || argc > 3) {
	Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			  " watch ?bool?\"", (char *) NULL);
	return TCL_ERROR;
    }

    if (argc < 3) {
	Tcl_SetResult (interp, watch ? "1" : "0", TCL_STATIC);
	return TCL_OK;
    }

    return (Tcl_GetBoolean (interp, argv[2], &watch));
}

/*
 * This is the http command as described in the scotty documentation.
 */

int
Scotty_HttpCmd (clientData, interp, argc, argv)
     ClientData clientData;
     Tcl_Interp *interp;
     int argc;
     char **argv;
{
    static int initialized = 0;
    int length;
    char c;

    if (! initialized) {
	initialized = 1;
	Tcl_InitHashTable (&mimeTypeTable, TCL_STRING_KEYS);
    }

    if (argc < 2) {
	Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			  " option ?arg arg ...?\"", (char *) NULL);
	return TCL_ERROR;
    }

    c = argv[1][0];
    length = strlen (argv[1]);

    if (strncmp(argv[1], "proxy", length) == 0) {
        return HttpProxy (interp, argc, argv);
    } else if (strncmp(argv[1], "head", length) == 0) {
        return HttpHead (interp, argc, argv);
    } else if (strncmp(argv[1], "get", length) == 0) {
        return HttpGet (interp, argc, argv);
    } else if (strncmp(argv[1], "post", length) == 0) {
        return HttpPost (interp, argc, argv);
    } else if (strncmp(argv[1], "put", length) == 0) {
        return HttpPut (interp, argc, argv);
    } else if (strncmp(argv[1], "delete", length) == 0) {
        return HttpDelete (interp, argc, argv);
    } else if (strncmp(argv[1], "bind", length) == 0) {
        return HttpBind (interp, argc, argv);
    } else if (strncmp(argv[1], "server", length) == 0) {
        return HttpServer (interp, argc, argv);
    } else if (strncmp(argv[1], "mime", length) == 0) {
        return HttpMime (interp, argc, argv);
    } else if (strncmp(argv[1], "watch ", length) == 0) {
	return HttpWatch (interp, argc, argv);
    }

    Tcl_AppendResult (interp, "bad option \"", argv[1], "\": should be ",
	      "get, head, post, put, delete, proxy, bind, server, or mime",
		      (char *) NULL);
    return TCL_ERROR;
}
