/*
 * ined.c
 *
 * Extend a tcl command interpreter with an ined command. See the
 * documentation of tkined for more info about the ined command.
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
 */

#include "scotty.h"

#define BUF_INCR 1024

/*
 * The list that is used to queue received messages.
 */

struct q_elem {
    char   *msg;
    struct q_elem *next;
};
typedef struct q_elem q_elem;

static q_elem *queue = NULL;

/*
 * The default path where Tkined is installed. This
 * is normally overwritten in the Makefile.
 */

#ifndef TKINEDLIB
#define TKINEDLIB "/usr/local/lib/tkined"
#endif

/*
 * Forward declarations for procedures defined later in this file:
 */

static int
xread			_ANSI_ARGS_((int fd, char *buf, int len));

static int
xwrite			_ANSI_ARGS_((int fd, char *buf, int len));

static int 
string_to_type		_ANSI_ARGS_((char *str));

static void
InedInitialize		_ANSI_ARGS_((Tcl_Interp *interp));

static void 
InedFatal		_ANSI_ARGS_((void));

static void
InedQueue		_ANSI_ARGS_((void));

static void
InedFlushProc		_ANSI_ARGS_((ClientData clientData));

static void
InedFlushQueue		_ANSI_ARGS_((Tcl_Interp *));

static void 
InedAppendQueue		_ANSI_ARGS_((char *msg));

static char*
InedGets		_ANSI_ARGS_((void));

static int 
InedCompCmd		_ANSI_ARGS_((char *cmd, Tcl_Interp *interp, 
				     int argc, char **argv));
static void
InedReceiveProc		_ANSI_ARGS_((ClientData clientData, int mask));

/*
 * The following definitions and the string_to_type() function
 * are taken from the tkined source. These may need some update
 * if the tkined supports new object types.
 */

#define TKINED_NONE         0
#define TKINED_ALL          1
#define TKINED_NODE         2
#define TKINED_GROUP        3
#define TKINED_NETWORK      4
#define TKINED_LINK         5
#define TKINED_TEXT         6
#define TKINED_IMAGE        7
#define TKINED_INTERPRETER  8
#define TKINED_MENU         9
#define TKINED_LOG         10
#define TKINED_REFERENCE   11
#define TKINED_STRIPCHART  12
#define TKINED_BARCHART    13
#define TKINED_GRAPH	   14
#define TKINED_HTML	   15
#define TKINED_DATA	   16
#define TKINED_EVENT	   17

static int 
string_to_type (str)
    char *str;
{
    int type = TKINED_NONE;

    if (str != NULL) {
	if      (strcmp(str, "NODE") == 0)        type = TKINED_NODE;
	else if (strcmp(str, "GROUP") == 0)       type = TKINED_GROUP;
	else if (strcmp(str, "NETWORK") == 0)     type = TKINED_NETWORK;
	else if (strcmp(str, "LINK") == 0)        type = TKINED_LINK;
	else if (strcmp(str, "TEXT") == 0)        type = TKINED_TEXT;
	else if (strcmp(str, "IMAGE") == 0)       type = TKINED_IMAGE;
	else if (strcmp(str, "INTERPRETER") == 0) type = TKINED_INTERPRETER;
	else if (strcmp(str, "MENU") == 0)        type = TKINED_MENU;
	else if (strcmp(str, "LOG") == 0)         type = TKINED_LOG;
	else if (strcmp(str, "REFERENCE") == 0)   type = TKINED_REFERENCE;
	else if (strcmp(str, "STRIPCHART") == 0)  type = TKINED_STRIPCHART;
	else if (strcmp(str, "BARCHART") == 0)    type = TKINED_BARCHART;
	else if (strcmp(str, "GRAPH") == 0)       type = TKINED_GRAPH;
	else if (strcmp(str, "HTML") == 0)        type = TKINED_HTML;
	else if (strcmp(str, "DATA") == 0)        type = TKINED_DATA;
	else if (strcmp(str, "EVENT") == 0)       type = TKINED_EVENT;
   }

    return type;
}
/*
 * This wrapper for write() is needed to fix broken SYS V semantics.
 */

static int
xwrite (fd, buf, len)
    int fd;
    char *buf;
    int len;
{
    int rc;

    do {
	while ((rc = write (fd, buf, len)) < 0
	       && (errno == EINTR || errno == EAGAIN))
		continue;
	len -= rc;
	buf += rc;
    } while ((len > 0) && (rc > 0));

    return rc;
}

/*
 * This wrapper for read() is needed to fix broken SYS V semantics.
 */

static int 
xread (fd, buf, len)
    int fd;
    char *buf;
    int len;
{
    int rc;

    while ((rc = read (fd, buf, len)) < 0
	   && (errno == EINTR || errno == EAGAIN))
	    continue;

    return rc;
}

/*
 * Initialize the ined module. This is called when the first ined
 * command is evaluated.
 */

static void
InedInitialize (interp)
     Tcl_Interp *interp;
{
    char *path, *tmp, *p;

    /*
     * Make stdin unbuffered and register a file handler to receive
     * commands from the tkined editor. Make sure that an already
     * existing file handler is removed first (make wish happy).
     */
    
    setbuf (stdin, NULL);
    Tk_DeleteFileHandler (0);
    Tk_CreateFileHandler (0, TK_READABLE, InedReceiveProc, interp);
    InedFlushQueue (interp);
    
    /* 
     * Adjust the auto_path to take care of the environment variable
     * TKINED_PATH, $HOME/.tkined and the default Tkined library.
     */
    
    path = Tcl_GetVar (interp, "auto_path", TCL_GLOBAL_ONLY);
    if (path) {
	path = ckstrdup (path);
    }
    
    Tcl_SetVar (interp, "auto_path", "", TCL_GLOBAL_ONLY);
    
    if ((p = getenv ("TKINED_PATH"))) {
	tmp = ckstrdup (p);
	for (p = tmp; *p; p++) {
	    if (*p == ':') {
		*p = ' ';
	    }
	}
	Tcl_SetVar (interp, "auto_path", tmp, TCL_GLOBAL_ONLY);
	ckfree (tmp);
    }
    
    if ((p = getenv("HOME"))) {
	tmp = ckalloc (strlen (p) + 20);
	sprintf (tmp, "%s/.tkined", p);
	Tcl_SetVar (interp, "auto_path", tmp, 
		    TCL_APPEND_VALUE | TCL_LIST_ELEMENT | TCL_GLOBAL_ONLY);
	ckfree (tmp);
    }
    
    tmp = ckalloc (strlen (TKINEDLIB) + 20);
    sprintf (tmp, "%s/site", TKINEDLIB);
    Tcl_SetVar (interp, "auto_path", tmp,
		TCL_APPEND_VALUE| TCL_LIST_ELEMENT | TCL_GLOBAL_ONLY);
    sprintf (tmp, "%s/apps", TKINEDLIB);
    Tcl_SetVar (interp, "auto_path", tmp,
		TCL_APPEND_VALUE| TCL_LIST_ELEMENT | TCL_GLOBAL_ONLY);
    Tcl_SetVar (interp, "auto_path", TKINEDLIB, 
		TCL_APPEND_VALUE | TCL_LIST_ELEMENT | TCL_GLOBAL_ONLY);
    ckfree (tmp);
    
    if (path) {
	Tcl_SetVar (interp, "auto_path", " ",
		    TCL_APPEND_VALUE | TCL_GLOBAL_ONLY);
	Tcl_SetVar (interp, "auto_path", path, 
		    TCL_APPEND_VALUE | TCL_GLOBAL_ONLY);
	ckfree (path);
    }
}

/*
 * Handle an error when talking to the tkined editor.
 */

static void
InedFatal()
{
    fputs ("lost connection -- shuting down interpreter\n", stderr);
    exit (1);
}

/*
 * Write a queue message to the tkined editor. No acknowledge is
 * expected. a queue message is used to let tkined know how busy
 * we are.
 */

static void
InedQueue ()
{
    int len = 0;
    q_elem *p;
    char buf[256];

    for (p = queue; p != NULL; p = p->next) len++;

    sprintf (buf, "ined queue %d\n", len);
    
    len = strlen (buf);
    if (xwrite (fileno(stdout), buf, len) < 0) {
	fprintf (stderr, "ignoring ined queue message\n");
    }
}

/*
 * This is the callback invoked by the event loop to flush the ined
 * queue. It just calls InedFlushQueue to do the job.
 */

static void
InedFlushProc (clientData)
     ClientData clientData;
{
    Tcl_Interp *interp = (Tcl_Interp *) clientData;
    InedFlushQueue (interp);
}

/*
 * Process the commands stored in the queue.
 */

static void
InedFlushQueue (interp)
     Tcl_Interp *interp;
{
    q_elem *p;

    if (queue == NULL) return;

    InedQueue ();
    while ((p = queue) != NULL) {
	queue = queue->next;

	if (Tcl_GlobalEval (interp, p->msg) != TCL_OK) {
	    Tk_BackgroundError (interp);
	}

	ckfree (p->msg);
	ckfree ((char *) p);
    }

    InedQueue ();
}

/*
 * Append a message to the queue.
 */

static void
InedAppendQueue (msg)
    char *msg;
{
    q_elem *np;
    q_elem *p;

    if (msg == (char *) NULL) return;

    np = (q_elem *) ckalloc (sizeof(q_elem));
    np->msg = msg;
    np->next = (q_elem *) NULL;

    if (queue == (q_elem *) NULL) {
        queue = np;
        return;
    }

    for (p = queue; (p->next != (q_elem *) NULL); p = p->next) ;
    p->next = np;

    InedQueue ();
}

/*
 * Read a message from the Tkined editor. Returns the complete message
 * in a malloced buffer. The caller must free this buffer.
 * because chunks are read, anything behind a newline is stored and 
 * returned with the next call, only this routine must read from stdin.
 */

static char*
InedGets ()
{
    char *p;
    char ch = '\0';
    int buffer_size = BUF_INCR;
    char *buffer = ckalloc (buffer_size);
#define IN_SIZ	256
    static char in_buf [IN_SIZ];
    static char *in_ptr = in_buf;
    static int in_len = 0;

    buffer[0] = ch;
    p = buffer;

    for (;;) {
	  if (in_len <= 0) {
	      /* read next chunk: */
	      in_len = xread (fileno(stdin), in_ptr = in_buf, IN_SIZ);
	      if (in_len <= 0) {
		  ckfree (buffer);
		  return (char *) NULL;
	      }
	  }

	  while (in_len-- > 0 && (ch = *in_ptr++) != '\n') {
	      *p = ch;
	      p++;
	      if (p-buffer == buffer_size) {
		  int i = p-buffer;
		  buffer_size += BUF_INCR;
		  buffer = ckrealloc (buffer, buffer_size);
		  p = buffer + i;
	      }
	  }
	  if (ch == '\n') {
	      *p = '\0';
	      break;
	  }
    }
    
    if (ch != '\n') {
        ckfree (buffer);
        return (char *) NULL;
    }

    return buffer;
}

/*
 * Check if we can evaluate cmd on the external object 
 * representation given in largv.
 */

static int
InedCompCmd (cmd, interp, argc, argv)
    char *cmd;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    int type = string_to_type (argv[0]);
    if ((type == TKINED_NONE) || (type == TKINED_ALL)) return TCL_ERROR;

    if ((strcmp (cmd, "type") == 0) && (argc > 0)) {
	Tcl_SetResult (interp, argv[0], TCL_VOLATILE);
	return TCL_OK;

    } else if ((strcmp (cmd, "id") == 0) && (argc > 1)) {
	Tcl_SetResult (interp, argv[1], TCL_VOLATILE);
	return TCL_OK;

    } else if ((strcmp (cmd, "name") == 0) && (argc > 2)) {
        if ((type == TKINED_NODE) || (type == TKINED_NETWORK)
	    || (type == TKINED_BARCHART) || (type == TKINED_STRIPCHART)
	    || (type == TKINED_GROUP) || (type == TKINED_REFERENCE)
	    || (type == TKINED_MENU) || (type == TKINED_LOG) 
	    || (type == TKINED_GRAPH) || (type == TKINED_HTML)
	    || (type == TKINED_DATA) || (type == TKINED_EVENT) )
	    Tcl_SetResult (interp, argv[2], TCL_VOLATILE);
        return TCL_OK;

    } else if ((strcmp (cmd, "address") == 0) && (argc > 3)) {
        if ((type == TKINED_NODE) || (type == TKINED_NETWORK)
	    || (type == TKINED_BARCHART) || (type == TKINED_STRIPCHART)
	    || (type == TKINED_REFERENCE) || (type == TKINED_GRAPH)
	    || (type == TKINED_DATA))
	    Tcl_SetResult (interp, argv[3], TCL_VOLATILE);
        return TCL_OK;

    } else if (strcmp (cmd, "oid") == 0) {
        if ((type == TKINED_GROUP) && (argc > 3)) {
	    Tcl_SetResult (interp, argv[3], TCL_VOLATILE);
	}
        if ((type == TKINED_NODE || type == TKINED_NETWORK) && (argc > 4)) {
	    Tcl_SetResult (interp, argv[4], TCL_VOLATILE);
	}
	return TCL_OK;

    } else if ((strcmp (cmd, "links") == 0) && (argc > 5)) {
        if ((type == TKINED_NODE) || (type == TKINED_NETWORK))
	    Tcl_SetResult (interp, argv[5], TCL_VOLATILE);
        return TCL_OK;

    } else if ((strcmp (cmd, "member") == 0) && (argc > 4)) {
        if (type == TKINED_GROUP)
	    Tcl_SetResult (interp, argv[4], TCL_VOLATILE);
        return TCL_OK;

    } else if ((strcmp (cmd, "src") == 0) && (argc > 2)) {
        if (type == TKINED_LINK)
	    Tcl_SetResult (interp, argv[2], TCL_VOLATILE);
        return TCL_OK;

    } else if ((strcmp (cmd, "dst") == 0) && (argc > 3)) {
        if (type == TKINED_LINK)
	    Tcl_SetResult (interp, argv[3], TCL_VOLATILE);
        return TCL_OK;

    } else if ((strcmp (cmd, "text") == 0) && (argc > 2)) {
        if (type == TKINED_LINK)
	    Tcl_SetResult (interp, argv[2], TCL_VOLATILE);
        return TCL_OK;

    }

    return TCL_ERROR;
}

/*
 * This is the ined command as described in the tkined documentation.
 * It just sends the command to the tkined editor and waits for an
 * acknowledge containing the answer or the error description.
 *
 * Everything received while waiting for the acknowledge is queued
 * for later execution.
 */

int
Scotty_InedCmd (clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    int i;
    char *p;
    static int initialized = 0;

    if (! initialized) {
	InedInitialize (interp);
	initialized = 1;
    }

    if (argc < 2) {
	Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			  " command ?arg ...?\"", (char *) NULL);
	return TCL_ERROR;
    }

    /* the loop command is used by programs that do not wish */
    /* to use their own main or event loop mechanism */

    if ((argc == 2) && (strcmp (argv[1], "loop") == 0)) {
	char *line;
	InedFlushQueue (interp);
	while ((line = InedGets()) != (char *) NULL) {
	    InedAppendQueue (line);
	    InedFlushQueue (interp);
	}
	return TCL_OK;
    }

    /* check for commands that act on external object representations */

    if (argc == 3) {
        int largc;
	char **largv;
        int rc = Tcl_SplitList (interp, argv[2], &largc, &largv);

	if (rc == TCL_OK) {
	    if (InedCompCmd (argv[1], interp, largc, largv) == TCL_OK) {
		ckfree ((char *) largv);
		return TCL_OK;
	    }
 	    ckfree ((char *) largv);
	}
    }

    for (i = 0; i < argc; i++) {
	if (fputc ('{', stdout) == EOF) InedFatal ();
        for (p = argv[i]; *p; p++) {
	    if (*p != '\n') {
	        if (fputc (*p, stdout) == EOF) InedFatal ();
	    } else {
	        if (fputs ("\\n", stdout) == EOF) InedFatal ();
	    }
	}
        if (fputs ("} ", stdout) == EOF) InedFatal ();
    }
    if (fputc ('\n', stdout) == EOF) InedFatal ();
    fflush (stdout);

    while ((p = InedGets ()) != (char *) NULL) {
        if (*p == '\0') continue;
	if (strncmp (p, "ined ok", 7) == 0) {
	    char *r = p+7;
	    while (*r && isspace(*r)) r++;
	    Tcl_SetResult(interp, r, TCL_VOLATILE);
	    ckfree (p);
	    return TCL_OK;
	} else if (strncmp (p, "ined error", 10) == 0) {
	    char *r = p+10;
	    while (*r && isspace(*r)) r++;
	    Tcl_SetResult(interp, r, TCL_VOLATILE);
	    ckfree (p);
	    return TCL_ERROR;
	} else {
	    InedAppendQueue (p);
	    Tk_CreateTimerHandler (0, InedFlushProc, (ClientData) interp);
	}
    }

    InedFatal ();

    return TCL_ERROR;
}

/*
 * InedReceiveProc() is the called from the event handler whenever
 * a command can be read from stdin.
 */

static void
InedReceiveProc (clientData, mask)
    ClientData clientData;
    int mask;
{
    Tcl_Interp *interp = (Tcl_Interp *) clientData;
    char *cmd = InedGets();

    if (cmd != NULL) {
        InedAppendQueue (cmd);
        InedFlushQueue (interp);
    } else {
	Tk_DeleteFileHandler (0);
    }
}
