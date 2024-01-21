/*
 * $Id: msqltcl.c,v 1.50 1995/06/06 14:25:25 hs Rel $
 *
 * MSQL interface to Tcl
 *
 * Hakan Soderstrom, hs@soderstrom.se
 *
 */

#include <scotty.h>

#ifdef HAVE_MSQL

/*
 * Copyright (c) 1994, 1995 Hakan Soderstrom and Tom Poindexter
 * 
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice and this permission notice
 * appear in all copies of the software and related documentation.
 * 
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
 *
 * IN NO EVENT SHALL HAKAN SODERSTROM OR SODERSTROM PROGRAMVARUVERKSTAD
 * AB BE LIABLE FOR ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL
 * DAMAGES OF ANY KIND, OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
 * OF USE, DATA OR PROFITS, WHETHER OR NOT ADVISED OF THE POSSIBILITY
 * OF DAMAGE, AND ON ANY THEORY OF LIABILITY, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include <tcl.h>
#include <msql.h>

#include <errno.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>


#define MSQL_HANDLES      15	/* Default number of handles available. */
#define MSQL_BUFF_SIZE	1024	/* Conversion buffer size for various needs. */
#define MSQL_SMALL_SIZE  TCL_RESULT_SIZE /* Smaller buffer size. */
#define MSQL_NAME_LEN     80    /* Max. host, database name length. */

typedef struct MsqlTclHandle {
  int connection ;         /* Connection handle, if connected; -1 otherwise. */
  char host[MSQL_NAME_LEN] ;      /* Host name, if connected. */
  char database[MSQL_NAME_LEN] ;  /* Db name, if selected; NULL otherwise. */
  m_result* result ;              /* Stored result, if any; NULL otherwise. */
  int res_count ;                 /* Count of unfetched rows in result. */
  int col_count ;                 /* Column count in result, if any. */
} MsqlTclHandle;

static MsqlTclHandle   MsqlHandle[MSQL_HANDLES];  

static char *MsqlHandlePrefix = "msql";
/* Prefix string used to identify handles.
 * The following must be strlen(MsqlHandlePrefix).
 */
#define MSQL_HPREFIX_LEN 4

/* Array for status info, and its elements. */
static char *MsqlStatusArr = "msqlstatus";
#define MSQL_STATUS_CODE "code"
#define MSQL_STATUS_CMD  "command"
#define MSQL_STATUS_MSG  "message"
#define MSQL_STATUS_NULLV  "nullvalue"

/* C variable corresponding to msqlstatus(nullvalue) */
static char* MsqlNullvalue = NULL ;
#define MSQL_NULLV_INIT ""

/* Options to the 'info', 'result', 'col' combo commands. */
     
static char* MsqlDbOpt[] =
{
  "dbname", "dbname?", "tables", "host", "host?", "databases"
};
#define MSQL_INFNAME_OPT 0
#define MSQL_INFNAMEQ_OPT 1
#define MSQL_INFTABLES_OPT 2
#define MSQL_INFHOST_OPT 3
#define MSQL_INFHOSTQ_OPT 4
#define MSQL_INFLIST_OPT 5

#define MSQL_INF_OPT_MAX 5

static char* MsqlResultOpt[] =
{
  "rows", "rows?", "cols", "cols?", "current", "current?"
};
#define MSQL_RESROWS_OPT 0
#define MSQL_RESROWSQ_OPT 1
#define MSQL_RESCOLS_OPT 2
#define MSQL_RESCOLSQ_OPT 3
#define MSQL_RESCUR_OPT 4
#define MSQL_RESCURQ_OPT 5

#define MSQL_RES_OPT_MAX 5

/* Column info definitions. */

static char* MsqlColkey[] =
{
  "table", "name", "type", "length", "prim_key", "non_null"
};

#define MSQL_COL_TABLE_K 0
#define MSQL_COL_NAME_K 1
#define MSQL_COL_TYPE_K 2
#define MSQL_COL_LENGTH_K 3
#define MSQL_COL_PRIMKEY_K 4
#define MSQL_COL_NONNULL_K 5

#define MSQL_COL_K_MAX 5

/* Macro for checking handle syntax; arguments:
 * 'h' must point to the handle.
 * 'H' must be an int variable.
 * RETURNS the handle index; or -1 on syntax conflict.
 * SIDE EFFECT: 'H' will contain the handle index on success.
 * 'h' will point to the first digit on success.
 * ASSUMES constant MSQL_HANDLES >= 10.
 */
#define HSYNTAX(h,H) \
(((strncmp(h,MsqlHandlePrefix,MSQL_HPREFIX_LEN) == 0) && \
(h+=MSQL_HPREFIX_LEN) && isdigit(h[0]))? \
 (((H=h[0]-'0')+1 && h[1]=='\0')?H: \
  ((isdigit(h[1]) && h[2]=='\0' && (H=10*H+h[1]-'0')<MSQL_HANDLES)?H:-1)) \
:-1)


/* Prototypes for all functions. */

extern Tcl_CmdProc  Msqltcl_Connect;
extern Tcl_CmdProc  Msqltcl_Use;
extern Tcl_CmdProc  Msqltcl_Sel;
extern Tcl_CmdProc  Msqltcl_Next;
extern Tcl_CmdProc  Msqltcl_Seek;
extern Tcl_CmdProc  Msqltcl_Map;
extern Tcl_CmdProc  Msqltcl_Exec;
extern Tcl_CmdProc  Msqltcl_Close;
extern Tcl_CmdProc  Msqltcl_Info;
extern Tcl_CmdProc  Msqltcl_Result;
extern Tcl_CmdProc  Msqltcl_Col;
extern Tcl_CmdProc  Msqltcl_State;

  
/* CONFLICT HANDLING
 *
 * Every command begins by calling 'msql_prologue'.
 * This function resets msqlstatus(code) to zero; the other array elements
 * retain their previous values.
 * The function also saves argc/argv in global variables.
 * After this the command processing proper begins.
 *
 * If there is a conflict, the message is taken from one of the following
 * sources,
 * -- this code (msql_prim_confl),
 * -- the database server (msql_server_confl),
 * -- a POSIX (system call) diagnostic.
 * A complete message is put together from the above plus the name of the
 * command where the conflict was detected.
 * The complete message is returned as the Tcl result and is also stored in
 * msqlstatus(message).
 * msqlstatus(code) is set to "-1", except for POSIX conflicts where 'errno'
 * is used.
 * In addition, the whole command where the conflict was detected is put
 * together from the saved argc/argv and is copied into msqlstatus(command).
 */

static Tcl_Interp* saved_interp;
static int saved_argc;
static char** saved_argv;


/*
 *----------------------------------------------------------------------
 * msql_reassemble
 * Reassembles the current command from the saved argv; copies it into
 * msqlstatus(command).
 */

static void
msql_reassemble ()
{
  unsigned int flags = TCL_GLOBAL_ONLY | TCL_LIST_ELEMENT;
  int idx ;

  for (idx = 0; idx < saved_argc; ++idx)
    {
      Tcl_SetVar2 (saved_interp, MsqlStatusArr, MSQL_STATUS_CMD,
		   saved_argv[idx], flags) ;
      flags |= TCL_APPEND_VALUE ;
    }
}


/*
 *----------------------------------------------------------------------
 * msql_posix_confl
 * Conflict handling after a failed system call.
 */

static int
msql_posix_confl (syscall)
     char* syscall;
{
  char buf[MSQL_SMALL_SIZE];
  char* strerror () ;
  char* msg = strerror (errno) ;

  sprintf(buf, "%d", errno) ;
  Tcl_SetVar2 (saved_interp, MsqlStatusArr, MSQL_STATUS_CODE, buf,
	       TCL_GLOBAL_ONLY);
  Tcl_SetResult (saved_interp, "", TCL_STATIC) ;
  Tcl_AppendResult (saved_interp, saved_argv[0], "/POSIX: (", syscall,
		    ") ", (msg == NULL) ? "" : msg, (char*)NULL);
  Tcl_SetVar2 (saved_interp, MsqlStatusArr, MSQL_STATUS_MSG,
	       saved_interp->result, TCL_GLOBAL_ONLY);
  msql_reassemble () ;
  return TCL_ERROR ;
}


/*
 *----------------------------------------------------------------------
 * msql_prim_confl
 * Conflict handling after a primitive conflict.
 *
 */

static int
msql_prim_confl (msg)
     char* msg ;
{
  Tcl_SetVar2 (saved_interp, MsqlStatusArr, MSQL_STATUS_CODE, "-1",
	       TCL_GLOBAL_ONLY);
  Tcl_SetResult (saved_interp, "", TCL_STATIC) ;
  Tcl_AppendResult (saved_interp, saved_argv[0], ": ", msg, (char*)NULL) ;
  Tcl_SetVar2 (saved_interp, MsqlStatusArr, MSQL_STATUS_MSG,
	       saved_interp->result, TCL_GLOBAL_ONLY);
  msql_reassemble () ;
  return TCL_ERROR ;
}


/*
 *----------------------------------------------------------------------
 * msql_server_confl
 * Conflict handling after an mSQL conflict.
 *
 */

static int
msql_server_confl ()
{
  Tcl_SetVar2 (saved_interp, MsqlStatusArr, MSQL_STATUS_CODE, "-1",
	       TCL_GLOBAL_ONLY);
  Tcl_SetResult (saved_interp, "", TCL_STATIC) ;
  Tcl_AppendResult (saved_interp, saved_argv[0], "/db server: ",
		    (msqlErrMsg == NULL) ? "" : msqlErrMsg, (char*)NULL) ;
  Tcl_SetVar2 (saved_interp, MsqlStatusArr, MSQL_STATUS_MSG,
	       saved_interp->result, TCL_GLOBAL_ONLY);
  msql_reassemble () ;
  return TCL_ERROR ;
}


/*----------------------------------------------------------------------
 * get_handle_plain
 * Check handle syntax (and nothing else).
 * RETURN: MsqlHandle index number or -1 on error.
 */
static int
get_handle_plain (handle) 
     char *handle;
{
  int hi ;
  char* hp = handle ;
  
  if (HSYNTAX(hp,hi) < 0)
    {
      msql_prim_confl ("weird handle") ; /*  */
      return -1 ;
    }
  else
    return hi ;
}


/*----------------------------------------------------------------------
 * get_handle_conn
 * Check handle syntax, verify that the handle is connected.
 * RETURN: MsqlHandle index number or -1 on error.
 */
static int
get_handle_conn (handle) 
     char *handle;
{
  int hi ;
  char* hp = handle ;

  if (HSYNTAX(hp,hi) < 0)
    {
      msql_prim_confl ("weird handle") ;
      return -1 ;
    }

  if (MsqlHandle[hi].connection < 0)
    {
      msql_prim_confl ("handle not connected") ;
      return -1 ;
    }
  else
    return hi ;
}


/*----------------------------------------------------------------------
 * get_handle_db
 * Check handle syntax, verify that the handle is connected and that
 * there is a current database.
 * RETURN: MsqlHandle index number or -1 on error.
 */
static int
get_handle_db (handle) 
     char *handle;
{
  int hi ;
  char* hp = handle ;

  if (HSYNTAX(hp,hi) < 0)
    {
      msql_prim_confl ("weird handle") ;
      return -1 ;
    }

  if (MsqlHandle[hi].connection < 0)
    {
      msql_prim_confl ("handle not connected") ;
      return -1 ;
    }

  if (MsqlHandle[hi].database[0] == '\0')
    {
      msql_prim_confl ("no current database") ;
      return -1 ;
    }
  else
    return hi ;
}


/*----------------------------------------------------------------------
 * get_handle_res
 * Check handle syntax, verify that the handle is connected and that
 * there is a current database and that there is a pending result.
 * RETURN: MsqlHandle index number or -1 on error.
 */

static int
get_handle_res (handle) 
     char *handle;
{
  int hi ;
  char* hp = handle ;

  if (HSYNTAX(hp,hi) < 0)
    {
      msql_prim_confl ("weird handle") ;
      return -1 ;
    }

  if (MsqlHandle[hi].connection < 0)
    {
      msql_prim_confl ("handle not connected") ;
      return -1 ;
    }

  if (MsqlHandle[hi].database[0] == '\0')
    {
      msql_prim_confl ("no current database") ;
      return -1 ;
    }

  if (MsqlHandle[hi].result == NULL)
    {
      msql_prim_confl ("no result pending") ; /*  */
      return -1 ;
    }
  else
    return hi ;
}


/* 
 *----------------------------------------------------------------------
 * handle_init
 * Initialize the handle array.
 */
static void 
handle_init () 
{
  int i ;

  for (i = 0; i < MSQL_HANDLES; i++) {
    MsqlHandle[i].connection = -1 ;
    MsqlHandle[i].host[0] = '\0' ;
    MsqlHandle[i].database[0] = '\0' ;
    MsqlHandle[i].result = NULL ;
    MsqlHandle[i].res_count = 0 ;
    MsqlHandle[i].col_count = 0 ;
  }
}


/*
 *----------------------------------------------------------------------
 * clear_msg
 *
 * Clears all error and message elements in the global array variable.
 *
 */

static void
clear_msg(interp)
    Tcl_Interp *interp;
{
    Tcl_SetVar2(interp, MsqlStatusArr, MSQL_STATUS_CODE, "0", TCL_GLOBAL_ONLY);
    Tcl_SetVar2(interp, MsqlStatusArr, MSQL_STATUS_CMD, "", TCL_GLOBAL_ONLY);
    Tcl_SetVar2(interp, MsqlStatusArr, MSQL_STATUS_MSG, "", TCL_GLOBAL_ONLY);
}


/*
 *----------------------------------------------------------------------
 * msql_prologue
 *
 * Does most of standard command prologue; required for all commands
 * having conflict handling.
 * 'req_args' must be the required number of arguments for the command,
 * including the command word.
 * 'usage_msg' must be a usage message, leaving out the command name.
 * Checks the handle assumed to be present in argv[1] if 'check' is not NULL.
 * RETURNS: Handle index or -1 on failure.
 * Returns zero if 'check' is NULL.
 * SIDE EFFECT: Sets the Tcl result on failure.
 */

static int
msql_prologue (interp, argc, argv, req_args, check, usage_msg)
     Tcl_Interp *interp;
     int         argc;
     char      **argv;
     int         req_args;
     char       *usage_msg;
     int (*check) () ; /* Pointer to function for checking the handle. */
{
  char buf[MSQL_BUFF_SIZE];
  int hand = 0;
  int need;

  /* Reset msqlstatus(code). */
  Tcl_SetVar2 (interp, MsqlStatusArr, MSQL_STATUS_CODE, "0",
	       TCL_GLOBAL_ONLY);

  /* Save command environment. */
  saved_interp = interp;
  saved_argc = argc ;
  saved_argv = argv ;

  /* Check number of minimum args. */
  if ((need = req_args - argc) > 0) 
    {
      sprintf (buf, "%d more %s needed: %s %s", need, (need>1)?"args":"arg",
	       argv[0], usage_msg);
      (void)msql_prim_confl (buf) ;
      return -1 ;
    }

  /* Check the handle.
   * The function is assumed to set the status array on conflict.
   */
  if (check != NULL && (hand = check (argv[1])) < 0)
    return -1 ;

  return (hand);
}


/*
 *----------------------------------------------------------------------
 * msql_colinfo
 *
 * Given an m_field struct and a string keyword appends a piece of
 * column info (one item) to the Tcl result.
 * ASSUMES 'fld' is non-null.
 * RETURNS 0 on success, 1 otherwise.
 * SIDE EFFECT: Sets the result and status on failure.
 */

static int
msql_colinfo (interp, fld, keyw)
     Tcl_Interp  *interp;
     m_field* fld ;
     char* keyw ;
{
  char buf[MSQL_SMALL_SIZE];
  char keybuf[MSQL_SMALL_SIZE];
  int idx ;
  char* res ;
  int retcode ;
  
  for (idx = 0;
       idx <= MSQL_COL_K_MAX && strcmp (MsqlColkey[idx], keyw) != 0;
       idx++) ;

  switch (idx)
    {
    case MSQL_COL_TABLE_K:
      res = fld->table ;
      break ;
    case MSQL_COL_NAME_K:
      res = fld->name ;
      break ;
    case MSQL_COL_TYPE_K:
      switch (fld->type)
	{
	case INT_TYPE:
	  res = "int" ;
	  break ;
	case CHAR_TYPE:
	  res = "char" ;
	  break ;
	case REAL_TYPE:
	  res = "real" ;
	  break ;
	default:
	  sprintf (buf, "column '%s' has weird datatype", fld->name) ;
	  res = NULL ;
	}
      break ;
    case MSQL_COL_LENGTH_K:
      sprintf (buf, "%d", fld->length) ;
      res = buf ;
      break ;
    case MSQL_COL_PRIMKEY_K:
      sprintf (buf, "%c", (IS_PRI_KEY(fld->flags))?'1':'0') ;
      res = buf ;
      break ;
    case MSQL_COL_NONNULL_K:
      sprintf (buf, "%c", (IS_NOT_NULL(fld->flags))?'1':'0') ;
      res = buf ;
      break ;
    default:
      if (strlen (keyw) >= MSQL_NAME_LEN)
	{
	  strncpy (keybuf, keyw, MSQL_NAME_LEN) ;
	  strcat (keybuf, "...") ;
	}
      else
	strcpy (keybuf, keyw) ;

      sprintf (buf, "unknown option: %s", keybuf) ;
      res = NULL ;
    }

  if (res == NULL)
    {
      (void)msql_prim_confl (buf) ;
      retcode = 1 ;
    }
  else
    {
      Tcl_AppendElement (interp, res) ;
      retcode = 0 ;
    }

  return retcode ;
}


/*
 *----------------------------------------------------------------------
 * Msqltcl_Kill
 * Close all connections.
 *
 */

void
Msqltcl_Kill (clientData)
    ClientData clientData;
{
  int i ;

  for (i = 0; i < MSQL_HANDLES; i++)
    {
      if (MsqlHandle[i].connection >=0)
	msqlClose (MsqlHandle[i].connection) ;
    }
  
  handle_init () ;
}


/*
 *----------------------------------------------------------------------
 * Msqltcl_Init
 * Perform all initialization for the MSQL to Tcl interface.
 * Adds additional commands to interp, creates message array, initializes
 * all handles.
 *
 * A call to Msqltcl_Init should exist in Tcl_CreateInterp or
 * Tcl_CreateExtendedInterp.
 */

int
Msqltcl_Init (interp)
    Tcl_Interp *interp;
{
  char nbuf[MSQL_SMALL_SIZE];

  /*
   * Initialize mSQL proc structures 
   */
  handle_init () ;

  /*
   * Initialize the new Tcl commands.
   * Deleting any command will close all connections.
   */
  Tcl_CreateCommand (interp, "msqlconnect", Msqltcl_Connect, (ClientData)NULL,
		     Msqltcl_Kill);
  Tcl_CreateCommand (interp, "msqluse",     Msqltcl_Use,     (ClientData)NULL,
		     Msqltcl_Kill);
  Tcl_CreateCommand (interp, "msqlsel",     Msqltcl_Sel,     (ClientData)NULL,
		     Msqltcl_Kill);
  Tcl_CreateCommand (interp, "msqlnext",    Msqltcl_Next,    (ClientData)NULL,
		     Msqltcl_Kill);
  Tcl_CreateCommand (interp, "msqlseek",    Msqltcl_Seek,    (ClientData)NULL,
                     Msqltcl_Kill);
  Tcl_CreateCommand (interp, "msqlmap",     Msqltcl_Map,     (ClientData)NULL,
		     Msqltcl_Kill);
  Tcl_CreateCommand (interp, "msqlexec",    Msqltcl_Exec,    (ClientData)NULL,
		     Msqltcl_Kill);
  Tcl_CreateCommand (interp, "msqlclose",   Msqltcl_Close,   (ClientData)NULL,
		     Msqltcl_Kill);
  Tcl_CreateCommand (interp, "msqlinfo",    Msqltcl_Info,    (ClientData)NULL,
		     Msqltcl_Kill);
  Tcl_CreateCommand (interp, "msqlresult",  Msqltcl_Result,  (ClientData)NULL,
		     Msqltcl_Kill);
  Tcl_CreateCommand (interp, "msqlcol",     Msqltcl_Col,     (ClientData)NULL,
		     Msqltcl_Kill);
  Tcl_CreateCommand (interp, "msqlstate",   Msqltcl_State,   (ClientData)NULL,
		     Msqltcl_Kill);

  /* Initialize msqlstatus global array. */
  clear_msg(interp);

  /* Link the null value element to the corresponding C variable. */
  if ((MsqlNullvalue = (char*) ckalloc (12)) == NULL)
    {
      fprintf (stderr, "*** msqltcl: out of memory\n") ;
      return TCL_ERROR ;
    }
  (void)strcpy (MsqlNullvalue, MSQL_NULLV_INIT);
  (void)sprintf (nbuf, "%s(%s)", MsqlStatusArr, MSQL_STATUS_NULLV) ;
  Tcl_LinkVar (interp, nbuf, (char*)&MsqlNullvalue, TCL_LINK_STRING) ;

  /* A little sanity check.
   * If this message appears you must change the source code and recompile.
   */
  if (strlen (MsqlHandlePrefix) == MSQL_HPREFIX_LEN)
    return TCL_OK;
  else
    {
      fprintf (stderr, "*** msqltcl (msqltcl.c): handle prefix inconsistency!\n") ;
      return TCL_ERROR ;
    }
}


/*
 *----------------------------------------------------------------------
 *
 * Msqltcl_Connect
 * Implements the msqlconnect command:
 * usage: msqlconnect ?server-host?
 *	                
 * Results:
 *      handle - a character string of newly open handle
 *      TCL_OK - connect successful
 *      TCL_ERROR - connect not successful - error message returned
 */

int
Msqltcl_Connect (clientData, interp, argc, argv)
     ClientData   clientData;
     Tcl_Interp  *interp;
     int          argc;
     char       **argv;
{
  int        hand = -1;
  int        i;
  char       buf[MSQL_BUFF_SIZE];
  int connect ;

  /* Pro-forma check (should never fail). */
  if (msql_prologue (interp, argc, argv, 1, NULL, "?hostname?") < 0)
    return TCL_ERROR;

  /* Find an unused handle. */
  for (i = 0; i < MSQL_HANDLES; i++) {
    if (MsqlHandle[i].connection <= 0) {
      hand = i;
      break;
    }
  }

  if (hand == -1)
    return msql_prim_confl ("no mSQL handles available");

  if (argc > 1 && strlen (argv[1]) > 0)
    {
      connect = msqlConnect (argv[1]) ;
      strncpy (MsqlHandle[hand].host, argv[1], MSQL_NAME_LEN) ;
      MsqlHandle[hand].host[MSQL_NAME_LEN - 1] = '\0' ;
    }
  else
    {
      if (gethostname (buf, MSQL_NAME_LEN) == 0)
	{
	  connect = msqlConnect ((char*)NULL) ;
	  strcpy (MsqlHandle[hand].host, buf) ;
	}
      else
	return msql_posix_confl ("gethostname") ;
    }

  if (connect < 0)
    {
      MsqlHandle[hand].connection = -1 ; /* Just to be sure. */
      return msql_server_confl ();
    }
  else
    MsqlHandle[hand].connection = connect ;

  /* Construct handle and return. */
  sprintf(buf, "%s%d", MsqlHandlePrefix, hand);
  Tcl_SetResult(interp, buf, TCL_VOLATILE);

  return TCL_OK;
}


/*
 *----------------------------------------------------------------------
 *
 * Msqltcl_Use
 *    Implements the msqluse command:
 *    usage: msqluse handle dbname
 *	                
 *    results:
 *	Sets current database to dbname.
 */

int
Msqltcl_Use (clientData, interp, argc, argv)
     ClientData   clientData;
     Tcl_Interp  *interp;
     int          argc;
     char       **argv;
{
  int hand;
  
  if ((hand = msql_prologue(interp, argc, argv, 3, get_handle_conn,
			    "handle dbname")) < 0)
    return TCL_ERROR;

  if (strlen (argv[2]) >= MSQL_NAME_LEN)
    return msql_prim_confl ("database name too long") ;
  if (msqlSelectDB (MsqlHandle[hand].connection, argv[2]) < 0)
    return msql_server_confl () ;

  strcpy (MsqlHandle[hand].database, argv[2]) ;
  return TCL_OK;
}



/*
 *----------------------------------------------------------------------
 *
 * Msqltcl_Sel
 *    Implements the msqlsel command:
 *    usage: msqlsel handle sel-query
 *	                
 *    results:
 *
 *    SIDE EFFECT: Flushes any pending result, even in case of conflict.
 *    Stores new results.
 */

int
Msqltcl_Sel (clientData, interp, argc, argv)
     ClientData   clientData;
     Tcl_Interp  *interp;
     int          argc;
     char       **argv;
{
  int     hand;
  
  if ((hand = msql_prologue(interp, argc, argv, 3, get_handle_db,
			    "handle sel-query")) < 0)
    return TCL_ERROR;

  /* Flush any previous result. */
  if (MsqlHandle[hand].result != NULL)
    {
      msqlFreeResult (MsqlHandle[hand].result) ;
      MsqlHandle[hand].result = NULL ;
    }

  if (msqlQuery (MsqlHandle[hand].connection, argv[2]) < 0)
    return msql_server_confl () ;

  if ((MsqlHandle[hand].result = msqlStoreResult ()) == NULL)
    {
      (void)strcpy (interp->result, "-1") ;
    }
  else
    {
      MsqlHandle[hand].res_count = msqlNumRows (MsqlHandle[hand].result) ;
      MsqlHandle[hand].col_count = msqlNumFields (MsqlHandle[hand].result) ;
      (void)sprintf (interp->result, "%d", MsqlHandle[hand].res_count) ;
    }

  return TCL_OK;
}


/*
 *----------------------------------------------------------------------
 *
 * Msqltcl_Exec
 * Implements the msqlexec command:
 * usage: msqlexec handle sql-statement
 *	                
 * Results:
 *
 * SIDE EFFECT: Flushes any pending result, even in case of conflict.
 */

int
Msqltcl_Exec (clientData, interp, argc, argv)
     ClientData   clientData;
     Tcl_Interp  *interp;
     int          argc;
     char       **argv;
{
  int     hand;
  
  if ((hand = msql_prologue(interp, argc, argv, 3, get_handle_db,
			    "handle sql-statement")) < 0)
    return TCL_ERROR;

  /* Flush any previous result. */
  if (MsqlHandle[hand].result != NULL)
    {
      msqlFreeResult (MsqlHandle[hand].result) ;
      MsqlHandle[hand].result = NULL ;
    }

  if (msqlQuery (MsqlHandle[hand].connection, argv[2]) < 0)
    return msql_server_confl () ;

  return TCL_OK ;
}


/*
 *----------------------------------------------------------------------
 *
 * Msqltcl_Next
 *    Implements the msqlnext command:
 *    usage: msqlnext handle
 *	                
 *    results:
 *	next row from pending results as tcl list, or null list.
 */

int
Msqltcl_Next (clientData, interp, argc, argv)
     ClientData   clientData;
     Tcl_Interp  *interp;
     int          argc;
     char       **argv;
{
  int hand;
  int idx ;
  m_row row ;
  char* val ;
  
  if ((hand = msql_prologue(interp, argc, argv, 2, get_handle_res,
			    "handle")) < 0)
    return TCL_ERROR;

  
  if (MsqlHandle[hand].res_count == 0)
    return TCL_OK ;
  else if ((row = msqlFetchRow (MsqlHandle[hand].result)) == NULL)
    {
      MsqlHandle[hand].res_count = 0 ;
      return msql_prim_confl ("result counter out of sync") ;
    }
  else
    MsqlHandle[hand].res_count-- ;
  
  for (idx = 0 ; idx < MsqlHandle[hand].col_count ; idx++)
    {
      if ((val = *row++) == NULL)
	val = MsqlNullvalue ;
      Tcl_AppendElement (interp, val) ;
    }
  
  return TCL_OK;
}


/*
 *----------------------------------------------------------------------
 *
 * Msqltcl_Seek
 *    Implements the msqlseek command:
 *    usage: msqlseek handle rownumber
 *	                
 *    results:
 *	number of remaining rows
 */

int
Msqltcl_Seek (clientData, interp, argc, argv)
    ClientData   clientData;
    Tcl_Interp  *interp;
    int          argc;
    char       **argv;
{
    int hand;
    int res;
    int row;
    int total;
   
    if ((hand = msql_prologue(interp, argc, argv, 3, get_handle_res,
                              " handle row-index")) < 0)
      return TCL_ERROR;

    if ((res = Tcl_GetInt (interp, argv[2], &row)) != TCL_OK)
      return res;
    
    total = msqlNumRows (MsqlHandle[hand].result);
    
    if (total + row < 0)
      {
	msqlDataSeek (MsqlHandle[hand].result, 0);
	MsqlHandle[hand].res_count = total;
      }
    else if (row < 0)
      {
	msqlDataSeek (MsqlHandle[hand].result, total + row);
	MsqlHandle[hand].res_count = -row;
      }
    else if (row >= total)
      {
	msqlDataSeek (MsqlHandle[hand].result, row);
	MsqlHandle[hand].res_count = 0;
      }
    else
      {
	msqlDataSeek (MsqlHandle[hand].result, row);
	MsqlHandle[hand].res_count = total - row;
      }

    (void)sprintf (interp->result, "%d", MsqlHandle[hand].res_count) ;
    return TCL_OK;
}


/*
 *----------------------------------------------------------------------
 *
 * Msqltcl_Map
 * Implements the msqlmap command:
 * usage: msqlmap handle binding-list script
 *	                
 * Results:
 * SIDE EFFECT: For each row the column values are bound to the variables
 * in the binding list and the script is evaluated.
 * The variables are created in the current context.
 * NOTE: msqlmap works very much like a 'foreach' construct.
 * The 'continue' and 'break' commands may be used with their usual effect.
 */

int
Msqltcl_Map (clientData, interp, argc, argv)
     ClientData   clientData;
     Tcl_Interp  *interp;
     int          argc;
     char       **argv;
{
  int code ;
  int count ;
  int hand ;
  int idx ;
  int listArgc ;
  char** listArgv ;
  m_row row ;
  char* val ;
  
  if ((hand = msql_prologue(interp, argc, argv, 4, get_handle_res,
			    "handle binding-list script")) < 0)
    return TCL_ERROR;

  if (Tcl_SplitList (interp, argv[2], &listArgc, &listArgv) != TCL_OK)
    return TCL_ERROR ;
  
  if (listArgc > MsqlHandle[hand].col_count)
    {
      ckfree ((char*)listArgv) ;
      return msql_prim_confl ("too many variables in binding list") ;
    }
  else
    count = (listArgc < MsqlHandle[hand].col_count)?listArgc
      :MsqlHandle[hand].col_count ;
  
  while (MsqlHandle[hand].res_count > 0)
    {
      /* Get next row, decrement row counter. */
      if ((row = msqlFetchRow (MsqlHandle[hand].result)) == NULL)
	{
	  MsqlHandle[hand].res_count = 0 ;
	  ckfree ((char*)listArgv) ;
	  return msql_prim_confl ("result counter out of sync") ;
	}
      else
	MsqlHandle[hand].res_count-- ;
      
      /* Bind variables to column values. */
      for (idx = 0; idx < count; idx++)
	{
	  if (listArgv[idx][0] != '-')
	    {
	      if ((val = *row++) == NULL)
		val = MsqlNullvalue ;
	      if (Tcl_SetVar (interp, listArgv[idx], val, TCL_LEAVE_ERR_MSG)
		  == NULL)
		{
		  ckfree ((char*)listArgv) ;
		  return TCL_ERROR ;
		}
	    }
	  else
	    row++ ;
	}

      /* Evaluate the script. */
      if ((code = Tcl_Eval (interp, argv[3])) != TCL_OK)
	switch (code) 
	  {
	  case TCL_CONTINUE:
	    continue ;
	    break ;
	  case TCL_BREAK:
	    ckfree ((char*)listArgv) ;
	    return TCL_OK ;
	    break ;
	  default:
	    ckfree ((char*)listArgv) ;
	    return code ;
	  }
    }
  ckfree ((char*)listArgv) ;
  return TCL_OK ;
}


/*
 *----------------------------------------------------------------------
 *
 * Msqltcl_Info
 * Implements the msqlinfo command:
 * usage: msqlinfo handle option
 *
 */

int
Msqltcl_Info (clientData, interp, argc, argv)
     ClientData   clientData;
     Tcl_Interp  *interp;
     int          argc;
     char       **argv;
{
  char buf[MSQL_BUFF_SIZE];
  int count ;
  int hand ;
  int idx ;
  m_result* list ;
  m_row row ;
  char* val ;
  
  /* We can't fully check the handle at this stage. */
  if ((hand = msql_prologue(interp, argc, argv, 3, get_handle_plain,
			    "handle option")) < 0)
    return TCL_ERROR;

  for (idx = 0;
       idx <= MSQL_INF_OPT_MAX && strcmp (argv[2], MsqlDbOpt[idx]) != 0;
       idx++) ;

  /* First check the handle. Checking depends on the option. */
  switch (idx)
    {
    case MSQL_INFNAME_OPT:
    case MSQL_INFTABLES_OPT:
      hand = get_handle_db (argv[1]) ;
      break ;
    case MSQL_INFNAMEQ_OPT:
      if ((hand = get_handle_conn (argv[1])) >= 0)
	{
	  if (MsqlHandle[hand].database[0] == '\0')
	    return TCL_OK ; /* Return empty string if no current db. */
	}
      break ;
    case MSQL_INFHOST_OPT:
    case MSQL_INFLIST_OPT:
      hand = get_handle_conn (argv[1]) ;
      break ;
    case MSQL_INFHOSTQ_OPT:
      if (MsqlHandle[hand].connection < 0)
	return TCL_OK ; /* Return empty string if not connected. */
      break ;
    default: /* unknown option */
      sprintf (buf, "'%s' unknown option", argv[2]);
      return msql_prim_confl (buf) ;
    }

  if (hand < 0)
      return TCL_ERROR ;

  /* Handle OK, return the requested info. */
  switch (idx)
    {
    case MSQL_INFNAME_OPT:
    case MSQL_INFNAMEQ_OPT:
      strcpy (interp->result, MsqlHandle[hand].database) ;
      break ;
    case MSQL_INFTABLES_OPT:
      if ((list = msqlListTables (MsqlHandle[hand].connection)) == NULL)
	return msql_prim_confl ("could not access table names; server may have gone away") ;

      for (count = msqlNumRows (list); count > 0; count--)
	{
	  val = *(row = msqlFetchRow (list)) ;
	  Tcl_AppendElement (interp, (val == NULL)?"":val) ;
	}
      msqlFreeResult (list) ;
      break ;
    case MSQL_INFHOST_OPT:
    case MSQL_INFHOSTQ_OPT:
      strcpy (interp->result, MsqlHandle[hand].host) ;
      break ;
    case MSQL_INFLIST_OPT:
      if ((list = msqlListDBs (MsqlHandle[hand].connection)) == NULL)
	return msql_prim_confl ("could not access database names; server may have gone away") ;

      for (count = msqlNumRows (list); count > 0; count--)
	{
	  val = *(row = msqlFetchRow (list)) ;
	  Tcl_AppendElement (interp, (val == NULL)?"":val) ;
	}
      msqlFreeResult (list) ;
      break ;
    default: /* should never happen */
      return msql_prim_confl ("weirdness in Msqltcl_Info") ;
    }
  return TCL_OK ;
}


/*
 *----------------------------------------------------------------------
 *
 * Msqltcl_Result
 * Implements the msqlresult command:
 * usage: msqlresult handle option
 *
 */

int
Msqltcl_Result (clientData, interp, argc, argv)
     ClientData   clientData;
     Tcl_Interp  *interp;
     int          argc;
     char       **argv;
{
  char buf[MSQL_BUFF_SIZE];
  int hand ;
  int idx ;


  /* We can't fully check the handle at this stage. */
  if ((hand = msql_prologue(interp, argc, argv, 3, get_handle_plain,
			    " handle option")) < 0)
    return TCL_ERROR;

  for (idx = 0;
       idx <= MSQL_RES_OPT_MAX && strcmp (argv[2], MsqlResultOpt[idx]) != 0;
       idx++) ;

  /* First check the handle. Checking depends on the option. */
  switch (idx)
    {
    case MSQL_RESROWS_OPT:
    case MSQL_RESCOLS_OPT:
    case MSQL_RESCUR_OPT:
      hand = get_handle_res (argv[1]) ;
      break ;
    case MSQL_RESROWSQ_OPT:
    case MSQL_RESCOLSQ_OPT:
    case MSQL_RESCURQ_OPT:
      if ((hand = get_handle_db (argv[1])) >= 0)
	{
	  if (MsqlHandle[hand].result == NULL)
	    return TCL_OK ; /* Return empty string if no pending result. */
	}
      break ;
    default: /* unknown option */
      sprintf (buf, "'%s' unknown option", argv[2]);
      return msql_prim_confl (buf) ;
    }

  if (hand < 0)
    return TCL_ERROR ;

  /* Handle OK; return requested info. */
  switch (idx)
    {
    case MSQL_RESROWS_OPT:
    case MSQL_RESROWSQ_OPT:
      sprintf (interp->result, "%d", MsqlHandle[hand].res_count) ;
      break ;
    case MSQL_RESCOLS_OPT:
    case MSQL_RESCOLSQ_OPT:
      sprintf (interp->result, "%d", MsqlHandle[hand].col_count) ;
      break ;
    case MSQL_RESCUR_OPT:
    case MSQL_RESCURQ_OPT:
      sprintf (interp->result, "%d", msqlNumRows (MsqlHandle[hand].result)
	       - MsqlHandle[hand].res_count) ;
/*     default: */
    }
  return TCL_OK ;
}


/*
 *----------------------------------------------------------------------
 *
 * Msqltcl_Col
 *    Implements the msqlcol command:
 *    usage: msqlcol handle table-name option ?option ...?
 *           msqlcol handle -current option ?option ...?
 * '-current' can only be used if there is a pending result.
 *	                
 *    results:
 *	List of lists containing column attributes.
 *      If a single attribute is requested the result is a simple list.
 *
 * SIDE EFFECT: '-current' disturbs the field position of the result.
 */

int
Msqltcl_Col (clientData, interp, argc, argv)
     ClientData   clientData;
     Tcl_Interp  *interp;
     int          argc;
     char       **argv;
{
  char buf[MSQL_BUFF_SIZE];
  int coln ;
  int conflict ;
  int current_db ;
  int hand;
  int idx ;
  int listArgc ;
  char** listArgv ;
  m_field* fld ;
  m_result* result ;
  char* sep ;
  int simple ;
  
  /* This check is enough only without '-current'. */
  if ((hand = msql_prologue(interp, argc, argv, 4, get_handle_db,
			    "handle table-name option ?option ...?")) < 0)
    return TCL_ERROR;

  /* Fetch column info.
   * Two ways: explicit database and table names, or current.
   */
  current_db = strcmp (argv[2], "-current") == 0 ;
  
  if (current_db)
    {
      if ((hand = get_handle_res (argv[1])) < 0)
	return TCL_ERROR ;
      else
	result = MsqlHandle[hand].result ;
    }
  else
    {
      if ((result = msqlListFields (MsqlHandle[hand].connection, argv[2])) == NULL)
	{
	  sprintf (buf, "no column info for table '%s'; %s", argv[2],
		   "server may have gone away") ;
	  return msql_prim_confl (buf) ;
	}
    }

  /* Must examine the first specifier at this point. */
  if (Tcl_SplitList (interp, argv[3], &listArgc, &listArgv) != TCL_OK)
    return TCL_ERROR ;

  conflict = 0 ;
  simple = (argc == 4) && (listArgc == 1) ;

  if (simple)
    {
      msqlFieldSeek (result, 0) ;
      while ((fld = msqlFetchField (result)) != NULL)
	if (msql_colinfo (interp, fld, argv[3]))
	  {
	    conflict = 1 ;
	    break ;
	  }
    }
  else if (listArgc > 1)
    {
      msqlFieldSeek (result, 0) ;
      for (sep = "{"; (fld = msqlFetchField (result)) != NULL; sep = " {")
	{
	  Tcl_AppendResult (interp, sep, (char*)NULL) ;
	  for (coln = 0; coln < listArgc; coln++)
	    if (msql_colinfo (interp, fld, listArgv[coln]))
	      {
		conflict = 1 ;
		break ;
	      }
	  if (conflict)
	    break ;
	  Tcl_AppendResult (interp, "}", (char*)NULL) ;
	}
      ckfree ((char*)listArgv) ;
    }
  else
    {
      ckfree ((char*)listArgv) ; /* listArgc == 1, no splitting */
      for (idx = 3, sep = "{"; idx < argc; idx++, sep = " {")
	{
	  Tcl_AppendResult (interp, sep, (char*)NULL) ;
	  msqlFieldSeek (result, 0) ;
	  while ((fld = msqlFetchField (result)) != NULL)
	    if (msql_colinfo (interp, fld, argv[idx]))
	      {
		conflict = 1 ;
		break ;
	      }
	  if (conflict)
	    break ;
	  Tcl_AppendResult (interp, "}", (char*)NULL) ;
	}
    }
  
  if (!current_db)
    msqlFreeResult (result) ;
  return (conflict)?TCL_ERROR:TCL_OK ;
}


/*
 *----------------------------------------------------------------------
 *
 * Msqltcl_State
 *    Implements the msqlstate command:
 *    usage: msqlstate ?-numeric? handle 
 *	                
 */

int
Msqltcl_State (clientData, interp, argc, argv)
     ClientData   clientData;
     Tcl_Interp  *interp;
     int          argc;
     char       **argv;
{
  int hi;
  char* hp ;
  int numeric ;
  char* res ;
  
  if (msql_prologue(interp, argc, argv, 2, NULL, "?-numeric? handle") < 0)
    return TCL_ERROR;

  if ((numeric = (strcmp (argv[1], "-numeric") == 0)) && argc < 3)
    return msql_prim_confl ("handle required") ;
  
  hp = (numeric)?argv[2]:argv[1] ;

  if (HSYNTAX(hp,hi) < 0)
    res = (numeric)?"0":"NOT_A_HANDLE" ;
  else if (MsqlHandle[hi].connection < 0)
    res = (numeric)?"1":"UNCONNECTED" ;
  else if (MsqlHandle[hi].database[0] == '\0')
    res = (numeric)?"2":"CONNECTED" ;
  else if (MsqlHandle[hi].result == NULL)
    res = (numeric)?"3":"IN_USE" ;
  else
    res = (numeric)?"4":"RESULT_PENDING" ;

  (void)strcpy (interp->result, res) ;
  return TCL_OK ;
}


/*
 *----------------------------------------------------------------------
 *
 * Msqltcl_Close --
 *    Implements the msqlclose command:
 *    usage: msqlclose ?handle?
 *	                
 *    results:
 *	null string
 */

int
Msqltcl_Close (clientData, interp, argc, argv)
     ClientData   clientData;
     Tcl_Interp  *interp;
     int          argc;
     char       **argv;
{
  int     hand;
  
  /* If handle omitted, close all connections. */
  if (argc == 1)
    {
      Msqltcl_Kill ((ClientData)NULL) ;
      return TCL_OK ;
    }
  
  if ((hand = msql_prologue(interp, argc, argv, 2, get_handle_conn,
			    "handle")) < 0)
    return TCL_ERROR;

  msqlClose (MsqlHandle[hand].connection) ;

  MsqlHandle[hand].connection = -1 ;
  MsqlHandle[hand].host[0] = '\0' ;
  MsqlHandle[hand].database[0] = '\0' ;

  if (MsqlHandle[hand].result != NULL)
    msqlFreeResult (MsqlHandle[hand].result) ;
    
  MsqlHandle[hand].result = NULL ;
  MsqlHandle[hand].res_count = 0 ;

  return TCL_OK;
}

#endif HAVE_MSQL
