/*
Copyright (c) 1991-1995 Xerox Corporation.  All Rights Reserved.  

Unlimited use, reproduction, and distribution of this software is
permitted.  Any copy of this software must include both the above
copyright notice of Xerox Corporation and this paragraph.  Any
distribution of this software must comply with all applicable United
States export control laws.  This software is made available AS IS,
and XEROX CORPORATION DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED,
INCLUDING WITHOUT LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE, AND NOTWITHSTANDING ANY OTHER
PROVISION CONTAINED HEREIN, ANY LIABILITY FOR DAMAGES RESULTING FROM
THE SOFTWARE OR ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN
CONTRACT, TORT (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF
XEROX CORPORATION IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
*/
/* $Id: debug.c,v 1.70 1996/06/11 03:57:41 janssen Exp $ */
/* Last edited by Mike Spreitzer December 11, 1995 1:58 pm PST */

#define _POSIX_SOURCE

#include <ctype.h>
#include <stdarg.h>

#include "iluntrnl.h"
#include "oscalls.h"	/* for OS_SLEEP */

/*L1, L2, Main unconstrained*/

ilu_cardinal _ilu_DebugLevel = 0;

static ilu_boolean debug_initialized = FALSE;

static void 
printErrorRaises(ilu_ErrorType et,
		 const char *filename,
		 int line)
{
  ILU_ERRPRINTF("**** ILU:  Error <%s> raised in %s, line %d\n",
	  ilu_GetErrorTypeDetails(et)->name, filename, line);
  return;
}
			      
#define CHAR_UNSIGNED 0

#define STRINGIFY(x)	#x

#ifdef _IS_BSD
#define IS_BSD_VAL "is BSD"
#else
#define IS_BSD_VAL "not BSD"
#endif

#ifdef _IS_POSIX
#define IS_POSIX_VAL "is POSIX"
#else
#define IS_POSIX_VAL "not POSIX"
#endif

#ifdef ILU_SOLARIS2_THREADS
#define THREAD_VAL "Solaris 2 threads"
#elif defined(ILU_POSIX_THREADS)
#define THREAD_VAL "POSIX threads"
#else
#define THREAD_VAL "no threads"
#endif

#ifdef SUNRPC_PROTOCOL
#define SUNRPC_PVALUE " sunrpc"
#else
#define SUNRPC_PVALUE ""
#endif

#ifdef COURIER_PROTOCOL
#define COURIER_PVALUE " courier"
#else
#define COURIER_PVALUE ""
#endif

#ifdef IIOP_PROTOCOL
#define IIOP_PVALUE " iiop"
#else
#define IIOP_PVALUE ""
#endif

#ifdef HTTP_PROTOCOL
#define HTTP_PVALUE " http"
#else
#define HTTP_PVALUE ""
#endif

#ifdef W3NG_PROTOCOL
#define W3NG_PVALUE " w3ng"
#else
#define W3NG_PVALUE ""
#endif

#ifdef TCPIP_TRANSPORT
#define TCPIP_PVALUE " tcp"
#else
#define TCPIP_PVALUE ""
#endif

#ifdef UDPSOCKET_TRANSPORT
#define UDPSOCKET_PVALUE " udp"
#else
#define UDPSOCKET_PVALUE ""
#endif

#ifdef SUNRPCRM_TRANSPORT
#define SUNRPCRM_PVALUE " sunrpcrm"
#else
#define SUNRPCRM_PVALUE ""
#endif

#ifdef W3MUX_TRANSPORT
#define W3MUX_PVALUE " w3mux"
#else
#define W3MUX_PVALUE ""
#endif

#ifdef SECURE_TRANSPORT
#define SECURE_PVALUE " secure"
#else
#define SECURE_PVALUE ""
#endif

#ifdef __CHAR_UNSIGNED__
#define CHAR_SIGN_PVALUE "u"
#else
#define CHAR_SIGN_PVALUE "s"
#endif

#ifdef SIZE_T
#define expand_one(x)	#x
#define expand_two(x)	expand_one(x)
#define SIZE_T_PVALUE	expand_two(SIZE_T)
#else
#define SIZE_T_PVALUE	"size_t"
#endif

ilu_cardinal ilu_SetDebugLevel(ilu_cardinal level)
{
  ilu_cardinal old_level = _ilu_DebugLevel;

  if (level != 0 || _ilu_DebugLevel != 0) {
    ILU_ERRPRINTF(
      "ILU version %s.  Copyright 1990-1996 Xerox Corporation.\n",
	    ilu_GetILUVersion());
    ILU_ERRPRINTF(
     "------------------------------------------------------------\n");
    ILU_ERRPRINTF("Configuration info: %s-endian, %s, %s, %s, size_t=%s\n",
#ifdef WORDS_BIGENDIAN
"big",
#else
"little",
#endif
		  IS_BSD_VAL, IS_POSIX_VAL, THREAD_VAL, SIZE_T_PVALUE);
    ILU_ERRPRINTF("  char=%u%s, short=%u, int=%u, long=%u, void *=%u, fnptr=%u,",
		  (unsigned) SIZEOF_CHAR, CHAR_SIGN_PVALUE,
		  (unsigned) SIZEOF_SHORT, (unsigned) SIZEOF_INT, (unsigned) SIZEOF_LONG,
		  (unsigned) SIZEOF_VOID_P, (unsigned) SIZEOF_FN_P);
    ILU_ERRPRINTF(" long long=%u, long double=%u, enum=%u,\n",
		  (unsigned) SIZEOF_LONG_LONG, (unsigned) SIZEOF_LONG_DOUBLE, (unsigned) SIZEOF_ENUM);
    ILU_ERRPRINTF("  protocols =%s, transports =%s,\n",
		  SUNRPC_PVALUE COURIER_PVALUE IIOP_PVALUE HTTP_PVALUE W3NG_PVALUE,
		  " inmem" TCPIP_PVALUE UDPSOCKET_PVALUE SUNRPCRM_PVALUE SECURE_PVALUE W3MUX_PVALUE);
#ifdef ILU_BINDING_DIRECTORY
    {
      char *binding_dir;
      if ((binding_dir = getenv("ILU_BINDING_DIRECTORY")) == NIL)
	binding_dir = ILU_BINDING_DIRECTORY;
      ILU_ERRPRINTF("  binding via shared files in %s\n", binding_dir);
    }
#elif (defined(ILU_BINDING_HOST) && defined(ILU_BINDING_PORT))
    ILU_ERRPRINTF("  binding via ILU service on %s:%u\n",
		  ILU_BINDING_HOST, ILU_BINDING_PORT);
#elif (defined(ILU_BINDING_MCASTADDR))
    ILU_ERRPRINTF("  binding via multicast to %s\n",
		  ILU_BINDING_MCASTADDR);
#endif
    ILU_ERRPRINTF(
     "------------------------------------------------------------\n");
    ILU_ERRPRINTF(
     "ilu_SetDebugLevel:  setting debug mask from 0x%x to 0x%lx\n",
	    _ilu_DebugLevel, level);
  }

  _ilu_DebugLevel = level;

#ifdef ENABLE_DEBUGGING

  if ((_ilu_DebugLevel & ERROR_DEBUG) != 0)
    {
      ILU_ERRPRINTF("ilu_SetDebugLevel:  noting error raises via <debug.c:printErrorRaises>\n");
      ilu_SetRaiseDebugHook (printErrorRaises);
    }

#else

  if (_ilu_DebugLevel != 0)
    ILU_ERRPRINTF("ilu_SetDebugLevel:  ILU kernel was compiled without debugging.  No debugging messages available.\n");

#endif

  debug_initialized = TRUE;

  return old_level;
}

struct debug_entry {
  ilu_string name;
  ilu_cardinal value;
};

static struct debug_entry debugs[] = {  ILU_DEBUG_LIST /* defined in iludebug.h */ };

ilu_integer _ilu_atoi (ilu_string p, ilu_string *success)
{
 ilu_integer sign = 1;
  ilu_cardinal base = 10;
  ilu_string s = p;
  ilu_string last;

  if (*s == '-')
    {
      s++;
      sign = -1;
    }
  else if (*s == '+')
    {
      s++;
    }

  if (*s == '0')
    {
      switch (*++s)
	{
	case 'b':
	case 'B':
	  ++s;
	  base = 2;
	  break;

	case 'x':
	case 'X':
	  ++s;
	  base = 16;
	  break;

	case 'd':
	case 'D':
	  ++s;
	  base = 10;
	  break;

	case 'o':
	case 'O':
	  ++s;
	  base = 8;
	  break;

	default:
	  --s;
	}
    }

  base = strtol(s, &last, (int) base);
  if (base == 0 && last == s && success != NIL)
    *success = p;
  else if (last > s && success != NIL)
    *success = last;
  return (base * sign);
}  

int _ilu_casefree_cmp (const ilu_string s1, const ilu_string s2)
     /* returns 0 if s1 == s2, -1 if s1 < s2, 1 if s1 > s2 */
{
  register ilu_string p1 = s1;
  register ilu_string p2 = s2;
  register char c1;
  register char c2;

  do
    {
      c1 = tolower(*p1);
      c2 = tolower(*p2);

      if (c1 < c2)
	return (-1);
      else if (c1 > c2)
	return (1);
      else if (*p1 == (char) 0)
	return (0);
      p1++; p2++;
    }
  while (*p1 != (char) 0);
  return ((*p2 == (char) 0) ? 0 : -1);
}

int
_ilu_casefree_ncmp(const ilu_string s1, const ilu_string s2,
		   ilu_cardinal n)
/* returns 0 if s1 == s2, -1 if s1 < s2, 1 if s1 > s2 */
{
  ilu_string      p1 = s1, p2 = s2;
  char            c1, c2;
  while (n > 0) {
    c1 = tolower(*p1);
    c2 = tolower(*p2);
    if (c1 < c2)
      return (-1);
    else if (c1 > c2)
      return (1);
    else if (*p1 == (char) 0)
      return (0);
    p1++;
    p2++;
    n--;
  }
  return 0;
}

ilu_cardinal ilu_SetDebugLevelViaString (ilu_string s)
{
  if (s != NIL) {
    char            buf[2000];
    ilu_string      p = NIL;
    ilu_cardinal    debug = 0, i, debugcount;

    if (((debug = _ilu_atoi(s, &p)) == 0 && p == s) || *p != '\0') {
      strcpy(buf, s);
      for (debug = 0, p = buf, s = strchr(buf, ':'),
       debugcount = (sizeof(debugs) / sizeof(struct debug_entry));
	   p != NIL;
	   p = s + 1, s = strchr(s + 1, ':')) {
	if (s != NIL)
	  *s = '\0';
	for (i = 0; i < debugcount; i += 1)
	  if (_ilu_casefree_cmp(debugs[i].name, p) == 0) {
	    debug |= debugs[i].value;
	    break;
	  }
	if (i >= debugcount) {
	  ILU_ERRPRINTF(
		  "_ilu_AutoSetDebugLevel:  Bad debug option \"%s\" specified.  Valid flags are:  ",
		  p);
	  for (i = 0; i < debugcount; i++)
	    ILU_ERRPRINTF(" %s", debugs[i].name);
	  ILU_ERRPRINTF("\n");
	}
	if (s == NIL)
	  break;
      }
    }
    return (ilu_SetDebugLevel(debug));
  }
  return _ilu_DebugLevel;
}

static void DebugPrint (char *formatSpec, va_list ap)
{
  /* vfprintf is ANSI C, section 4.9.6.7 */
  (void) vfprintf (stderr, formatSpec, ap);
}

static void (*debugMessageRoutine) (char *, va_list) = DebugPrint;

void ilu_DebugPrintf (char *formatSpec, ...)
{
  va_list ap;

  va_start (ap, formatSpec);

  if (debugMessageRoutine != NULLFN)
    (*debugMessageRoutine) (formatSpec, ap);

  va_end(ap);
}

void _ilu_DebugPrintfToFileStar (void *fp, char *formatSpec, ...)
{
  va_list ap;

  va_start (ap, formatSpec);

  if (debugMessageRoutine != NULLFN)
    (*debugMessageRoutine) (formatSpec, ap);

  va_end(ap);
}

void ilu_SetDebugMessageHandler (void (*handler)(char *, va_list))
{
  if (((ilu_cardinal) handler) == 1)
    debugMessageRoutine = DebugPrint;
  else
    debugMessageRoutine = handler;
}

static FILE * theDebugOutputFile = ILU_NIL;

static void defaultFileOutput (char * formatSpec, va_list ap)
{
  FILE *f = (theDebugOutputFile == NIL) ? stderr : theDebugOutputFile;

  /* vfprintf is ANSI C, section 4.9.6.7 */
  (void) vfprintf (f, formatSpec, ap);
  fflush(f);
}

void ilu_SendDebugOutputToFile (ilu_string filename)
{
  if ((theDebugOutputFile = fopen(filename, "w")) == ILU_NIL)
    ilu_DebugPrintf ("Can't open debugging output file \"%s\".\n", filename);
  else
    ilu_SetDebugMessageHandler (defaultFileOutput);
}

void _ilu_AutoSetDebugLevel (void)
{
  if (!debug_initialized)	/* just do it once */
    {
      ilu_string s = (ilu_string) getenv ("ILU_DEBUG");
      ilu_string file = (ilu_string) getenv ("ILU_DEBUG_FILE");

      if (file != NIL)
	ilu_SendDebugOutputToFile(file);

      if (s != NIL)
	(void) ilu_SetDebugLevelViaString (s);

      debug_initialized = TRUE;
    }
}

#define MAXDUMP		10000

void _ilu_debug_DumpPacket (ilu_byte *packet, ilu_cardinal length, ilu_string direction)
{
  ilu_cardinal dumplength, i, j;
  ilu_cardinal n;
  ilu_byte c;

  if (length > MAXDUMP) {
    ILU_ERRPRINTF("Request to dump %s%spacket of %lu bytes.",
		  (direction != NIL) ? direction : "",
		  (direction != NIL) ? " " : "", length);
    ILU_ERRPRINTF("  Only %u bytes being dumped.\n", MAXDUMP);
    dumplength = MAXDUMP;
  } else
    dumplength = length;
  if (packet == NIL)
    {
      ILU_ERRPRINTF("Attempt to dump NIL packet.\n");
      return;
    }
  ILU_ERRPRINTF("DumpPacket of %s%spacket %p, length ",
		(direction == NIL) ? "" : direction,
		(direction == NIL) ? "" : " ",
		(void *) packet);
  ILU_ERRPRINTF("%lu bytes, dumping %lu bytes:\n", length, dumplength);
  for (i = 0;  i < dumplength;  i += 16)
    {
      ILU_ERRPRINTF("%6lu:  ", i);
      for (j = 0;  j < 16 AND (i + j) < dumplength;  j += 1)
	ILU_ERRPRINTF("%02x%s ", packet[i + j],
		 ((j % 4) == 3) ? " " : "");
      n = 1;	/* padding before ASCII */
      if (j < 16)
	n += (((16-j)*3) + (4 - (j/4)));
      ILU_ERRPRINTF("%*.*s", n, n, "");
      for (j = 0;  j < 16 AND (i + j) < dumplength;  j += 1)
	{
	  c = packet[i + j];
	  ILU_ERRPRINTF("%c", ((c >= ' ') && (c <= '~')) ? (char) c
							    : '.');
	}
      ILU_ERRPRINTF("\n");
    }
}

/* added for use in debugging interpreted programs */
static _ilu_FailureHandler theAFC = {_ilu_ConsumeByLoop, TRUE};

void ilu_SetAssertionFailureAction (int afa)
{
  DEBUG(ERROR_DEBUG,
	(stderr, "ilu_SetAssertionFailureAction: to %d.\n",
	 afa));
  theAFC = _ilu_FailureActionToConsumer(afa, 1);
  return;
}

void ilu_SetAssertionFailConsumer (ilu_FailureConsumer afc)
{
  _ilu_Assert(afc != NULLFN, "SetAssertionFailConsumer(NIL)");
  DEBUG(ERROR_DEBUG,
	(stderr, "ilu_SetAssertionFailConsumer: to %p.\n",
	 afc));
  theAFC.fc = afc;
  theAFC.printMsg = FALSE;
  return;
}

void 
_ilu_FullAssert(int t, ilu_string id,
		const char *file, int line)
{
  if (t)
    return;
  if (theAFC.printMsg) {
    ILU_ERRPRINTF(
		  "\nILU %s:  old-style runtime kernel consistency check failure,",
		  ilu_GetILUVersion());
    ILU_ERRPRINTF(" at line %d in file %s;", line, file);
    ILU_ERRPRINTF(" clue: %s\n", id);
    ILU_ERRPRINTF("For information on how to debug or report this,");
    ILU_ERRPRINTF(" see the Debugging section of the ILU manual.\n");
  }
  (*theAFC.fc) (file, line);
  ILU_ERRPRINTF("ilu_FailureConsumer %p returned!", theAFC);
  ILU_ERRPRINTF("going into sleep loop!\n");
  _ilu_ConsumeByLoop(__FILE__, __LINE__);
  return;
}

static _ilu_FailureHandler theCFC = {_ilu_ConsumeByLoop, TRUE};

void ilu_SetCheckFailureAction (int cfa)
{
  DEBUG(ERROR_DEBUG,
	(stderr, "ilu_SetCheckFailureAction: to %d.\n",
	 cfa));
  theCFC = _ilu_FailureActionToConsumer(cfa, 2);
  return;
}

void ilu_SetCheckFailureConsumer (ilu_CheckFailureConsumer cfc)
{
  _ilu_Assert(cfc != NULLFN, "SetCheckFailureConsumer(NIL)");
  DEBUG(ERROR_DEBUG,
	(stderr, "ilu_SetCheckFailureConsumer: to %p.\n",
	 cfc));
  theCFC.fc = cfc;
  theCFC.printMsg = FALSE;
  return;
}

ilu_boolean
ilu_FullCheckFailed(ILU_ERRS((internal)) * err,
		     const char *file, int line)
{
  if (theCFC.printMsg) {
    ILU_ERRPRINTF(
		  "\nILU %s:  new-style runtime kernel consistency check failure,",
		  ilu_GetILUVersion());
    ILU_ERRPRINTF(" at line %d in file %s.\n", line, file);
    ILU_ERRPRINTF("For information on how to debug or report this,");
    ILU_ERRPRINTF(" see the Debugging section of the ILU manual.\n");
  }
  (*theCFC.fc) (file, line);
  (void) ILU_ERR_FULLCONS1(internal, err, minor, ilu_im_check, 6,
			   file, line);
  return FALSE;
}

#ifdef WIN16
extern void _far _pascal Yield(void);
#endif

void     _ilu_ConsumeByLoop(const char *f, int l)
{
  ILU_ERRPRINTF(
	  "Entering endless sleep loop (at line %d of %s)",
	  __LINE__ + 4, __FILE__);
  ILU_ERRPRINTF(" for debugging purposes...\n");
  while (1)
#ifdef WIN16
    Yield();
#else
    OS_SLEEP(10);
#endif
}

static void ConsumeByDump(const char*f, int l)
{
  (*(int *) NIL) = 1;		/* This had better terminate the
				 * prog */
  exit(32767);			/* ... just in case it doesn't */
  return;
}

static void ConsumeByRaise(const char *f, int l)
{
  return;
}

static int exits[3];

static void ConsumeByExit0(const char*f, int l) { exit(exits[0]); }
static void ConsumeByExit1(const char*f, int l) { exit(exits[1]); }
static void ConsumeByExit2(const char*f, int l) { exit(exits[2]); }

static ilu_FailureConsumer consumeByExit[3] = {ConsumeByExit0, ConsumeByExit1, ConsumeByExit2};

_ilu_FailureHandler
_ilu_FailureActionToConsumer(int fa, int which)
{
  _ilu_FailureHandler ans = {0, TRUE};
  _ilu_Assert(0 <= which && which <= 2, "FaultActionToConsumer which");
  if (fa > 0) {
    exits[which] = fa;
    ans.fc = consumeByExit[which];
    return ans;
  }
  _ilu_Assert(fa > ((which == 2) ? -3 : -2),
	      "_ilu_FaultActionToConsumer(bogon)");
  if (fa == -1)
    ans.fc = _ilu_ConsumeByLoop;
  else if (fa == -2)
    ans.fc = ConsumeByDump;
  else {
    ans.fc = ConsumeByRaise;
    ans.printMsg = FALSE;
  }
  return ans;
}

