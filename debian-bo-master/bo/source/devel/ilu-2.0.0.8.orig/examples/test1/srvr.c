/* $Id: srvr.c,v 1.44 1996/07/02 07:20:30 janssen Exp $ */
/* Last edited by Mike Spreitzer June 20, 1996 10:12 am PDT */

#include <stdio.h>
#include <math.h>
#include <stdlib.h>	/* for exit() */

#include "Test1.h"
#include "Test3.h"

#include "srvr.h"

ilu_Server theServer = ILU_NIL;
ilu_Server SunRPCServer = ILU_NIL;

#if (defined(SECURE_TRANSPORT) && defined(SUNRPCRM_TRANSPORT) && defined(TCPIP_TRANSPORT))

#include <gssapi.h>

#include <ilugssmech_nil.h>
#include <ilugssns_rfc822.h>

static ilu_Passport SetupNILCredentials (char *name)
{
  OM_uint32       major, minor;
  gss_cred_id_t   cred;
  ilu_Error       err;
  ilu_IdentityInfo info;
  ilu_Passport    pp;

  cred = ILU_C_AcquireGSSCredForName(name,
				     GSS_C_INDEFINITE,
				     ilugssmech_nil_OID,
				     ilu_TRUE,
				     &err);
  if (ILU_ERRNOK(err)) {
    ilu_DebugPrintf("Can't acquire credentials for name \"%s\", err <%s>\n",
		    name, ILU_ERR_NAME(err));
    ILU_HANDLED(err);
    return ilu_FALSE;
  }
  info = ILU_C_AcquireGSSIdentity(cred, &err);
  if (ILU_ERRNOK(err)) {
    ilu_DebugPrintf("Can't create GSS identity for name \"%s\", err <%s>\n",
		    name, ILU_ERR_NAME(err));
    ILU_HANDLED(err);
    return ilu_FALSE;
  }
  pp = ILU_C_CreatePassport(info, &err);
  if (ILU_ERRNOK(err)) {
    ilu_DebugPrintf("Can't create passport for name \"%s\", err <%s>\n",
		    name, ILU_ERR_NAME(err));
    ILU_HANDLED(err);
    return ilu_FALSE;
  }
  return pp;
}

#endif /* SECURE_TRANSPORT etc. */

static char    *default_pinfo = "csunrpc";

#if (defined(SUNRPCRM_TRANSPORT) && defined(TCPIP_TRANSPORT))

static ilu_string default_tinfo_insecure[3] = {"sunrpcrm", "tcp_0_0", ILU_NIL};

#if defined(SECURE_TRANSPORT)
static ilu_string default_tinfo_secure[5] = {
  "sunrpcrm",
  "security_1_Xerox.ILU.GSS.NIL",
  "sunrpcrm", "tcp_0_0", ILU_NIL};
#else
static ilu_string *default_tinfo_secure = ILU_NIL;
#endif /* security filter available */

#else
#error "Don't know which transport to use"
#endif


ilu_TransportInfo DefaultTInfo(ilu_boolean secure)
{
  return (secure ? default_tinfo_secure : default_tinfo_insecure);
}


int
doit(char *pinfo, ilu_TransportInfo tinfo, ilu_boolean threadly,
     ilu_boolean init_credentials)
{
#if defined(_WINIO)
  extern void     set_process_windows_messages_alarm(int *pi_stop);
  static int      i_stop = 0;
#endif
  Test1_O1        uc, uc2;
  ilu_Passport    pp = ILU_NIL;

  if (threadly) {
#if (defined(ILU_OS_THREADED))
    ILU_C_USE_OS_THREADS;
#else
    OUTPUT("OS-supplied thread support not configured into ILU!\n");
    exit(-1);
#endif				/* (defined(ILU_OS_THREADED)) */
  }

  Test1__InitializeServer();
  Test3__InitializeServer();
  if (pinfo == NULL)
    pinfo = default_pinfo;
  if (tinfo != NULL)
    0;
  else if (default_tinfo_secure != ILU_NIL) {
    tinfo = default_tinfo_secure;
    init_credentials = ilu_TRUE;
  } else
    tinfo = default_tinfo_insecure;

  if (init_credentials) {
#if defined(SECURE_TRANSPORT)
    OUTPUT("setting up NIL GSS credentials...\n");
    ilugssmech_nil_initialize();
    ilugssns_rfc822_initialize();
    pp = (SetupNILCredentials
    ("Xerox.ILU.GSS.RFC822:server@test1.examples.parc.xerox.com"));
    if (pp == ILU_NIL)
      fprintf(stderr, "Can't initialize NIL credentials.\n");
#else
    OUTPUT("Security support not configured into ILU!\n");
    exit(-1);
#endif				/* security */
  }

  theServer = ILU_C_InitializeServer("Test1-Server", NULL, pinfo, tinfo,
				     pp, ilu_TRUE);

  if (theServer == NULL) {
    OUTPUT("*** Error, Couldn't create server!\n");
    exit(1);
  }
  uc = Test1_O1__CreateTrue("Test1_Initial_Object", theServer, NULL);
  if (uc == NULL) {
    OUTPUT("*** Error, couldn't create object!\n");
    exit(1);
  }
  if (ILU_C_PublishObject(uc) == NULL) {
    fprintf(stderr, "*** Error, can't publish Test1-Server object!\n");
    exit(1);
  }
  /* test the publish and lookup a bit */
  uc2 = ILU_C_LookupObject("Test1-Server", "Test1_Initial_Object",
			   Test1_O1__MSType);
  if (uc2 != uc) {
    fprintf(stderr, "*** Error, lookup returns wrong object!\n");
    exit(1);
  }
  OUTPUT("exported %s\n", ILU_C_SBHOfObject(uc));
#if defined(_WINIO)
  set_process_windows_messages_alarm(&i_stop);
  ILU_C_Stoppable_Run(&i_stop);
  return 0;
#elif (defined(ILU_OS_THREADED))
  ILU_C_FINISH_MAIN_THREAD(0);
#else
  ILU_C_Run();
  return 0;
#endif
}

Test1_U        *
server_Test1_TheO1_U_CSS_to_U(Test1_O1 h, Test1_U * u, Test1_CSS * css,
			      ILU_C_ENVIRONMENT * s)
{
  ilu_Passport    p = s->callerPassport;
  ilu_IdentityInfo ident;
  int             first;
  ilu_Error       err;
  Test1_U        *ans = CORBA_sequence_Test1_U_allocbuf(1);

  OUTPUT("Test1.O1.U-CSS-to-U");
  if (p != ILU_NIL) {
    first = 0;
    OUTPUT(" [caller is");
    ident = ILU_C_FindIdentity(p, ilu_ConnectionIdentity);
    if (ident != ILU_NIL)
      OUTPUT("%sconnection:\"%s\"",
	     (first++ == 0) ? " " : ", ",
	     (ilu_string) (ident->ii_info));
#ifdef SUNRPC_PROTOCOL
    ident = ILU_C_FindIdentity(p, ilu_SunRPCAuthUnixIdentity);
    if (ident != ILU_NIL)
      OUTPUT("%s sunrpc-authunix:(%u,%u)@%s",
	     (first++ == 0) ? " " : ",",
	     (unsigned int) ((ilu_SunRPCAuthUnixIdentityInfo) (ident->ii_info))->ii_UID,
	     (unsigned int) ((ilu_SunRPCAuthUnixIdentityInfo) (ident->ii_info))->ii_GID,
	     ((ilu_SunRPCAuthUnixIdentityInfo)(ident->ii_info))->ii_hostname);
#endif
#ifdef SECURE_TRANSPORT
    {
      ilu_Error err;
      gss_name_t name;
      ilu_boolean localp;
      ilu_string s;

      ident = ILU_C_FindIdentity(p, ilu_GSSIdentity);
      if (ident != ILU_NIL)
	{
	  if (ILU_C_DecodeGSSIdentity (ident, &name, ILU_NIL, ILU_NIL, &localp, ILU_NIL, &err))
	    {
	      if ((s = ILU_C_GSSNameToString (name, &err)) != ILU_NIL)
		{
		  OUTPUT("%sGSS:\"%s\"(%s)", (first++ == 0) ? " " : ", ", s, (localp ? "local" : "remote"));
		  ilu_free(s);
		}
	      else
		{
		  ilu_DebugPrintf ("Error <%s> on attempt to stringify GSS name.\n", ILU_ERR_NAME(err));
		  ILU_HANDLED(err);
		}
	    }
	  else
	    {
	      ilu_DebugPrintf ("Error <%s> on attempt to display GSS identity.\n", ILU_ERR_NAME(err));
	      ILU_HANDLED(err);
	    }
	}
    }
#endif
    OUTPUT("]");
  }
  OUTPUT("\n");
  ans->_d = Test1_U__O1;
  ans->_u.O1 = h;
  return ans;
}

Test1_RO server_Test1_TheO1_f_CSS_to_RO( Test1_O1 h, Test1_CSS *css, ILU_C_ENVIRONMENT *s )
{
  Test1_RO	x = (Test1_RO) ilu_must_malloc( sizeof( Test1_R ));

  x->i = 9;
  Test1_CSS_Init( &x->css , 0, NULL );
  x->a[0]= ILU_C_Strdup("hello");
  x->a[1]= ILU_C_Strdup("world");
  x->a[2]= ILU_C_Strdup("!\n");
  OUTPUT ("Test1.O1.f-CSS-to-R0\n");
  return( x );
}

float server_Test1_TheO1_R_ScS_to_F( Test1_O1 h, Test1_R *r, Test1_ScS str, ILU_C_ENVIRONMENT *s )
{
  float	f = 39.7;

  OUTPUT("Test1.O1.R-ScS-to-F returning %f\n", f);
  return( f );
}

void server_Test1_TheO1_a_RO( Test1_O1 h, Test1_RO ro, ILU_C_ENVIRONMENT *s )
{
  OUTPUT("Test1.O1.a-RO\n");
}

Test1_O2 server_Test1_TheO1_get_O2 ( Test1_O1 h, ILU_C_ENVIRONMENT *s)
{
  static Test1_O2 uc = NULL;

  /* note that Test1.O2 is a singleton class, of cinfo-type "sunrpc_2_0x3458_3".
     This means that it has to be exported via a kernel server with a "sunrpc"
     port... */

  OUTPUT ("Test1.O1.get-O2\n");
  if (uc == NULL)
    {
      ilu_Server s;
      ilu_string sunrpctinfo[] = { "sunrpcrm", "tcp_0_0", ILU_NIL };
      s = ILU_C_InitializeServer("Test1-SunRPC-Server", NULL, "sunrpc", sunrpctinfo, ILU_NIL,
				 ilu_TRUE);
      uc = Test1_O2__CreateTrue( NULL, s, NULL );
    }
  if (uc == NULL)
    {
      s->_major = ILU_C_USER_EXCEPTION;
      s->returnCode = ex_Test1_CantCreate;
      return (NULL);
    }
  else
    return (uc);
}

Test1_O3 server_Test1_TheO1_get_O3 ( Test1_O1 h, CORBA_boolean b, ILU_C_ENVIRONMENT *s )
{
  Test1_O3 uc;
  static int one = 0;

  OUTPUT ("Test1.O1.get-O3\n");
  if (b)
    uc = Test3_O__CreateTrue( NULL, theServer, NULL );
  else
    {
      if (one == 0)
	{
	  one = 1;
	  OUTPUT ("making O3...\n");
	  uc = Test1_O3__CreateTrue( NULL, theServer, NULL );
	}
      else
	{
	  one = 0;
	  OUTPUT ("making O4...\n");
	  uc = Test1_O4__CreateTrue (NULL, theServer, NULL);
	}
    }
  if (uc == NULL)
    {
      s->_major = ILU_C_USER_EXCEPTION;
      s->returnCode = ex_Test1_CantCreate;
      return (NULL);
    }
  else
    return (uc);
}

Test1_CSS      *
server_Test1_O2_OO_A0_to_CSS(Test1_O2 self, Test1_OO o, Test1_A0 a,
			     ILU_C_ENVIRONMENT * s)
{
  OUTPUT("Test1.o2.OO-A0-to-CSS\n");
  if (o == NULL) {
    s->_major = ILU_C_USER_EXCEPTION;
    s->returnCode = ex_Test1_E2;
    s->ptr = ilu_must_malloc(sizeof(ilu_integer));
    *((ilu_integer *) s->ptr) = 7;
    return (NULL);
  } else {
    Test1_CSS      *ans = CORBA_sequence_Test1_CSS_allocbuf(1);
    Test1_CSS_Init(ans, 0, NULL);
    return (ans);
  }
}

ilu_byte * server_Test1_O2_R_I_A1_to_I_A0 (Test1_O2 self, Test1_R *r, Test1_I *i, Test1_A1 a, ILU_C_ENVIRONMENT *s)
{
  ilu_byte       *a2;

  OUTPUT("Test1.O2.R-I-A1-to-I-A0\n");
  a2 = (ilu_byte *) ilu_must_malloc(8);
#if 1
  for (*i = 0; *i < 8; (*i)++)
    a2[*i] = 1 << (1 + ((6 + *i) % 8));
#else
  (*i) += 2;
  a2[2] = 7;
#endif
  return (a2);
}

Test1_IS       *
server_Test1_O3_RS_R_to_R_IS(Test1_O3 self, Test1_RS * r, Test1_R ** pr2,
			     ILU_C_ENVIRONMENT * s)
{
  Test1_IS       *is = CORBA_sequence_Test1_IS_allocbuf(1);
  Test1_R        *r2 = CORBA_sequence_Test1_R_allocbuf(1);

  OUTPUT("Test1.O3.RS-R-to-R-IS\n");
  r2->i = 3;
  r2->css._maximum = 0;
  r2->css._length = 0;
  r2->css._buffer = NULL;
  r2->a[0] = ILU_C_Strdup("just");
  r2->a[1] = ILU_C_Strdup("a");
  r2->a[2] = ILU_C_Strdup("string");
  *pr2 = r2;
  Test1_IS_Init(is, 0, NULL);
  return (is);
}

Test1_IS       *
server_Test1_P_RS_R_to_R_IS(Test1_P self, Test1_RS * r, Test1_R ** pr2,
			    ILU_C_ENVIRONMENT * s)
{
  Test1_IS       *is = CORBA_sequence_Test1_IS_allocbuf(1);
  Test1_R        *r2 = CORBA_sequence_Test1_R_allocbuf(1);

  OUTPUT("Test1.P.RS-R-to-R-IS\n");
  r2->i = 25719;
  r2->css._maximum = 0;
  r2->css._length = 0;
  r2->css._buffer = NULL;
  r2->a[0] = ILU_C_Strdup("from");
  r2->a[1] = ILU_C_Strdup("P");
  r2->a[2] = ILU_C_Strdup("string");
  *pr2 = r2;
  Test1_IS_Init(is, 0, NULL);
  return (is);
}

Test1_I server_Test1_P_BS_to_I (Test1_P self, Test1_BS *b, ILU_C_ENVIRONMENT *s)
{
  return (b->_length);
}

Test1_IS       *
server_Test1_O4_RS_R_to_R_IS(Test1_O4 self, Test1_RS * r, Test1_R ** pr2,
			     ILU_C_ENVIRONMENT * s)
{
  Test1_IS       *is = CORBA_sequence_Test1_IS_allocbuf(1);
  Test1_R        *r2 = CORBA_sequence_Test1_R_allocbuf(1);

  OUTPUT("Test1.O4.RS-R-to-R-IS\n");
  r2->i = 25719;
  r2->css._maximum = 0;
  r2->css._length = 0;
  r2->css._buffer = NULL;
  r2->a[0] = ILU_C_Strdup("from");
  r2->a[1] = ILU_C_Strdup("O4");
  r2->a[2] = ILU_C_Strdup("string");
  *pr2 = r2;
  Test1_IS_Init(is, 0, NULL);
  return (is);
}

void server_Test1_O4_O1_U_to_U (Test1_O4 self, Test1_O1 o, Test1_U *u, ILU_C_ENVIRONMENT *s)
{
  OUTPUT ("Test1.O4.O1-U-to-U\n");
  u->_d = Test1_U__O1;
  u->_u.O1 = o;
  return;
}

Test1_I server_Test1_O4_BS_to_I (Test1_O4 self, Test1_BS *b, ILU_C_ENVIRONMENT *s)
{
#define GETB(x)		((b->_length <= (x)) ? 0 : b->_buffer[x])

  OUTPUT ("Test1.O4.BS_to_I (%lu:  %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x ...) => %lu\n",
	  b->_length,
	  GETB(0), GETB(1), GETB(2), GETB(3), GETB(4), GETB(5), GETB(6), GETB(7), GETB(8), GETB(9), GETB(10),
	  b->_length);
  return (b->_length);
}

ilu_real server_Test1_O4_R_to_R (Test1_O4 self, ilu_real r, ILU_C_ENVIRONMENT *s)
{
  ilu_real r2 = 1020304.05060708;

  OUTPUT ("Test1.O4.R_to_R (%.10f) => %.10f\n", r, r2);
  return (r2);
}

Test1_I server_Test1_O3_BS_to_I (Test1_P self, Test1_BS *b, ILU_C_ENVIRONMENT *s)
{
  return (b->_length * b->_length);
}

Test1_I server_Test3_O_BS_to_I (Test3_O self, Test1_BS *b, ILU_C_ENVIRONMENT *s)
{
  return (b->_length * b->_length);
}

Test1_IS       *
server_Test1_P_m2(Test1_P self, ilu_integer j, ILU_C_ENVIRONMENT * s)
{
  Test1_IS       *foo = CORBA_sequence_Test1_IS_allocbuf(1);

  Test1_IS_Init(foo, 2, NULL);
  foo->_buffer[0] = j;
  foo->_buffer[1] = j * j;
  return (foo);
}

void server_Test1_O3_O1_U_to_U (Test1_O3 self, Test1_O1 o, Test1_U *u, ILU_C_ENVIRONMENT *s)
{
  OUTPUT ("Test1.O3.O1-U-to-U\n");
  u->_d = Test1_U__O1;
  u->_u.O1 = o;
  return;
}

void server_Test1_P_O1_U_to_U (Test1_P self, Test1_O1 o, Test1_U *u, ILU_C_ENVIRONMENT *s)
{
  OUTPUT ("Test1.P.O1-U-to-U\n");
  u->_d = Test1_U__O1;
  u->_u.O1 = o;
  return;
}

Test1_IS       *
server_Test3_O_RS_R_to_R_IS(Test3_O self, Test1_RS * r, Test1_R ** pr2,
			    ILU_C_ENVIRONMENT * s)
{
  Test1_IS       *is = CORBA_sequence_Test1_IS_allocbuf(1);
  Test1_R        *r2 = CORBA_sequence_Test1_R_allocbuf(1);

  OUTPUT("Test3.O.RS-R-to-R-IS\n");
  r2->i = 3;
  r2->css._maximum = 0;
  r2->css._length = 0;
  r2->css._buffer = NULL;
  r2->a[0] = ILU_C_Strdup("just");
  r2->a[1] = ILU_C_Strdup("a");
  r2->a[2] = ILU_C_Strdup("string");
  *pr2 = r2;
  Test1_IS_Init(is, 0, NULL);
  return (is);
}

void server_Test3_O_O1_U_to_U (Test1_O3 self, Test1_O1 o, Test1_U *u, ILU_C_ENVIRONMENT *s)
{
  OUTPUT ("Test3.O.O1-U-to-U(0x%lx, {%d})\n", (unsigned long) o, u->_d);
  u->_d = Test1_U__O1;
  u->_u.O1 = o;
  return;
}

ilu_integer server_Test3_O_SR_to_I (Test3_O self, ilu_shortreal i, ILU_C_ENVIRONMENT *s)
{
  ilu_integer j = i;

  OUTPUT ("Test3.O.SR-to-I(%f)\n", i);
  return (j);
}

Test1_U        *
server_Test3_O_I_to_Test1U(Test3_O self, Test1_I i,
			   ILU_C_ENVIRONMENT * s)
{
  Test1_U        *u = CORBA_sequence_Test1_U_allocbuf(1);
  OUTPUT("Test3.O.I-to-Test1U(%ld)\n", i);
  u->_d = Test1_U__boolean;
  u->_u.boolean = ilu_TRUE;
  ILU_C_SET_SUCCESSFUL(s);
  return (u);
}
