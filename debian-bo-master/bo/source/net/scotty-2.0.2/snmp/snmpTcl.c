/*
 * snmpTcl.c
 *
 * Extend a Tcl-Interpreter about the ability to speak SNMP (Version 1
 * as well as Version 2).
 *
 * Copyright (c) 1994, 1995
 *
 * Sven Schmidt, J. Schoenwaelder
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

#include "snmp.h"
#include "mib.h"

/*
 * The global variable SNMP_Session list contains all existing
 * session handles.
 */

SNMP_Session *sessionList = NULL;

int hexdump = 0;

/*
 * A local hash table to store agent session configurations.
 */

static Tcl_HashTable aliasTable;

/*
 * Forward declarations for procedures defined later in this file:
 */

#ifdef SNMPv2CLASSIC
static int
GetPort		_ANSI_ARGS_((Tcl_Interp *interp, char *arg));

static int
GetAddr		_ANSI_ARGS_((Tcl_Interp *interp, char *host,
			     int port, struct sockaddr_in *addr));
#endif

static int
Configured	_ANSI_ARGS_((Tcl_Interp *interp, SNMP_Session *session));

#ifdef SNMPv2CLASSIC
static void
ShowParty	_ANSI_ARGS_((Tcl_DString *dst, SNMP_Party *party, 
			     char *pfx));
static void
ShowAuth	_ANSI_ARGS_((Tcl_DString *dst, SNMP_Party *party, 
			     char *pfx));
#endif

static void
ShowSession	_ANSI_ARGS_((Tcl_Interp	*interp, SNMP_Session *session));

static int
ShowOption	_ANSI_ARGS_((Tcl_Interp *interp, SNMP_Session *session,
			     char *option));

#ifdef SNMPv2CLASSIC
static int
ConfigParty	_ANSI_ARGS_((Tcl_Interp *interp, SNMP_Party *party, 
			     char *args));
static int
ConfigAuth	_ANSI_ARGS_((Tcl_Interp *interp, SNMP_Party *party,
			     char *args));
static int
ConfigPriv	_ANSI_ARGS_((Tcl_Interp *interp, SNMP_Party *party,
			     char *args));
#endif

static int
ConfigSession	_ANSI_ARGS_((Tcl_Interp *interp, SNMP_Session *session,
			       int argc, char **argv));

static void
DeleteAgentInterp _ANSI_ARGS_((ClientData clientData, Tcl_Interp *interp));

static int
SnmpCmd		_ANSI_ARGS_((ClientData	clientData, Tcl_Interp *interp,
			     int argc, char **argv));
static int
SessionCmd	_ANSI_ARGS_((ClientData	clientData, Tcl_Interp *interp,
			     int argc, char **argv));
static int
WaitSession	_ANSI_ARGS_((SNMP_Session *session, char *id));

static void
DestroySession	_ANSI_ARGS_((ClientData clientdata));

static int
Request		_ANSI_ARGS_((Tcl_Interp *interp, SNMP_Session *session,
			     int pdu_type, int argc, char **argv));

static int
SNMPWalk	_ANSI_ARGS_((Tcl_Interp *interp, SNMP_Session *session,
			     int argc, char **argv));


#ifdef SNMPv2CLASSIC
/*
 * GetPort() returns the port number by interpreting the string in arg.
 * An error message is left in interp->result if the argument can not
 * be converted to a port number and -1 is returned.
 */

static int
GetPort (interp, name)
     Tcl_Interp *interp;
     char *name;
{
    int port;

    if (isdigit(*name)) {
	if (Tcl_GetInt (interp, name, &port) != TCL_OK) {
	    port = -1;
	}
	if (port <= 0) {
	    Tcl_SetResult (interp, "illegal port number", TCL_STATIC);
	    port = -1;
	}
    } else {
	struct servent *servent = getservbyname (name, "udp");
	if (servent) {
	    port = ntohs (servent->s_port);
	} else {
	    Tcl_AppendResult (interp, "unknown port \"", name, "\"",
			      (char *) NULL);
	    port = -1;
	}
    }

    return port;
}


/*
 * GetAddr() returns the network address for the named host. Returns
 * a standard Tcl result and leaves an error message in interp->result.
 */

static int
GetAddr (interp, host, port, addr)
     Tcl_Interp	*interp;
     char *host;
     int port;
     struct sockaddr_in *addr;
{
    struct hostent *hp;

    if ((hp = gethostbyname (host))) {
	memcpy ((char *) &addr->sin_addr, (char *) hp->h_addr, hp->h_length);
    } else {
	int hostaddr = inet_addr (host);
	if (hostaddr == -1) {
	    Tcl_AppendResult (interp, "no such host \"", host, "\"",
			      (char *) NULL);
	    return TCL_ERROR;
	}
	memcpy( (char *) &addr->sin_addr, (char *) &hostaddr, 4);
    } 

    addr->sin_family = AF_INET;
    addr->sin_port = htons (port);
    
    return TCL_OK;
}
#endif


/*
 * Configured() takes a session and checks wether it's parameters for
 * the communication with the desired agent are set. If communication
 * is possible, the function returns 1. If any parameter is missing,
 * so that the communication will fail (e.g. no community string is
 * given), 0 is returned and the missing parameter is reported in
 * interp->result.
 */

static int
Configured (interp, session)
     Tcl_Interp *interp;
     SNMP_Session *session;
{
    if (! session->version) {
        Tcl_AppendResult (interp, "session \"", session->name, 
			  "\" not configured", (char *) NULL);
	return TCL_ERROR;
    }

    switch (session->version) {
      case SNMPv1: {
	  if (! session->community) {
	      Tcl_AppendResult (interp, "session \"", session->name,
				"\" has no community string", (char *) NULL);
	      return TCL_ERROR;
	  }
	  break;
      }

#ifdef SNMPv2C
      case SNMPv2C: {
	  if (! session->community) {
	      Tcl_AppendResult (interp, "session \"", session->name,
				"\" has no community string", (char *) NULL);
	      return TCL_ERROR;
	  }
	  break;
      }
#endif
	
#ifdef SNMPv2CLASSIC
      case SNMPv2CLASSIC: {
	  if (! session->dstParty.Identity) {
	      Tcl_AppendResult (interp, "session \"", session->name,
				"\" has no destination party", (char *) NULL);
	      return TCL_ERROR;
	  }
	  if (! session->srcParty.Identity) {
	      Tcl_AppendResult (interp, "session \"", session->name,
				"\" has no source party", (char *) NULL);
	      return TCL_ERROR;
	  }
	  if (! session->context.Identity) {
	      Tcl_AppendResult (interp, "session \"", session->name,
				"\" has no context", (char *) NULL);
	      return TCL_ERROR;
	  }
	  break;
      }
#endif
	
#ifdef SNMPv2USEC
      case SNMPv2USEC: {
	  if (session->userNameLen == 0) {
	      Tcl_AppendResult (interp, "session \"", session->name,
				"\" has no user name", (char *) NULL);
	      return TCL_ERROR;
	  }
	  break;
      }
#endif
	
      default: {
	  Tcl_SetResult (interp, "unknown SNMP version", TCL_STATIC);
	  return TCL_ERROR;
      }
    }
    
    return TCL_OK;
}


#ifdef SNMPv2CLASSIC
/*
 * ShowParty() writes the configuration of the given party into
 * the dynamic string dst.
 */

static void
ShowParty (dst, party, pfx)
     Tcl_DString *dst;
     SNMP_Party *party;
     char *pfx;
{
    char buf[120];
    char *tmp = ASN1_Oid2Str (party->Identity, party->IdentityLen);

    if (pfx) {
        Tcl_DStringAppendElement (dst, "-");
	Tcl_DStringAppend (dst, pfx, -1);
	Tcl_DStringAppend (dst, "party", 5);
	Tcl_DStringStartSublist (dst);
    }
    Tcl_DStringAppendElement (dst, tmp);
    Tcl_DStringAppendElement (dst, party->TDomain);
    Tcl_DStringAppendElement (dst, party->TAddress);
    sprintf (buf, "%d", party->TPort);
    Tcl_DStringAppendElement (dst, buf);
    sprintf (buf, "%d", party->MaxMessageSize);
    Tcl_DStringAppendElement (dst, buf);
    if (pfx) {
        Tcl_DStringEndSublist (dst);
    }
}


/*
 * ShowAuth() writes the authentication parameter of a party into
 * the dynamic string dst.
 */

static void
ShowAuth (dst, party, pfx) 
     Tcl_DString *dst;
     SNMP_Party *party;
     char *pfx;
{
    char buf[120];

    if (party->AuthProtocol != NO_AUTH) {
        if (pfx) {
	    Tcl_DStringAppendElement (dst, "-");
	    Tcl_DStringAppend (dst, pfx, -1);
	    Tcl_DStringAppend (dst, "partyauth", 9);
	    Tcl_DStringStartSublist (dst);
	}
	sprintf (buf, "%d", party->AuthClock);
	Tcl_DStringAppendElement (dst, buf);
	sprintf (buf, "%d", party->AuthLifetime);
	Tcl_DStringAppendElement (dst, buf);
	SNMP_BinToHex (party->AuthPrivate, MD5_SIZE, buf);
	Tcl_DStringAppendElement (dst, buf);
	if (pfx) {
	    Tcl_DStringEndSublist (dst);
	}
    }
}
#endif


/*
 * ShowSession() writes the configuration settings into the interp.
 */

static void
ShowSession (interp, session)
     Tcl_Interp	*interp;
     SNMP_Session *session;
{
    char buf[120];
    Tcl_DString	result;
    
    Tcl_DStringInit (&result);

    switch (session->version) {
      case SNMPv1:
	if (session->community != NULL) {
	    Tcl_DStringAppendElement (&result, "-community");
	    Tcl_DStringAppendElement (&result, session->community);
	}
	Tcl_DStringAppendElement (&result, "-address");
	Tcl_DStringAppendElement (&result, inet_ntoa(session->tAddr.sin_addr));
	Tcl_DStringAppendElement (&result, "-port");
	sprintf (buf, "%u", ntohs (session->tAddr.sin_port));
	Tcl_DStringAppendElement (&result, buf);
	Tcl_DStringAppendElement (&result, "-version");
	Tcl_DStringAppendElement (&result, "SNMPv1");
	break;
#ifdef SNMPv2C
      case SNMPv2C:
	if (session->community != NULL) {
	    Tcl_DStringAppendElement (&result, "-community");
	    Tcl_DStringAppendElement (&result, session->community);
	}
	Tcl_DStringAppendElement (&result, "-address");
	Tcl_DStringAppendElement (&result, inet_ntoa(session->tAddr.sin_addr));
	Tcl_DStringAppendElement (&result, "-port");
	sprintf (buf, "%u", ntohs (session->tAddr.sin_port));
	Tcl_DStringAppendElement (&result, buf);
	Tcl_DStringAppendElement (&result, "-version");
	Tcl_DStringAppendElement (&result, "SNMPv2C");
	break;	
#endif
	
#ifdef SNMPv2CLASSIC
      case SNMPv2CLASSIC:
	if (session->dstParty.Identity != NULL) {
	    ShowParty (&result, &session->dstParty, "dst");
	    ShowAuth (&result, &session->dstParty, "dst");
	}
	if (session->srcParty.Identity != NULL) {
	    ShowParty (&result, &session->srcParty, "src");
	    ShowAuth (&result, &session->srcParty, "src");
	}
	if (session->context.Identity != NULL) {
	    char *tmp = ASN1_Oid2Str (session->context.Identity,
				      session->context.IdentityLen);
	    Tcl_DStringAppendElement (&result, "-context");
	    Tcl_DStringAppendElement (&result, tmp);
	}
	Tcl_DStringAppendElement (&result, "-version");
	Tcl_DStringAppendElement (&result, "SNMPv2CLASSIC");
	break;
#endif

#ifdef SNMPv2USEC
      case SNMPv2USEC:
	Tcl_DStringAppendElement (&result, "-user");
	memset (buf, '\0', 40);
	memcpy (buf, session->userName, session->userNameLen);
	Tcl_DStringAppendElement (&result, buf);
	if (session->cntxtLen) {
	    Tcl_DStringAppendElement (&result, "-context");
	    memset (buf, '\0', 70);
            memcpy (buf, session->cntxt, session->cntxtLen);
	    Tcl_DStringAppendElement (&result, buf);
	}
	if (session->qos & USEC_QOS_AUTH) {
	    SNMP_BinToHex (session->authKey, MD5_SIZE, buf);
	    Tcl_DStringAppendElement (&result, "-auth");
	    Tcl_DStringAppendElement (&result, buf);
	}
	if (session->qos & USEC_QOS_PRIV) {
	    SNMP_BinToHex (session->privKey, MD5_SIZE, buf);
	    Tcl_DStringAppendElement (&result, "-priv");
	    Tcl_DStringAppendElement (&result, buf);
	}
	Tcl_DStringAppendElement (&result, "-address");
	Tcl_DStringAppendElement (&result, inet_ntoa(session->tAddr.sin_addr));
	Tcl_DStringAppendElement (&result, "-port");
	sprintf (buf, "%u", ntohs (session->tAddr.sin_port));
	Tcl_DStringAppendElement (&result, buf);
	Tcl_DStringAppendElement (&result, "-version");
	Tcl_DStringAppendElement (&result, "SNMPv2USEC");
	break;
#endif
    }

    if (session->agentInterp) {
        int rc = TCL_OK;
	Tcl_ResetResult(interp);
#if (TCL_MINOR_VERSION > 4)
	rc = Tcl_GetInterpPath(interp, session->agentInterp);
#endif
	if (rc == TCL_OK) {
	    Tcl_DStringAppendElement(&result, "-agent");
	    Tcl_DStringAppendElement(&result, interp->result);
	}
    }

    Tcl_DStringAppendElement (&result, "-timeout");
    sprintf (buf, "%d", session->timeout);
    Tcl_DStringAppendElement (&result, buf);

    Tcl_DStringAppendElement (&result, "-retries");
    sprintf (buf, "%d", session->retries);
    Tcl_DStringAppendElement (&result, buf);
    
    Tcl_DStringResult (interp, &result);
    Tcl_DStringFree (&result);
}


/*
 * ShowOption returns the current setting of a single option. This is 
 * the implementation of the cget session command option.
 */

static int
ShowOption (interp, session, option)
     Tcl_Interp *interp;
     SNMP_Session *session;
     char *option;
{
    int len = strlen (option);

    if (strncmp (option, "-timeout", len) == 0) {
        sprintf (interp->result, "%d", session->timeout);
    } else if (strncmp (option, "-retries", len) == 0) {
        sprintf (interp->result, "%d", session->retries);
    } else if (strncmp (option, "-version", len) == 0) {
        switch (session->version) {
	case SNMPv1:
	  Tcl_SetResult (interp, "SNMPv1", TCL_STATIC);
	  break;
#ifdef SNMPv2C
	case SNMPv2C:
	  Tcl_SetResult (interp, "SNMPv2C", TCL_STATIC);
          break;
#endif
#ifdef SNMPv2CLASSIC
	case SNMPv2CLASSIC:
	  Tcl_SetResult (interp, "SNMPv2CLASSIC", TCL_STATIC);
          break;
#endif
#ifdef SNMPv2USEC
	case SNMPv2USEC:
	  Tcl_SetResult (interp, "SNMPv2USEC", TCL_STATIC);
          break;
#endif
	}
    } else if (strncmp (option, "-agent", len) == 0 && len > 1) {
        if (! session->agentInterp) {
	    Tcl_AppendResult(interp, "session \"", session->name, 
			     "\" is not an agent session", (char *) NULL);
	    return TCL_ERROR;
	}
#if (TCL_MINOR_VERSION > 4)
        if (Tcl_GetInterpPath(interp, session->agentInterp) == TCL_ERROR) {
	    Tcl_SetResult(interp, "target interpreter is not a descendant",
			  TCL_STATIC);
	    return TCL_ERROR;
	}
#endif
	return TCL_OK;
    } else if (session->version == SNMPv1) {
        if (strncmp (option, "-address", len) == 0 && len > 1) {
	    Tcl_SetResult (interp,
			   inet_ntoa(session->tAddr.sin_addr), TCL_STATIC);
	} else if (strncmp (option, "-port", len) == 0) {
	    sprintf (interp->result, "%u", ntohs (session->tAddr.sin_port));
	} else if (strncmp (option, "-community", len) == 0) {
	    Tcl_SetResult (interp, session->community, TCL_STATIC);
	} else {
	    goto errorExit;
	}

#ifdef SNMPv2CLASSIC
    } else if (session->version == SNMPv2CLASSIC) {
        Tcl_DString result;
	Tcl_DStringInit (&result);
        if (strncmp (option, "-dstparty", len) == 0) {
	    ShowParty (&result, &session->dstParty, NULL);
	    Tcl_DStringResult (interp, &result);
	} else if (strncmp (option, "-srcparty", len) == 0) {
	    ShowParty (&result, &session->srcParty, NULL);
	    Tcl_DStringResult (interp, &result);
	} else if (strncmp (option, "-dstpartyauth", len) == 0) {
	    ShowAuth (&result, &session->dstParty, NULL);
	    Tcl_DStringResult (interp, &result);
	} else if (strncmp (option, "-srcpartyauth", len) == 0) {
	    ShowAuth (&result, &session->srcParty, NULL);
	    Tcl_DStringResult (interp, &result);
	} else if (strncmp (option, "-context", len) == 0) {
	    Tcl_SetResult (interp, ASN1_Oid2Str (session->context.Identity,
						 session->context.IdentityLen),
			   TCL_STATIC);
	} else {
	    goto errorExit;
	}
#endif

#ifdef SNMPv2USEC
    } else if (session->version == SNMPv2USEC) {
	if (strncmp (option, "-address", len) == 0 && len > 1) {
	    Tcl_SetResult (interp, 
			   inet_ntoa(session->tAddr.sin_addr), TCL_STATIC);
        } else if (strncmp (option, "-port", len) == 0) {
	    sprintf (interp->result, "%u", ntohs (session->tAddr.sin_port));
	} else if (strncmp (option, "-user", len) == 0) {
	    char buf[40];
	    memset (buf, '\0', 40);
	    memcpy (buf, session->userName, session->userNameLen);
	    Tcl_SetResult (interp, buf, TCL_VOLATILE);
	} else if (strncmp (option, "-context", len) == 0) {
	    char buf[70];
	    memset (buf, '\0', 70);
            memcpy (buf, session->cntxt, session->cntxtLen);
	    Tcl_SetResult (interp, buf, TCL_VOLATILE);
	} else if (strncmp (option, "-auth", len) == 0 && len > 1) {
	    if (session->qos & USEC_QOS_AUTH) {
		SNMP_BinToHex (session->authKey, MD5_SIZE, interp->result);
	    }
	} else if (strncmp (option, "-priv", len) == 0) {
	    if (session->qos & USEC_QOS_PRIV) {
		SNMP_BinToHex (session->privKey, MD5_SIZE, interp->result);
	    }
	} else {
	    goto errorExit;
	}
#endif
    }

    return TCL_OK;

  errorExit:
    Tcl_AppendResult (interp, "unknown option \"", option, "\"", 
		      (char *) NULL);
    return TCL_ERROR;
}


#ifdef SNMPv2CLASSIC
/*
 * ConfigParty() configures a party given the parameters in args.
 */

static int
ConfigParty (interp, party, args)
     Tcl_Interp *interp;
     SNMP_Party *party;
     char *args;
{
    int argc;
    char **argv;
    int port, size;
    
    party->AuthProtocol = NO_AUTH;
    party->PrivProtocol = NO_PRIV;

    if (! args || *args == '-') {
	Tcl_SetResult (interp, "party parameters missing", TCL_STATIC);
	return TCL_ERROR;
    }

    if (Tcl_SplitList (interp, args, &argc, &argv) != TCL_OK) {
        return TCL_ERROR;
    }
    if (argc != 5) {
        Tcl_AppendResult (interp, "party parameter list must include ",
			  "oid, domain, address, port, and maxsize", 
			  (char *) NULL);
	goto errorExit;
    }

    if ((port = GetPort (interp, argv[3])) < 0) {
	goto errorExit;
    }

    if (Tcl_GetInt (interp, argv[4], &size) != TCL_OK) {
	goto errorExit;
    }
    if (size <= 0) {
        Tcl_SetResult (interp, "size must be positive", TCL_STATIC);
	goto errorExit;
    }
    
    if (party->Identity) ckfree ((char *) party->Identity);
    if (party->TDomain)  ckfree (party->TDomain);
    if (party->TAddress) ckfree (party->TAddress);

    party->Identity = ASN1_Str2Oid (argv[0], &party->IdentityLen);
    party->Identity = ASN1_OidDup (&party->IdentityLen, party->Identity, 
				   party->IdentityLen);
    party->TDomain  = ckstrdup (argv[1]);
    party->TAddress = ckstrdup (argv[2]);
    party->TPort = port;
    party->MaxMessageSize = size;

    ckfree ((char *) argv);
    return TCL_OK;

  errorExit:
    ckfree ((char *) argv);
    return TCL_ERROR;
}


/*
 * ConfigAuth() configures the authentication parameter of a party.
 */

static int
ConfigAuth (interp, party, args)
     Tcl_Interp *interp;
     SNMP_Party *party;
     char *args;
{
    int argc;
    char **argv;
    int clock, lifetime;
    int len;

    if (! args || *args == '-') {
	Tcl_SetResult (interp, "party authentication parameters missing",
		       TCL_STATIC);
	return TCL_ERROR;
    }

    if (Tcl_SplitList (interp, args, &argc, &argv) != TCL_OK) {
        return TCL_ERROR;
    }
    if (argc != 3) {
        Tcl_AppendResult (interp, "authentication parameter list must ",
			  "include key, clock, and lifetime", (char *) NULL);
	goto errorExit;
    }

    if (Tcl_GetInt (interp, argv[0], &clock) != TCL_OK) {
	goto errorExit;
    }
    if (clock <= 0) {
        Tcl_SetResult (interp, "clock must be positive", TCL_STATIC);
	goto errorExit;
    }
    if (Tcl_GetInt (interp, argv[1], &lifetime) != TCL_OK) {
	goto errorExit;
    }
    if (lifetime <= 0) {
        Tcl_SetResult (interp, "lifetime must be positive", TCL_STATIC);
	goto errorExit;
    }

    if ((len = strlen (argv[2])) == MD5_SIZE) {
	memcpy (party->AuthPrivate, argv[2], MD5_SIZE);
    } else if (len == 3 * MD5_SIZE - 1) {
	if (SNMP_HexToBin (argv[2], party->AuthPrivate, &len) < 0) {
	    Tcl_SetResult (interp, "illegal octet string value", TCL_STATIC);
	    goto errorExit;
	}
    } else {
	sprintf (interp->result, "auth key should be %d octets", MD5_SIZE);
	goto errorExit;
    }

    party->AuthProtocol = MD5_AUTH;
    party->AuthClock = (u_int) clock;
    party->AuthLifetime = (u_int) lifetime;

    ckfree ((char *) argv);
    return TCL_OK;

  errorExit:
    ckfree ((char *) argv);
    return TCL_ERROR;
}


/*
 * ConfigPriv() configures the privacy parameter of a party.
 */

static int
ConfigPriv (interp, party, args)
     Tcl_Interp *interp;
     SNMP_Party *party;
     char *args;
{
    int len;

    if (! args || *args == '-') {
	Tcl_SetResult (interp, "party privacy parameters missing",
                       TCL_STATIC);
        return TCL_ERROR;
    }

    if ((len = strlen (args)) == MD5_SIZE) {
        memcpy (party->PrivPrivate, args, MD5_SIZE);
    } else if (len == 3 * MD5_SIZE - 1) {
        if (SNMP_HexToBin (args, party->PrivPrivate, &len) < 0) {
	    Tcl_SetResult (interp, "illegal octet string value", TCL_STATIC);
	    return TCL_ERROR;
	}
    } else {
	sprintf (interp->result, "privacy key should be %d octets", MD5_SIZE);
	return TCL_ERROR;
    }

    party->PrivProtocol = DES_PRIV;    

    return TCL_OK;
}
#endif


/*
 * ConfigSession() configures a session handle. Before actually changing
 * the session structure, we have to make sure that there are no pending
 * requests. Otherwise, we could get into problems if the authentication
 * model changes, since we patch authentication bytes into the BER encoded
 * packets before they go out on the wire.
 */

static int
ConfigSession (interp, session, argc, argv)
     Tcl_Interp *interp;
     SNMP_Session *session;
     int argc;
     char **argv;
{
    WaitSession (session, NULL);

    argc++; argv--;	/* this is a hack */

    while (--argc && **++argv == '-') {

	int len = strlen (*argv);

	/*
	 * An alias definition. Expand the alias name to the actual 
	 * configuration parameters. Note, this implementation allows
	 * recursive definitions.
	 */

	if (!strncmp (*argv, "-alias", len) && len > 2) {
	    Tcl_HashEntry *entryPtr;
	    int largc, code;
	    char **largv;
	    char *alias;
	    if (--argc <= 0 || **++argv == '-') {
		Tcl_SetResult (interp, "alias name missing", TCL_STATIC);
		return TCL_ERROR;
	    }
	    entryPtr = Tcl_FindHashEntry (&aliasTable, *argv);
	    if (!entryPtr) {
		Tcl_AppendResult (interp, "unknown alias \"",
				  *argv, "\"", (char *) NULL);
		return TCL_ERROR;
	    }
	    alias = (char *) Tcl_GetHashValue (entryPtr);
	    if (! alias) {
		Tcl_SetResult (interp, "alias loop detected", TCL_STATIC);
		return TCL_ERROR;
	    }
	    if (Tcl_SplitList (interp, alias, &largc, &largv) != TCL_OK) {
		return TCL_ERROR;
	    }
	    Tcl_SetHashValue (entryPtr, NULL);
	    code = ConfigSession (interp, session, largc, largv);
	    Tcl_SetHashValue (entryPtr, alias);
	    ckfree ((char *) largv);
	    if (code != TCL_OK) {
		return code;
	    }
	    continue;
	}

	/*
	 * Agent session configuration.
	 */
	
	if (!strncmp (*argv, "-agent", len) && len > 2) {
	    if (--argc <= 0 || **++argv == '-') {
		Tcl_SetResult (interp, "interp name missing", TCL_STATIC);
		return TCL_ERROR;
	    }

#ifdef SNMPv2USEC
	    /*
	     * This is a hack. We are required to store these values
	     * in NVRAM - I need to think about how to implement this.
	     */
	    {
		u_char *p = session->agentID;
		int id = 1701;
		*p++ = (id >> 24) & 0xff;
		*p++ = (id >> 16) & 0xff;
		*p++ = (id >> 8) & 0xff;
		*p++ = id & 0xff;
		id = session->tAddr.sin_addr.s_addr;
		*p++ = (id >> 24) & 0xff;
                *p++ = (id >> 16) & 0xff;
                *p++ = (id >> 8) & 0xff;
                *p++ = id & 0xff;
		memcpy (p, "tubs", 4);
	    }
	    session->agentTime = time ((time_t *) NULL);
	    session->agentBoots = session->agentTime;
#endif

#if (TCL_MINOR_VERSION > 4)
	    session->agentInterp = Tcl_GetSlave (interp, *argv);
	    if (! session->agentInterp) {
	        Tcl_AppendResult (interp, "unknown interp \"", *argv, "\"",
				  (char *) NULL);
		return TCL_ERROR;
	    }
#else
	    session->agentInterp = interp;
#endif
	    Tcl_CallWhenDeleted(session->agentInterp, DeleteAgentInterp, 
				(ClientData) session);

	    if (session->agentInterp != interp) {
		Tcl_CreateCommand(session->agentInterp, session->name,
				  SessionCmd, (ClientData) session,
				  DestroySession);
	    }

#if (TCL_MINOR_VERSION > 4)
	    if (! Tcl_IsSafe(session->agentInterp)) {
#endif
		fprintf(stderr, "Warning: SNMP agent created based %s\n",
			"on an unsafe Tcl interpreter!");
#if (TCL_MINOR_VERSION > 4)
	    }
#endif
	    continue;
	}

	/*
	 * SNMPv1 / SNMPv2USEC destination IP address
	 */
	
	if (!strncmp (*argv, "-address", len) && len > 2) {
	    struct hostent *hp;
#ifndef SNMPv2USEC
	    session->version = SNMPv1;
#endif
	    if (--argc <= 0 || **++argv == '-') {
		Tcl_SetResult (interp, "IP address missing", TCL_STATIC);
		return TCL_ERROR;
	    }
	    if ((hp = gethostbyname (*argv))) {
		memcpy ((char *) &session->tAddr.sin_addr,
			(char *) hp->h_addr, hp->h_length);
	    } else {
		int hostaddr = inet_addr (*argv);
		if (hostaddr == -1) {
		    Tcl_AppendResult (interp, "no such host \"", *argv, "\"",
				      (char *) NULL);
		    return TCL_ERROR;
		}
		memcpy( (char *) &session->tAddr.sin_addr,
		       (char *) &hostaddr, 4);
	    } 
	    continue;
	}
	
	/*
	 * SNMPv1 / SNMPv2USEC port number
	 */
	
	if (!strncmp (*argv, "-port", len)) {
#ifndef SNMPv2USEC
	    session->version = SNMPv1;
#endif
	    if (--argc <= 0 || **++argv == '-') {
		Tcl_SetResult (interp, "port number missing", TCL_STATIC);
		return TCL_ERROR;
	    }
	    if (isdigit (*argv[0])) {
		int port;
		if (Tcl_GetInt (interp, *argv, &port) != TCL_OK) {
		    return TCL_ERROR;
		}
		if (port <= 0) {
		    Tcl_SetResult (interp, "illegal port number", TCL_STATIC);
		    return TCL_ERROR;
		}
		session->tAddr.sin_port = htons (port);
	    } else {
		struct servent *servent = getservbyname (*argv, "udp");
		if (servent) {
		    session->tAddr.sin_port = servent->s_port;
		} else {
		    Tcl_AppendResult (interp, "unknown port \"", *argv, "\"",
				      (char *) NULL);
		    return TCL_ERROR;
		}
	    }
	    continue;
	}
	
	/*
	 * SNMPv1: community string
	 */
	
	if (!strncmp (*argv, "-community", len)) {
	    session->version = SNMPv1;
	    if (--argc <= 0 || **++argv == '-') {
		Tcl_SetResult (interp, "community string missing", TCL_STATIC);
		return TCL_ERROR;
	    }
	    ckfree (session->community);
	    session->community = ckstrdup (*argv);
	    continue;
	}
	
#ifdef SNMPv2CLASSIC

	/*
         * SNMPv2: basic parameters for source party
	 */

	if (!strncmp (*argv, "-srcparty", len)) {
	    session->version = SNMPv2CLASSIC;
	    argc--;
	    if (ConfigParty (interp, &session->srcParty, *++argv) != TCL_OK) {
		return TCL_ERROR;
	    }
	    continue;
	}

	/*
         * SNMPv2: authentication protocol parameters for source party
	 *     ==> AuthClock, AuthLifetime, AuthPrivate, AuthPublic
	 */

	if (!strncmp (*argv, "-srcpartyauth", len)) {
	    session->version = SNMPv2CLASSIC;
	    argc--;
	    if (ConfigAuth (interp, &session->srcParty, *++argv) != TCL_OK) {
                return TCL_ERROR;
            }
            continue;
        }
	
	/*
         * SNMPv2: privacy protocol parameters for source party
         *     ==> PrivPrivate, PrivPublic
	 */

	if (!strncmp (*argv, "-srcpartypriv", len)) {
	    session->version = SNMPv2CLASSIC;
	    argc--;
	    if (ConfigPriv (interp, &session->srcParty, *++argv) != TCL_OK) {
                return TCL_ERROR;
            }
            continue;
        }
	
	/*
	 * SNMPv2: basic parameters for destination party
	 */
	
	if (!strncmp (*argv, "-dstparty", len)) {
	    session->version = SNMPv2CLASSIC;
	    argc--;
 	    if (ConfigParty (interp, &session->dstParty, *++argv) != TCL_OK) {
		return TCL_ERROR;
	    }
	    continue;
	}
	
	/*
         * SNMPv2: authentication protocol parameters for destination party
         *     ==> AuthClock, AuthLifetime, AuthPrivate, AuthPublic
	 */
	
	if (!strncmp (*argv, "-dstpartyauth", len)) {
	    session->version = SNMPv2CLASSIC;
	    argc--;
	    if (ConfigAuth (interp, &session->dstParty, *++argv) != TCL_OK) {
                return TCL_ERROR;
            }
            continue;
        }

	/*
	 * SNMPv2: privacy protocol parameters for destination party
         *     ==> PrivPrivate, PrivPublic
	 */

	if (!strncmp (*argv, "-dstpartypriv", len)) {
	    session->version = SNMPv2CLASSIC;
	    argc--;
	    if (ConfigPriv (interp, &session->dstParty, *++argv) != TCL_OK) {
                return TCL_ERROR;
            }
            continue;
        }
	
	/*
	 * SNMPv2: context for SNMP operations
	 */
	
	if (session->version == SNMPv2CLASSIC && ! strncmp (*argv, "-context", len)) {
	    if (--argc <= 0 || **++argv == '-') {
		interp->result = "context identifier missing";
		return TCL_ERROR;
	    }
	    if (session->context.Identity) {
		ckfree ((char *) session->context.Identity);
	    }
	    session->context.Identity 
	      = ASN1_Str2Oid (*argv, &session->context.IdentityLen);
	    session->context.Identity 
	      = ASN1_OidDup (&session->context.IdentityLen,
			     session->context.Identity, 
			     session->context.IdentityLen);
	    continue;
	}
#endif

#ifdef SNMPv2USEC

	/*
         * SNMPv2: user for the USEC model
         */

	if (! strncmp (*argv, "-user", len)) {
	    session->version = SNMPv2USEC;
	    if (--argc <= 0 || **++argv == '-') {
		Tcl_SetResult (interp, "user name missing", TCL_STATIC);
                return TCL_ERROR;
            }
	    if (strlen(*argv) > USEC_MAX_USER) {
		Tcl_SetResult (interp, "user name too long", TCL_STATIC);
		return TCL_ERROR;
	    }
	    session->userNameLen = strlen(*argv);
	    memcpy (session->userName, *argv, session->userNameLen);
	    continue;
	}

	if (session->version == SNMPv2USEC && 
	    !strncmp (*argv, "-context", len)) {
	    if (--argc <= 0 || **++argv == '-') {
		Tcl_SetResult (interp, "context name missing", TCL_STATIC);
		return TCL_ERROR;
	    }
	    if (strlen(*argv) > USEC_MAX_CONTEXT) {
		Tcl_SetResult (interp, "context name too long", TCL_STATIC);
		return TCL_ERROR;
	    }
	    session->cntxtLen = strlen(*argv);
	    memcpy (session->cntxt, *argv, session->cntxtLen);
	    continue;
	}
	
	if (! strncmp (*argv, "-auth", len) && len > 2) {
	    int len;
	    u_char buf[17];
	    session->version = SNMPv2USEC;
	    if (--argc <= 0 || **++argv == '-') {
                interp->result = "auth key missing";
                return TCL_ERROR;
            }
	    len = strlen (*argv);
	    if (len == 0) {
		session->qos &= ~ USEC_QOS_AUTH;
	    } else {
		session->qos |= USEC_QOS_AUTH;
		if (len == 3 * MD5_SIZE - 1) {
		    SNMP_HexToBin (*argv, buf, &len);
		    if  (len == MD5_SIZE) {
			memcpy (session->authKey, buf, len);
			continue;
		    }
		}
		SNMP_Passwd2Key (*argv, session->authKey);
	    }
	    continue;
	}

	if (! strncmp (*argv, "-priv", len)) {
	    u_char buf[17];
	    session->version = SNMPv2USEC;
	    if (--argc <= 0 || **++argv == '-') {
                interp->result = "priv key missing";
                return TCL_ERROR;
            }
	    len = strlen (*argv);
	    if (len == 0) {
		session->qos &= ~ USEC_QOS_PRIV;
	    } else {
		session->qos |= USEC_QOS_PRIV;
		if (len == 3 * MD5_SIZE - 1) {
		    SNMP_HexToBin (*argv, buf, &len);
		    if  (len == MD5_SIZE) {
			memcpy (session->privKey, buf, len);
			continue;
		    }
		}
		SNMP_Passwd2Key (*argv, session->privKey);
	    }
	    continue;
	}
#endif
	
	/*
	 * general: retries
	 */
	
	if (! strncmp (*argv, "-retries", len)) {
	    int num;
	    if (--argc <= 0 || **++argv == '-') {
		Tcl_SetResult (interp, "missing retry parameter", TCL_STATIC);
		return TCL_ERROR;
	    }
	    if (Tcl_GetInt (interp, *argv, &num) != TCL_OK) {
	        return TCL_ERROR;
	    }
	    if (num < 0) {
		Tcl_SetResult (interp, "retries may not be negative", 
			       TCL_STATIC);
		return TCL_ERROR;
	    }
	    session->retries = num;
	    continue;
	}
	
	/*
	 * general: timeout
	 */
	
	if (! strncmp (*argv, "-timeout", len)) {
	    int num;
	    if (--argc <= 0 || **++argv == '-') {
		Tcl_SetResult (interp, "missing timeout parameter", 
			       TCL_STATIC);
		return TCL_ERROR;
	    }
	    if (Tcl_GetInt (interp, *argv, &num) != TCL_OK) {
	        return TCL_ERROR;
	    }
	    if (num < 0) {
		Tcl_SetResult (interp, "timeout may not be negative", 
			       TCL_STATIC);
		return TCL_ERROR;
	    }
	    session->timeout = num;
	    continue;
	}
	
	/*
	 * general: version
	 */
	
	if (! strncmp (*argv, "-version", len)) {
	    int num;
	    if (--argc <= 0 || **++argv == '-') {
		Tcl_SetResult (interp, "missing version parameter", 
			       TCL_STATIC);
		return TCL_ERROR;
	    }
	    if (strcmp (*argv, "SNMPv1") == 0) {
	      session->version = SNMPv1;
#ifdef SNMPv2C
	    } else if (strcmp (*argv, "SNMPv2C") == 0) {
	      session->version = SNMPv2C;
#endif
#ifdef SNMPv2CLASSIC
	    } else if (strcmp (*argv, "SNMPv2CLASSIC") == 0) {
	      session->version = SNMPv2CLASSIC;
#endif
#ifdef SNMPv2USEC
	    } else if (strcmp (*argv, "SNMPv2USEC") == 0) {
	      session->version = SNMPv2USEC;
#endif
	    } else {
		Tcl_SetResult (interp, "unknown version", TCL_STATIC);
		return TCL_ERROR;
	    }
	    continue;
	}
	
	Tcl_AppendResult (interp, "unknown configuration option \"", *argv,
			  "\"", (char *) NULL);
	return TCL_ERROR;
    }
    
    /*
     * invalid config option given ?
     */
    
    if (argc) {
	Tcl_AppendResult (interp, "bad configuration option \"", *argv,
			  "\"", (char *) NULL);
	return TCL_ERROR;
    }

#ifdef SNMPv2CLASSIC    
    /*
     * Get the network address, if it's yet unknown. This is the old
     * version for P/P/C SNMPv2CLASSIC.
     */

    if (session->version == SNMPv2CLASSIC) {
	if (GetAddr (interp, session->dstParty.TAddress, 
		     session->dstParty.TPort, &session->tAddr) != TCL_OK) {
	    return TCL_ERROR;
	}
    }
#endif

    /*
     * Try to set up the agent if we are asked to create an agent
     * session.
     */

    if (session->agentInterp) {
	SNMP_AgentInit(session->agentInterp);
	if (SNMP_AgentSocket(session->agentInterp, session) != TCL_OK) {
	    if (interp != session->agentInterp) {
		Tcl_SetResult(interp, session->agentInterp->result, 
			      TCL_STATIC);
	    }
	    return TCL_ERROR;
	}
    }
    
    /*
     * Return the current settings as a result.
     */
    
    ShowSession (interp, session);
    return TCL_OK;
}


/*
 * Snmp_Init() initializes the SNMP extension. Note, it does not 
 * initialize the `mib' command. The auto-loading hack for the
 * mib command is in SnmpCmd().
 */

int
Snmp_Init (interp)
     Tcl_Interp *interp;
{
    SNMP_SysUpTime();
    memset ((char *) &snmpStats, '\0', sizeof (SNMP_Statistics));

    Tcl_CreateCommand (interp, "snmp", SnmpCmd, (ClientData) NULL,
		       (Tcl_CmdDeleteProc *) NULL);

    Tcl_InitHashTable (&aliasTable, TCL_STRING_KEYS);

    srand (time (NULL) * getpid ());

    return MIB_Init (interp);
}


/*
 * This function is called whenever the interpreter registered in
 * an agent session is deleted.
 */

static void
DeleteAgentInterp (clientData, interp)
     ClientData clientData;
     Tcl_Interp *interp;
{
    SNMP_Session *sPtr = (SNMP_Session *) clientData;
    sPtr->agentInterp = NULL;
    Tcl_DeleteCommand (interp, sPtr->name);
}


/*
 * SnmpCmd() is called from the TCL interpreter to evaluate the snmp
 * command.
 */

static int
SnmpCmd (clientData, interp, argc, argv)
     ClientData	clientData;
     Tcl_Interp	*interp;
     int argc;
     char **argv;
{
    static int id = 0;
    int len = argc > 1 ? strlen (argv[1]) : 0;
    SNMP_Session *sPtr;

    if (argc < 2) {
	Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			  " option ?arg arg ...?\"", (char *) NULL);
	return TCL_ERROR;
    }

    /*
     * snmp subcommand "alias"
     */
    
    if (strncmp (argv[1], "alias", len) == 0) {
        Tcl_HashEntry *entryPtr;
        if (argc == 2) {
	    Tcl_HashSearch search;
	    entryPtr = Tcl_FirstHashEntry (&aliasTable, &search);
	    while (entryPtr) {
	        Tcl_AppendElement (interp,
				   Tcl_GetHashKey (&aliasTable, entryPtr));
	        entryPtr = Tcl_NextHashEntry (&search);
	    }
	    return TCL_OK;
	} else if (argc == 3) {
	    entryPtr = Tcl_FindHashEntry (&aliasTable, argv[2]);
	    if (entryPtr) {
	        Tcl_SetResult (interp, (char *) Tcl_GetHashValue (entryPtr),
			       TCL_STATIC);
	    }
	    return TCL_OK;
	} else if (argc == 4) {
	    int isNew;
	    entryPtr = Tcl_CreateHashEntry (&aliasTable, argv[2], &isNew);
	    if (!isNew) {
		ckfree ((char *) Tcl_GetHashValue (entryPtr));
	    }
	    if (*argv[3] == '\0') {
		Tcl_DeleteHashEntry (entryPtr);
	    } else {
		Tcl_SetHashValue (entryPtr, ckstrdup (argv[3]));
	    }
	    return TCL_OK;
	} else {
	    Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			      " alias ?agent? ?config?\"", (char *) NULL);
	    return TCL_ERROR;
	}
    }

    if (strncmp (argv[1], "session", len) == 0) {

	/*
	 * snmp subcommand "session" -- create socket if neccessary 
	 * (this installs the event handler).
	 */
	
	if (SNMP_ManagerSocket (interp) != TCL_OK) {
	    return TCL_ERROR;
	}

	/* 
	 * Call an arbitrary mib command to trigger the autoload
	 * mechanism -- ugly but correct in most cases.
	 */

	Tcl_Eval (interp, "mib oid 1");
	Tcl_ResetResult (interp);
	
	/*
	 * Allocate memory for a new session and fill out the
	 * network address.
	 */
	
	sPtr = SNMP_MallocSession ();
	sprintf (sPtr->name, "snmp%d", id++);
	
	/* 
	 * Configure the session and link it into the session list.
	 */

	if (ConfigSession (interp, sPtr, argc - 2, argv + 2) != TCL_OK) {
	    SNMP_FreeSession (sPtr);
	    return TCL_ERROR;
	}

	sPtr->nextPtr = sessionList;
	sessionList = sPtr;

	/*
	 * Finally create a Tcl command for this session.
	 */
	
	Tcl_CreateCommand (interp, sPtr->name, SessionCmd,
			   (ClientData) sPtr, DestroySession);
	Tcl_SetResult (interp, sPtr->name, TCL_STATIC);
	return TCL_OK;

    } else if (strncmp (argv[1], "info", len) == 0) {

	/*
	 * snmp subcommand "info" -- simply output the list of
	 * open sessions.
	 */
	
        if (argc != 2) {
	    Tcl_AppendResult (interp, "wrong # args: should be \"",
			      argv[0], " info\"", (char *) NULL);
	    return TCL_ERROR;
	}
	for (sPtr = sessionList; sPtr; sPtr = sPtr->nextPtr) {
	    Tcl_AppendElement (interp, sPtr->name);
	}
	return TCL_OK;
    
    } else if (strncmp (argv[1], "wait", len) == 0) {

	/*
	 * snmp subcommand "wait" -- we have to start the loop every time
	 * we have done an event because the event may have changed the 
	 * session list.
	 */
	
        if (argc != 2) {
	    Tcl_AppendResult (interp, "wrong # args: should be \"",
			      argv[0], " wait\"", (char *) NULL);
	    return TCL_ERROR;
	}
      repeat:
	for (sPtr = sessionList; sPtr; sPtr = sPtr->nextPtr) {
	    if (sPtr->requestList) {
		Tk_DoOneEvent (0);
		goto repeat;
	    }
	}
	return TCL_OK;
    
    } else if (strncmp (argv[1], "watch", len) == 0) {

	/*
	 * snmp subcomand "watch" -- simply toggle the hexdump variable.
	 */

	if (argc > 3) {
            Tcl_AppendResult (interp, "wrong # args: should be \"",
                              argv[0], " watch ?bool?\"", (char *) NULL);
            return TCL_ERROR;
        }
	if (argc < 3) {
	    Tcl_SetResult (interp, hexdump ? "1" : "0", TCL_STATIC);
	    return TCL_OK;
	}
	return (Tcl_GetBoolean (interp, argv[2], &hexdump));
    }
    
    /*
     * invalid "snmp" subcommand
     */

    Tcl_AppendResult (interp, "bad option \"", argv[1], 
		      "\": should be alias, session, ",
		      "wait, watch, or info", (char *) NULL);
    return TCL_ERROR;
}


/*
 * SessionCmd() is called from the TCL interpreter to evaluate a
 * command on a specific session handle.
 */

static int
SessionCmd (clientData, interp, argc, argv)
     ClientData	clientData;
     Tcl_Interp	*interp;
     int argc;
     char **argv;
{
    int len = argc > 1 ? strlen (argv[1]) : 0;
    SNMP_Session *session = (SNMP_Session *) clientData;

    if (argc < 2) {
	Tcl_AppendResult (interp, "wrong # args: should be \"",
			  argv[0], " option ?arg arg ...?\"", (char *) NULL);
	return TCL_ERROR;
    }

    if (strncmp (argv[1], "configure", len) == 0) {
	if (argc == 2) {
	    ShowSession (interp, session);
	    return TCL_OK;
	}
	return ConfigSession (interp, session, argc - 2, argv + 2);
    } else if (strncmp (argv[1], "wait", len) == 0) {
	if (argc == 2) {
	    return WaitSession (session, NULL);
	} else if (argc == 3) {
	    return WaitSession (session, argv[2]);
	}
	Tcl_AppendResult (interp, "wrong # args: should be \"",
			  argv[0], " wait ?request?\"", (char *) NULL);
	return TCL_ERROR;
    } else if (strncmp (argv[1], "destroy", len) == 0) {
	
	/*
	 * If this is an agent session and the agent interpreter
	 * is the same as this one, make sure that the command
	 * is not removed twice. Therefore, we set the agent
	 * interpreter pointer to NULL.
	 */

	if (session->agentInterp == interp) {
	    session->agentInterp = NULL;
	}
	Tcl_DeleteCommand (interp, argv[0]);
	return TCL_OK;
    }

    /*
     * all commands below need a well configured session
     */

    if (strncmp (argv[1], "cget", len) == 0) {
        if (argc != 3) {
	    Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			      " cget option\"", (char *) NULL);
	    return TCL_ERROR;
        }
        return ShowOption (interp, session, argv[2]);
    }

    if (Configured (interp, session) != TCL_OK) return TCL_ERROR;

    if (! strcmp (argv[1], "get")) {
	return Request (interp, session, SNMP_GET, argc-1, argv+1);

    } else if (! strcmp (argv[1], "getnext")) {
	return Request (interp, session, SNMP_GETNEXT, argc-1, argv+1);

    } else if (! strcmp (argv[1], "getbulk")) {
	return Request (interp, session, SNMPv2_GETBULK, argc-1, argv+1);

    } else if (! strcmp (argv[1], "set")) {
	return Request (interp, session, SNMP_SET, argc-1, argv+1);
	
    } else if (! strcmp (argv[1], "walk")) {
	return SNMPWalk (interp, session, argc-1, argv+1);

    } else if (! strcmp (argv[1], "inform")) {
	if (session->version == SNMPv1) {
	    Tcl_AppendResult (interp, "inform option not allowed on ",
			      "SNMPv1 handle \"", session->name, "\"",
			      (char *) NULL);
	    return TCL_ERROR;
	}  else if (session->version & SNMPv2) {
	    return Request (interp, session, SNMPv2_INFORM, argc-1, argv+1);
	} else {
	    Tcl_SetResult (interp, "unknown SNMP version", TCL_STATIC);
            return TCL_ERROR;
	}

    } else if (! strcmp (argv[1], "trap")) {
	if (session->version == SNMPv1) {
	    return Request (interp, session, SNMPv1_TRAP, argc-1, argv+1);
	} else if (session->version & SNMPv2) {
	    return Request (interp, session, SNMPv2_TRAP, argc-1, argv+1);
	} else {
	    Tcl_SetResult (interp, "unknown SNMP version", TCL_STATIC);
            return TCL_ERROR;
        }

    } else if (! strcmp (argv[1], "bind")) {
	int event;
	SNMP_Binding *bindPtr = session->bindPtr;
	if (argc < 4 || argc > 5) {
	    Tcl_AppendResult (interp, "wrong # of args: should be \"",
			      argv[0], " bind label event ?command?\"", 
			      (char *) NULL);
	    return TCL_ERROR;
	}
	event = SNMP_Str2Event (argv[3]);
	if (argv[2][0] == '\0') {
	    int mask = SNMP_TRAP_EVENT | SNMP_INFORM_EVENT
	               | SNMP_BEGIN_EVENT | SNMP_END_EVENT
		       | SNMP_RECV_EVENT | SNMP_SEND_EVENT;
	    if (! (event & mask)) {
		Tcl_AppendResult (interp, "unknown event \"", argv[3], 
				  "\"", (char *) NULL);
		return TCL_ERROR;
	    }
	    if (event & (SNMP_TRAP_EVENT |SNMP_INFORM_EVENT)) {
		if (SNMP_TrapSocket (interp) != TCL_OK) {
		    return TCL_ERROR;
		}
	    }
	    while (bindPtr) {
		if (bindPtr->event == event) break;
		bindPtr = bindPtr->nextPtr;
	    }
	    if (argc == 4) {
		if (bindPtr) {
		    Tcl_SetResult (interp, bindPtr->command, TCL_STATIC);
		}
	    } else {
		if (! bindPtr) {
		    bindPtr = (SNMP_Binding *) ckalloc (sizeof (SNMP_Binding));
		    memset ((char *) bindPtr, '\0', sizeof (SNMP_Binding));
		    bindPtr->event = event;
		    bindPtr->nextPtr = session->bindPtr;
		    session->bindPtr = bindPtr;
		}
		if (bindPtr->command) ckfree (bindPtr->command);
		bindPtr->command = ckstrdup (argv[4]);
	    }
	} else {

	    int mask = SNMP_GET_EVENT | SNMP_SET_EVENT | SNMP_CREATE_EVENT 
	      | SNMP_CHECK_EVENT | SNMP_COMMIT_EVENT | SNMP_ROLLBACK_EVENT;
	    
	    char *oidstr = MIB_Oid(argv[2], 0);
	    ASN1_OID *oid;
	    int code, oidlen;
	    
	    if (!oidstr) {
		Tcl_AppendResult(interp, "no object \"", argv[2], "\"",
				 (char *) NULL);
		return TCL_ERROR;
	    }
	    
	    if (! (event & mask)) {
		Tcl_AppendResult(interp, "unknown event \"", argv[3],
				 "\": use get, set, create, check, commit,",
				 " or rollback", (char *) NULL);
		return TCL_ERROR;
	    }
	    
	    if (argc == 5) {
		oid = ASN1_Str2Oid(oidstr, &oidlen);
		code = SNMP_CreateInstBinding(session, oid, oidlen,
					      event, argv[4]);
		if (code != TCL_OK) {
		    Tcl_AppendResult(interp, "unknown instance \"",
				     argv[2], "\"", (char *) NULL);
		    return TCL_ERROR;
		}
	    } else {
		char *cmd;
		oid = ASN1_Str2Oid(oidstr, &oidlen);
		cmd = SNMP_GetInstBinding(session, oid, oidlen, event);
		Tcl_SetResult(interp, cmd ? cmd : "", TCL_STATIC);
	    }
	}
	return TCL_OK;

#if 0
    /*
     * snmp subcommand "walk"
     */
    
    } else if (strcmp (argv[1], "mywalk") == 0) {
 	if (argc == 4) {
	    SNMP_AgentWalk (interp, argv[2], argv[3]);	
	    return TCL_OK;
	} else {
	    Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			      " walk varName body\"", (char *) NULL);
	    return TCL_ERROR;
	}
#endif

    /*
     * snmp subcommand "instance"
     */

    } else if (strcmp (argv[1], "instance") == 0) {
	int code;
        if (argc < 4 || argc > 5) {
	    Tcl_AppendResult (interp,  "wrong # args: should be \"",
			      argv[0], " instance oid varName ?defval?\"",
			      (char *) NULL);
	    return TCL_ERROR;
	}
	
        if (! session->agentInterp) {
	    Tcl_AppendResult (interp, "invalid agent session \"", 
			      session->name, "\"", (char *) NULL);
	    return TCL_ERROR;
	}
	code = SNMP_CreateInst (session->agentInterp, argv[2], argv[3],
				(argc > 4) ? argv[4] : "");
	if (code != TCL_OK) {
	    Tcl_SetResult (interp, session->agentInterp->result, TCL_VOLATILE);
	    Tcl_ResetResult (session->agentInterp);
	    return code;
	}
	return TCL_OK;
    }

    Tcl_AppendResult (interp, "bad option \"", argv[1], "\": should be ",
		      "configure, cget, wait, destroy, ",
                      "get, getnext, getbulk, set, trap, inform, walk, ",
                      "instance, or bind", (char *) NULL);
    return TCL_ERROR;
}


/*
 * WaitSession() processes events until either the list of outstanding
 * requests is empty or until the id is no longer in the request list.
 */

static int
WaitSession (session, id)
     SNMP_Session *session;
     char *id;
{
    u_int reqid = 0;
    char *name = ckstrdup (session->name);

    if (id) {
	char *p;
	for (p = id; isdigit (*p); p++) {
	    reqid = 10 * reqid + *p - '0';
	}
    }

    /*
     * Do not use the session pointer! We have to search for the
     * session name after each single event because the session
     * may be deleted as a side effect of the event.
     */
    
  repeat:
    for (session = sessionList; session; session = session->nextPtr) {
	if (strcmp (session->name, name) != 0) continue;
	if (! id && session->requestList) {
	    Tk_DoOneEvent (0);
	    goto repeat;
	} else {
	    SNMP_Request *rPtr;
	    for (rPtr = session->requestList; rPtr; rPtr = rPtr->nextPtr) {
		if (rPtr->reqid == reqid) {
		    Tk_DoOneEvent (0);
		    goto repeat;
		}
	    }
	}
    }
    
    ckfree (name);
    return TCL_OK;
}


/*
 * DestroySession() is invoked when a session handle is deleted. It
 * frees the associated session structure and de-installs all
 * pending events. If it is the last session, we also close the
 * socket.
 */

static void
DestroySession (clientData)
     ClientData clientData;
{
    SNMP_Session *p, *q, *session = (SNMP_Session *) clientData;

    if (session->agentInterp) {
	Tcl_Interp *agentInterp = session->agentInterp;
	session->agentInterp = NULL;
	Tcl_DontCallWhenDeleted (agentInterp, DeleteAgentInterp,
				 (ClientData) session);
	Tcl_DeleteCommand (agentInterp, session->name);
    }
    
    for (p = sessionList, q = NULL; p != NULL; q = p,p = p->nextPtr) {
	if (p == session) break;
    }
    if (!p) return;

    if (q == NULL) {
	sessionList = p->nextPtr;
    } else {
	q->nextPtr = p->nextPtr;
    }

    SNMP_FreeSession (session);

    if (sessionList == NULL) {
	SNMP_ManagerClose();
    }
}


/*
 * Request() creates a pdu structure an calls SNMP_Encode to send the
 * packet to the destination.
 */

static int
Request (interp, session, pdu_type, argc, argv)
     Tcl_Interp	*interp;
     SNMP_Session *session;
     int pdu_type;
     int argc;
     char **argv;
{
    char *cmd = NULL;
    SNMP_PDU _pdu;
    SNMP_PDU *pdu = &_pdu;
    
    /* initialize the PDU */

    pdu->addr	      = session->tAddr;
    pdu->type         = pdu_type;
    pdu->request_id   = ++session->reqid;
    pdu->error_status = E_NOERROR;
    pdu->error_index  = 0;    
    pdu->trapOID      = NULL;
    Tcl_DStringInit (&pdu->varbind);

    /*
     * check # of arguments
     */
    
    if ((pdu->type == SNMPv2_GETBULK && argc < 4) 
	|| (pdu->type == SNMPv1_TRAP && argc < 3)
	|| (pdu->type == SNMPv2_TRAP && argc < 3)
	|| (pdu->type == SNMPv2_INFORM && argc < 3)
	|| (argc < 2)) {
	goto usage;
    }
    
    /*
     * read NonRepeaters and MaxRepetitions for GetBulkRequest
     */
    
    if (pdu->type == SNMPv2_GETBULK) {
	int num;
	if (--argc) {
	    if (Tcl_GetInt (interp, *++argv, &num) != TCL_OK) goto errorExit;
	    pdu->error_status = (num < 0) ? 0 : num;
	}
	if (--argc) {
	    if (Tcl_GetInt (interp, *++argv, &num) != TCL_OK) goto errorExit;
	    pdu->error_index  = (num < 0) ? 0 : num;
	}
    } else if (pdu->type == SNMPv1_TRAP 
	       || pdu->type == SNMPv2_TRAP
	       || pdu->type == SNMPv2_INFORM) {
	argc--;
	if (ASN1_IsOid (*++argv)) {
	    pdu->trapOID = ckstrdup(*argv);
	} else {
	    char *tmp = MIB_Oid (*argv, 0);
	    if (! tmp) {
		Tcl_AppendResult (interp,  "no object \"", *argv, "\"",
				  (char *) NULL);
		goto errorExit;	
	    }    
	    pdu->trapOID = ckstrdup (tmp);
	}
    } else {
	pdu->error_status = E_NOERROR;
	pdu->error_index  = 0;
    }
    
    /*
     * check for availability of VB-LIST and split it into args
     */
    
    if (!argc) goto usage;    
    Tcl_DStringAppend (&pdu->varbind, *++argv, -1);

    /*
     * check for availabilty of callback functions
     */
    
    if (--argc && *++argv != NULL) {
	cmd = *argv;
    }

    if (SNMP_Encode (interp, session, pdu, cmd) != TCL_OK) {
	goto errorExit;
    }

    if (pdu->trapOID) ckfree (pdu->trapOID);
    Tcl_DStringFree (&pdu->varbind);
    return TCL_OK;
    
  usage:
    if (pdu->type == SNMPv2_GETBULK) {
	Tcl_AppendResult (interp, "wrong # args: should be \"", session->name,
			  " getbulk non-repeaters max-repetitions ",
			  "list ?callback?\"", (char *) NULL);
    } else if (pdu->type == SNMPv1_TRAP 
	       || pdu->type == SNMPv2_TRAP) {
	Tcl_AppendResult (interp, "wrong # args: should be \"", session->name,
			  " trap snmpTrapOID list\"", (char *) NULL);
    } else if (pdu->type == SNMPv2_INFORM) {
	Tcl_AppendResult (interp, "wrong # args: should be \"", session->name,
                          " inform snmpTrapOID list\"", (char *) NULL);
    } else {
	Tcl_AppendResult (interp, "wrong # args: should be \"", session->name,
			  " ", *argv, " ",
			  "list ?callback?\"", 
			  (char *) NULL);
    }
    
  errorExit:
    if (pdu->trapOID) ckfree (pdu->trapOID);
    Tcl_DStringFree (&pdu->varbind);
    return TCL_ERROR;
}


/*
 * SNMPWalk() walks a MIB tree. It evaluates the given tcl command foreach
 * varbind retrieved using getbulk requests. First, all variables
 * contained in the list argument are converted to their OIDs. Then we
 * loop using gebulk requests until we get an error or until one returned
 * variable starts with an OID not being a valid prefix.
 */

static int
SNMPWalk (interp, session, argc, argv)
     Tcl_Interp *interp;
     SNMP_Session *session;
     int argc;
     char **argv;
{
    int i, j, k, result;
    int oidc, respc;
    char **oidv = NULL, **respv = NULL;
    SNMP_PDU _pdu, *pdu = &_pdu;
    
    if (argc != 4) {
	Tcl_AppendResult (interp, "wrong # args: should be \"",
			  session->name, " walk varName list command\"",
			  (char *) NULL);
	return TCL_ERROR;
    }

    /*
     * Initialize the PDU.
     */

    pdu->addr         = session->tAddr;
    pdu->type         = SNMPv2_GETBULK;
    pdu->request_id   = ++session->reqid;
    pdu->error_status = E_NOERROR;
    pdu->error_index  = 0;    
    pdu->trapOID      = NULL;
    Tcl_DStringInit (&pdu->varbind);

    /*
     * save the oid prefix contained in list in oidv and oidc
     */
    
    result = Tcl_SplitList(interp, argv[2], &oidc, &oidv);
    if (result != TCL_OK) {
	return result;
    }
    if (oidc == 0) {
	result = TCL_OK;
	goto loopDone;
    }
    
    for (i = 0; i < oidc; i++) {
	char *tmp = MIB_Oid (oidv[i], 0);
	if (!tmp) {
	    Tcl_AppendResult (interp,  "no object \"", oidv[i], "\"",
			      (char *) NULL);
	    ckfree ((char *) oidv);
	    Tcl_DStringFree (&pdu->varbind);
            return TCL_ERROR;
	}
	oidv[i] = ckalloc (strlen (tmp) + 2);
	strcpy (oidv[i], tmp);
	strcat (oidv[i], ".");
	Tcl_DStringAppendElement (&pdu->varbind, tmp);
    }

    while (1) {

	pdu->type         = SNMPv2_GETBULK;
	pdu->request_id   = ++session->reqid;

	/* 
	 * Set the non-repeaters and the max-repetitions for the getbulk
	 * operation. I do not know if 16 is a good constant. Perhaps we
	 * should start with a small value and increase in every loop?
	 */

	pdu->error_status = 0;
	pdu->error_index  = (16 / oidc > 0) ? 16 / oidc : 1;

	result = SNMP_Encode (interp, session, pdu, NULL);
	if (result == TCL_ERROR 
	    && (strncmp (interp->result, "noSuchName ", 11) == 0)) {
	    result = TCL_OK;
	    goto loopDone;
	}
	if (result != TCL_OK) {
            break;
        }
	
	if (respv) ckfree ((char *) respv);
	result = Tcl_SplitList (interp, interp->result, &respc, &respv);
	if (result != TCL_OK) {
	    goto loopDone;
	}

	if (respc % oidc) {
	    Tcl_SetResult (interp, 
			   "received response with wrong # of varbinds",
			   TCL_STATIC);
	    result = TCL_ERROR;
	    goto loopDone;
	}

	for (j = 0; j < respc / oidc; j++) {

	    for (i = 0; i < oidc; i++) {
		if (strncmp (oidv[i], respv[j * oidc + i], 
			     strlen(oidv[i])) != 0) {
		    result = TCL_OK;
		    goto loopDone;
		}
	    }

	    Tcl_DStringFree (&pdu->varbind);
	    for (k = j * oidc; k < (j+1) * oidc; k++) {
		int vbc;
		char **vbv;
		result = Tcl_SplitList (interp, respv[k], &vbc, &vbv);
		if (result != TCL_OK) {
		    goto loopDone;
		}
		if (strcmp (vbv[1], "endOfMibView") == 0) {
		    ckfree ((char *) vbv);
		    result = TCL_OK;
		    goto loopDone;
		}
		ckfree ((char *) vbv);
		Tcl_DStringAppendElement (&pdu->varbind, respv[k]);
	    }

	    if (Tcl_SetVar (interp, argv[1], Tcl_DStringValue(&pdu->varbind),
			    TCL_LEAVE_ERR_MSG) == NULL) {
		result = TCL_ERROR;
		goto loopDone;
	    }

	    result = Tcl_Eval (interp, argv[3]);
	    if (result != TCL_OK) {
		if (result == TCL_CONTINUE) {
		    result = TCL_OK;
		} else if (result == TCL_BREAK) {
		    result = TCL_OK;
		    goto loopDone;
		} else if (result == TCL_ERROR) {
		    char msg[100];
		    sprintf(msg, "\n    (\"%s walk\" body line %d)",
			    session->name, interp->errorLine);
		    Tcl_AddErrorInfo(interp, msg);
		    goto loopDone;
		} else {
		    goto loopDone;
		}
	    }
	}
    }

  loopDone:
    for (i = 0; i < oidc; i++) {
	ckfree (oidv[i]);
    }
    ckfree ((char *) oidv);
    if (respv)	ckfree ((char *) respv);
    Tcl_DStringFree (&pdu->varbind);

    /*
     * noSuchName errors mark the end of a SNMPv1 MIB view and hence 
     * they are no real errors. So we ignore them here.
     */

    if (result == TCL_ERROR 
	&& (strncmp (interp->result, "noSuchName", 10) == 0)) {
	result = TCL_OK;
    }

    if (result == TCL_OK) {
	Tcl_ResetResult(interp);
    }
    return result;
}
