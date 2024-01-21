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
/* $Id: object.c,v 1.143 1996/07/15 06:41:44 janssen Exp $ */
/* Last edited by Mike Spreitzer June 28, 1996 8:07 am PDT */

#define _POSIX_SOURCE

#ifdef MACOS
#pragma segment ilu
#endif

#include "iluntrnl.h"

#include <ctype.h>

#include "object.h"
#include "server.h"
#include "type.h"
#include "protocol.h"
#include "port.h"
#include "vector.h"

#include "version.h"	/* defines ILU_VERSION */

/*L1, L2, Main unconstrained*/

static const ilu_string _ilu_id = "$" "Id: ILU version " ILU_VERSION_STRING ".  Copyright 1990-1996 Xerox Corporation.  All Rights Reserved. $" ;
static const ilu_string _ilu_ilu_version = ILU_VERSION_STRING ;
static const ilu_cardinal _ilu_ilu_major_version = ILU_MAJOR_VERSION ;
static const ilu_cardinal _ilu_ilu_minor_version = ILU_MINOR_VERSION ;

/*L1 >= {cmu}*/
static ilu_ObjectNoter theNoters[MAX_LANGUAGES] = {NULLFN};
/* The proc to call when the "very interested" predicate changes. */

/*L1_sup < cmu*/
void ilu_SetNoter(ilu_ObjectNoter n, ilu_cardinal lang)
{
  _ilu_Assert(n!=NULLFN, "SetNoter: given NIL noter");
  _ilu_Assert(lang<=_ilu_NLanguages, "SetNoter: unknown language specified");
  _ilu_Assert(theNoters[lang]==NULLFN, "SetNoter: already set");
  _ilu_AcquireMutex(ilu_cmu);
  theNoters[lang] = n;
  _ilu_ReleaseMutex(ilu_cmu);
  return;
}

/*L1, L2, Main unconstrained*/

ilu_string ilu_GetILUVersion (void)
{
  return (_ilu_ilu_version);
}

ilu_cardinal ilu_GetILUMajorVersion (void)
{
  return (_ilu_ilu_major_version);
}

ilu_cardinal ilu_GetILUMinorVersion (void)
{
  return (_ilu_ilu_minor_version);
}

#define ISSAFE(c)	(isalnum(c)||((c)=='$')||((c)=='.')||((c)=='+')||((c)=='-')||((c)=='_'))

static const char hextable[] = "0123456789ABCDEF";
#define HEXDIGIT(x)	(hextable[x])
#define HEXVALUE(x)	\
	 (((x)>='0'&&(x)<='9')?((x)-'0')\
	:(((x)>='A'&&(x)<='F')?((x)-'A'+10)\
	:(((x)>='a'&&(x)<='f')?((x)-'a'+10):16)))

/*
 * Copy s into b, quoting unsafe chars.  Return TRUE iff b is long
 * enough.
 */
static          ilu_boolean
QuoteBuffer(char *s, ilu_cardinal slen,
	    char *b, ilu_cardinal blen)
{
  char           *sp = s, *slim = s + slen, *bp = b, *blim = b + blen;

  for (; sp < slim; sp++)
    if (ISSAFE(*sp)) {
      if (bp >= blim)
	return FALSE;
      *bp++ = *sp;
    } else {
      if (bp + 3 > blim)
	return FALSE;
      *bp++ = '%';
      *bp++ = HEXDIGIT((*sp >> 4) & 0xF);
      *bp++ = HEXDIGIT(*sp & 0xF);
    }
  if (bp >= blim)
    return FALSE;
  *bp++ = 0;
  return TRUE;
}

static char    *
Encode(char *b, ilu_cardinal len,
       ILU_ERRS((internal, no_memory)) * err)
{
  char           *p, *plim = b + len, *ans;
  ilu_cardinal   count = 0, i;
  for (p = b; p < plim; p++) {
    if (!ISSAFE(*p))
      count++;
  }
  ans = (char *) ilu_MallocE(i = len + (count * 2) + 1, err);
  if (ans == NIL)
    return NIL;
  if (!ilu_Check(QuoteBuffer(b, len, ans, i), err))
    return NIL;
  return ans;
}

/*
 * Copy s into b, turning quoted chars into plain chars; return next
 * position in b after terminating 0.  Raise inv_objref/minor on
 * quoting botch.  Raise internal/check if b isn't long enough.
 */
static char    *
DeQuoteBuffer(char *s, ilu_cardinal slen,
	      char *b, ilu_cardinal blen,
	      ILU_ERRS((inv_objref, internal)) * err,
	      ilu_inv_objref_Minor minor)
{
  char           *sp = s, *slim = s + slen, *bp = b, *blim = b + blen;
  for (; (sp < slim) && (bp < blim); bp++)
    if (*sp == '%') {
      int             hv1, hv2;
      if ((slim - sp <= 2)
	  || ((hv1 = HEXVALUE(sp[1])) == 16)
	  || ((hv2 = HEXVALUE(sp[2])) == 16))
	return ILU_ERR_CONS1(inv_objref, err, minor, minor, NIL);
      *bp = (char) ((hv1 << 4) + hv2);
      sp += 3;
    } else
      *bp = *sp++;
  if (!ilu_Check(sp == slim && bp < blim, err))
    return NIL;
  *bp++ = 0;
  return (bp);
}

static char    *
Decode(char *b, ilu_cardinal len,
       ILU_ERRS((internal, no_memory, inv_objref)) * err,
       ilu_inv_objref_Minor minor)
{
  char           *p, *plim = b + len, *ans;
  ilu_cardinal   count = 0, i;
  for (p = b; p < plim;)
    if (*p == '%') {
      count++;
      p += 3;
    } else
      p++;

  ans = ilu_MallocE(i = len - (count * 2) + 1, err);
  if (ans == NIL)
    return NIL;
  if (DeQuoteBuffer(b, len, ans, i, err, minor) == NIL)
    return NIL;
  return ans;
}

static ilu_boolean
  Parse_ILU_SBH(ilu_string encodedSBH,
		ilu_string * plainInstanceHandle,
		ilu_string * plainServerID,
		ilu_string * plainMstid,
		ilu_string * encodedContactInfo,
		ilu_cardinal * encodedContactInfoLen,
		ILU_ERRS((no_memory, internal, inv_objref)) * err)
{
  char           *p, *sid, *ih, *mstid, *cinfo;

  ILU_CLER(*err);

  DEBUG(OBJECT_DEBUG,
	(stderr, "(Parse_ILU_SBH):  sbh=<%s>\n", encodedSBH));

  if (strncmp(encodedSBH, "ilu:", 4) != 0)
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_bad_url_scheme,
			 ilu_FALSE);

  /* server ID */
  sid = encodedSBH + 4;
  if ((p = strchr(sid, '/')) == NIL)
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_sid, ilu_FALSE);
  DEBUG(OBJECT_DEBUG,
	(stderr, "(Parse_ILU_SBH):  encoded sid=<%*.*s>\n",
	 p - sid, p - sid, sid));
  if (plainServerID != NIL) {
    *plainServerID = Decode(sid, p - sid, err, ilu_iom_sid);
    if (*plainServerID == NIL)
      return ilu_FALSE;
  };

  /* instance handle */
  ih = p + 1;
  if ((p = strchr(ih, ILU_TYPE_MARKER)) == NIL) {
    if (plainServerID != NIL)
      ilu_free(*plainServerID);
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ih, ilu_FALSE);
  }
  DEBUG(OBJECT_DEBUG,
	(stderr, "(Parse_ILU_SBH):  encoded ih=<%*.*s>\n",
	 p - ih, p - ih, ih));
  if (plainInstanceHandle != NIL) {
    *plainInstanceHandle = Decode(ih, p - ih, err, ilu_iom_ih);
    if (*plainInstanceHandle == NIL) {
      if (plainServerID != NIL)
	ilu_free(*plainServerID);
      return ilu_FALSE;
    }
  };

  /* Most Specific Type ID */
  mstid = p + 1;
  if ((p = strchr(mstid, ILU_CINFO_MARKER)) == NIL) {
    if (plainServerID != NIL)
      ilu_free(*plainServerID);
    if (plainInstanceHandle != NIL)
      ilu_free(*plainInstanceHandle);
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_mstid, FALSE);
  }
  DEBUG(OBJECT_DEBUG,
	(stderr, "(Parse_ILU_SBH):  encoded mstid=<%*.*s>\n",
	 p - mstid, p - mstid, mstid));
  if (plainMstid != NIL) {
    *plainMstid = Decode(mstid, p - mstid, err, ilu_iom_mstid);
    if (*plainMstid == NIL) {
      if (plainServerID != NIL)
	ilu_free(*plainServerID);
      if (plainInstanceHandle != NIL)
	ilu_free(*plainInstanceHandle);
      return ilu_FALSE;
    }
  };

  /* Contact Info.s */
  cinfo = p + 1;
  DEBUG(OBJECT_DEBUG,
	(stderr, "(Parse_ILU_SBH):  encoded cinfo=<%s>\n", cinfo));
  if (encodedContactInfo != NIL)
    *encodedContactInfo = cinfo;
  if (encodedContactInfoLen != NIL)
    *encodedContactInfoLen = strlen(cinfo);

  return (ilu_TRUE);
}

static HashTable RegisteredParsers = NIL;

void ilu_RegisterSBHParser (ilu_string sbh_scheme, ilu_SBHParser parser)
{
  if (RegisteredParsers == NIL)
    RegisteredParsers = _ilu_hash_MakeNewTable(13, _ilu_hash_HashString,
					       _ilu_hash_StringCompare);
  _ilu_hash_AddToTable (RegisteredParsers, _ilu_Strdup(sbh_scheme), (ilu_private) parser);
}

ilu_boolean
  ilu_ParseSBH(ilu_string encodedSBH,
	       ilu_string * plainInstanceHandle,
	       ilu_string * plainServerID,
	       ilu_string * plainMstid,
	       ilu_string * encodedContactInfo,
	       ilu_cardinal * encodedContactInfoLen,
	       ILU_ERRS((no_memory, internal, inv_objref)) * err)
{
  char *ptr;
  char scheme_buffer[128];
  ilu_SBHParser parser;
  static ilu_boolean initted = ilu_FALSE;

  if (! initted)
    {
      ilu_RegisterSBHParser ("ilu", Parse_ILU_SBH);
#ifdef IIOP_PROTOCOL
      ilu_RegisterSBHParser ("IOR", _ilu_IIOP_ParseIOR);
      ilu_RegisterSBHParser ("ior", _ilu_IIOP_ParseIOR);
      ilu_RegisterSBHParser ("iiop", _ilu_IIOP_ParseIIOP);
#endif
#ifdef HTTP_PROTOCOL
      ilu_RegisterSBHParser ("http", _ilu_Parse_HTTP_URL);
#endif
      initted = ilu_TRUE;
    }

  ptr = strchr (encodedSBH, ':');
  if (ptr == NIL || (ptr - encodedSBH) > 127)
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_bad_url_scheme, ilu_FALSE);
  strncpy (scheme_buffer, encodedSBH, (ptr - encodedSBH));
  scheme_buffer[ptr - encodedSBH] = 0;
  parser = (ilu_SBHParser) _ilu_hash_FindInTable (RegisteredParsers, scheme_buffer);
  if (parser == NULLFN)
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_bad_url_scheme, ilu_FALSE);
  else
    return (*parser)(encodedSBH, plainInstanceHandle, plainServerID, plainMstid,
		     encodedContactInfo, encodedContactInfoLen, err);
}

ilu_boolean 
_ilu_ParseConnectInfo(ilu_string cinfo,
		      ilu_cardinal cinfolen,
		      ilu_string * plainProtocolInfo,
		      ilu_TransportInfo * transportInfo,
		      ILU_ERRS((no_memory, inv_objref,
				internal)) * err)
{
  char           *plim, *pstart, *pnext;
  if ((plim = strchr(cinfo, ILU_CINFO_MARKER)) == NIL
      || plim > cinfo + cinfolen)
    plim = cinfo + cinfolen;
  if ((pnext = strchr(cinfo, ILU_CINFO_DIVIDER)) == NIL
      || pnext > plim)
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_ci, FALSE);
  if (plainProtocolInfo != NIL) {
    *plainProtocolInfo = Decode(cinfo, pnext - cinfo, err, ilu_iom_ci);
    if (*plainProtocolInfo == NIL)
      return ilu_FALSE;
  }
  pstart = pnext + 1;
  if (transportInfo != NIL) {
    char           *buf;
    ilu_cardinal    count = 1, len;

    for (buf = strchr(pstart, ILU_TINFO_DIVIDER);
	 buf != NIL && buf < plim;
	 buf = strchr(buf + 1, ILU_TINFO_DIVIDER))
      count++;			/* count filters in tinfo string */
    /*
     * Allocate one piece of memory for both the vector and the
     * strings.
     */
    len = ((count + 1) * sizeof(ilu_string)) + (plim - pstart + 1);
    *transportInfo = (ilu_TransportInfo) ilu_MallocE(len, err);
    if (*transportInfo == NIL) {
      if (plainProtocolInfo != NIL)
	ilu_free(*plainProtocolInfo);
      return ilu_FALSE;
    }
    buf = (char *) ((*transportInfo) + count + 1);
    count = 0;
    while (pstart < plim) {
      pnext = strchr(pstart, ILU_TINFO_DIVIDER);
      if (pnext == NIL || pnext > plim)
	pnext = plim;
      (*transportInfo)[count] = buf;
      buf = DeQuoteBuffer(pstart, pnext - pstart, buf, 1 + pnext - pstart,
			  err, ilu_iom_ti);
      if (buf == NIL)
	return FALSE;
      count++;
      pstart = pnext + 1;
    }
    (*transportInfo)[count] = NIL;
  }
  return ilu_TRUE;
}

/*L1, L2, Main unconstrained*/

/*
 * This procedure is used internally for creating both true and
 * surrogate objects.  For true objects, this procedure is called
 * with non-NIL ih, server, cl, and lspo; for surrogate objects,
 * this procedure is called with non-NIL ih, server, and sbh.
 * Storage for ih, sbh, and mstid is owned by the caller.
 */
static ilu_Object CreateObject (ilu_string ih, ilu_Server server,
				ilu_Class class, ilu_refany lspo,
				ilu_string sbh, ilu_string mstid)
{
  ilu_Object new = (ilu_Object) ilu_must_malloc(sizeof(struct _ilu_Object_s));
  ilu_Alarmette_s gco = {NIL, NIL, FALSE, {0, 0}};
  unsigned        i;
  
  object_ih(new)	= _ilu_Strdup(ih);
  object_server(new)	= server;
  object_timeout(new)	= 1200;
  object_class(new)	= class;
  object_mstid(new)	= _ilu_Strdup(mstid);
  object_sbh(new)	= _ilu_Strdup(sbh);
  for (i = 0; i < MAX_LANGUAGES; i++)
    object_lspos(new)[i] = NIL;
  if (server_is_true(server))
    object_lspo(new, server_true_language(server)) = lspo;
  object_holds(new)	= 0;
  object_intNoted(new)	= (int) FALSE;
  object_notifying(new)	= FALSE;
  object_known(new)	= FALSE;
  object_gco(new)	= gco;
  object_lastRemote(new) = 0;
  object_gclist(new)	= NIL;
  return (new);
}

/*Destroy an object created but not yet put into the server's object table. */
static void UncreateObject(ilu_Object o)
{
  ilu_free(o->ob_ih);
  if (o->ob_mstid != NIL)
    ilu_free(o->ob_mstid);
  if (o->ob_sbh != NIL)
    ilu_free(o->ob_sbh);
  ilu_free(o);
}

/*L1 >= {s}*/

ilu_boolean _ilu_Addable(ilu_Server s, ilu_Class t, ilu_Object *h)
{
  ilu_cardinal i, l;
  if (!class_singleton(t))
      return TRUE;
  if (  (*h = (ilu_Object) _ilu_hash_FindInTable(server_singles(s), t))
	!= NIL)
      return FALSE;
  l = class_superclass_count(t);
  for (i = 0; i < l; i++)
      if (!_ilu_Addable(s, class_superclass(t, i), h))
          return FALSE;
  return TRUE;
}

/*
 * Bug: if the ancestors don't form a tree, some ancestors will be
 * visited more than once, and the later visits will fail in
 * _ilu_hash_AddToTable.
 */
void 
_ilu_AddSingleton(ilu_Server s, ilu_Class t, ilu_Object o)
{
  ilu_cardinal i, l;
  if (!class_singleton(t))
    return;
  _ilu_Assert((int) _ilu_hash_AddToTable(server_singles(s), t, o),
	      "AddSingleton");
  l = class_superclass_count(t);
  for (i = 0; i < l; i++)
    _ilu_AddSingleton(s, class_superclass(t, i), o);
  DEBUG(OBJECT_DEBUG,
	(stderr, "_ilu_AddSingleton (server \"%s\", class \"%s\", object %p \"%s\")\n",
	 s->sr_id, t->cl_name, o, (o->ob_ih == NIL) ? "(unset)" : o->ob_ih));
  return;
}

/*L1 >= {the object's server};
  L1 >= {gcmu} if cl collectible*/
ilu_Object ilu_FindOrCreateTrueObject (ilu_string ih, ilu_Server server,
					ilu_Class cl, ilu_refany lspo)
{
  ilu_Object      new;
  if (server == NIL)
    return (NIL);
  _ilu_Assert(ih != NIL, "CreateTrueObject: ih==NIL");
  _ilu_Assert(lspo != NIL, "CreateTrueObject: lspo==NIL");
  _ilu_Assert(cl != NIL, "CreateTrueObject: class==NIL");
  _ilu_HoldMutex(server_lock(server));
  new = (ilu_Object) _ilu_hash_FindInTable(server_objs(server), ih);
  if (new != NIL) {
    if (new->ob_class != cl) {
      DEBUG(OBJECT_DEBUG,
	    (stderr,
	     "FindOrCreateTrueObject: type of %s/%s is %s, not %s",
	     server->sr_id, ih, new->ob_class->cl_unique_id,
	     cl->cl_unique_id));
      return (NIL);
    }
    return (new);
  }
  if (server->sr_closing == TRUE) {
    DEBUG(OBJECT_DEBUG,
	  (stderr,
	   "ilu_CreateTrueObject: invoked on closing server %s.\n",
	   server->sr_id));
    return (NIL);
  }
  if (!_ilu_Addable(server, cl, &new)) {
    DEBUG(OBJECT_DEBUG,
	  (stderr,
	   "%s %s %s of type %s in server %s because %s %s.\n",
	   "ilu_CreateTrueObject: can't create another",
	   "singleton object", ih, cl->cl_unique_id, server->sr_id,
	   new->ob_ih, "already exists"));
    return (NIL);
  }
  new = CreateObject(ih, server, cl, lspo, NIL, cl->cl_unique_id);
  DEBUG(OBJECT_DEBUG,
	(stderr,
	 "ilu_FindOrCreateTrueObject (ih=\"%s\", server=\"%s\", class=\"%s\", lspo=%p) => %p\n",
	 ih, server->sr_id, cl->cl_name, lspo, new));
  _ilu_Assert((int) _ilu_hash_AddToTable(server_objs(server),
					 object_ih(new), new),
	      "FindOrCreateTrueObject: AddToTable failed");
  _ilu_AddSingleton(server, cl, new);
  if (class_collectible(cl))
    _ilu_StartGCingTrueObj(new);
  return new;
}

/*before: Inside(s, static_type)
  after:  result!=NIL => Inside(s, static_type);
  after:  result==NIL => L1 = {};
  forall conn: (L2 >= {conn.iomu}) => (L2 >= {conn.callmu});
  Main otherwise unconstrained*/
ilu_Object _ilu_FindOrCreateObject (ilu_string ih, ilu_Server s,
				    ilu_Class found_class, ilu_Class static_type,
				    char *mstid, char *sbh, ilu_Error *err)
{
  ilu_Object      o = NIL;
  ilu_Class       cl = (found_class == NIL) ? static_type : found_class;
  ilu_boolean     tidfound = (found_class != NIL);

  ILU_CLER(*err);

  o = _ilu_FindObjectInServer(ih, s);

  if (o == NIL) {
    ilu_Object      o2;
    if (server_is_true(s)) {
      ilu_ExitServer(s, static_type);
    } else if (s->sr_closing == TRUE) {
      ilu_ExitServer(s, static_type);
      DEBUG(OBJECT_DEBUG,
	    (stderr, "%s (ih %s) for closing server %s.\n",
      "_ilu_FindOrCreateObject:  refusing to create new surrogate",
	     ih, s->sr_id));
    } else if (!_ilu_Addable(s, cl, &o2)) {
      ilu_ExitServer(s, static_type);
      DEBUG(OBJECT_DEBUG,
	    (stderr,
	     "%s %s of type %s in server %s because %s %s.\n",
	"_ilu_FindOrCreateObject:  won't create new singleton", ih,
	 cl->cl_unique_id, s->sr_id, o2->ob_ih, "already exists"));
    } else {
      o = CreateObject(ih, s, cl, NIL, sbh, mstid);
      DEBUG(OBJECT_DEBUG,
	    (stderr,
	     "_ilu_FindOrCreateObject:  "
	     "Created new surrogate object %p, ih <%s>, "
	     "on server <%s> (%p).\n",
	     o, ih, s->sr_id, s));
      if (mstid == NIL || !tidfound) {
	ilu_ExitServer(s, static_type);
#ifdef IIOP_PROTOCOL
	if ((strncmp (s->sr_id, "CORBA:", 6) == 0) &&
	    ih[0] == '$' && ih[1] == 0)
	  /* non-ILU object; use "-is-a" instead of "ILUGetTypes";
	     less efficient but works with non-ILU orbs */
	  cl = _ilu_IIOP_FindClassViaRPC(o);
	else
#endif
	  cl = _ilu_FindClassViaRPC(o);
	if (cl != NIL) {
	  object_class(o) = cl;
	  ilu_EnterServer(s, static_type);
	} else {
	  DEBUG(OBJECT_DEBUG,
		(stderr,
		 "_ilu_FindOrCreateObject:  Couldn't determine"
		 " type for object %p, given mstid \"%s\".\n",
		 o, mstid));
	  UncreateObject(o);
	  o = NIL;
	}
      }
      if (o != NIL) {
	if (s->sr_closing == TRUE) {
	  DEBUG(OBJECT_DEBUG,
		(stderr,
		 "_ilu_FindOrCreateObject:  %s (ih %s) %s %s.\n",
		 "refusing to create new surrogate",
		 ih, "for closing server", s->sr_id));
	  UncreateObject(o);
	  ilu_ExitServer(s, static_type);
	  o = NIL;
	} else if (NIL != (o2 = (ilu_Object) _ilu_hash_FindInTable
			   (server_objs(s), object_ih(o)))) {
	  UncreateObject(o);
	  o = o2;
	} else if (!_ilu_Addable(s, object_class(o), &o2)) {
	  DEBUG(OBJECT_DEBUG,
		(stderr,
		 "%s %s of type %s in server %s because %s %s.\n",
	    "_ilu_FindOrCreateObject:  won't create new singleton",
	    ih, object_class(o)->cl_unique_id, s->sr_id, o2->ob_ih,
		 "already exists"));
	  UncreateObject(o);
	  ilu_ExitServer(s, static_type);
	  o = NIL;
	} else {
	  _ilu_Assert((int) _ilu_hash_AddToTable(server_objs(s), object_ih(o), o),
	       "_ilu_FindOrCreateObject: add to cache (2) failed");
	  _ilu_AddSingleton(s, object_class(o), o);
	}
      }
    }
  }
  DEBUG(OBJECT_DEBUG, (stderr,
	       "_ilu_FindOrCreateObject:  Object of <%s/%s> is %p.\n",
		s->sr_id, ih, o));
  if (o == NIL) {
  } else if (o->ob_mstid == NIL && mstid != NIL)
    o->ob_mstid = _ilu_Strdup(mstid);
  else if (o->ob_mstid == NIL || mstid == NIL) {
  } else if (strcmp(o->ob_mstid, mstid) != 0) {
    DEBUG(OBJECT_DEBUG,
	  (stderr,
	   "_ilu_FindOrCreateObject: Existing object %s/%s"
	   " has mstid %s, not %s.\n",
	   s->sr_id, ih, o->ob_mstid, mstid));
  }
  if (o != NIL && !ilu_IsSubObjectType(o->ob_class, static_type)) {
    DEBUG(OBJECT_DEBUG,
	  (stderr,
	   "%s %s/%s has type %s:%s, not a subtype of %s:%s.\n",
	   "_ilu_FindOrCreateObject: Existing object",
	   s->sr_id, ih,
	   o->ob_class->cl_unique_id, o->ob_class->cl_name,
	   static_type->cl_unique_id, static_type->cl_name));
  }
  return (o);
}

/*before: L1 = {};
  after:  result!=NIL => Inside(result's server, cl);
  after:  result==NIL => L1 = {};
  Main Remnant holds; L2 otherwise unconstrained*/
ilu_Object 
ilu_ObjectOfSBH(ilu_string sbh, ilu_Class static_type,
		ILU_ERRS((bad_locks, broken_locks, inv_objref,
			  internal)) * err)
{
  ilu_string      ih = NIL, serverID = NIL, connectInfo = NIL;
  ilu_string      mstid = NIL;
  ilu_cardinal    cinfolen;
  ilu_Object      o = NIL;
  ilu_boolean     tidfound = FALSE;
  ilu_Class       c2;
  ilu_Server      s;

  _ilu_AutoSetDebugLevel();

  if (!ilu_Check(sbh != NIL, err))
    return NIL;
  if (!ilu_Check(static_type != NIL, err))
    return NIL;

  if (!ilu_ParseSBH(sbh, &ih, &serverID, &mstid, &connectInfo,
		    &cinfolen, err)
      || ih == NIL || serverID == NIL
      || connectInfo == NIL) {
    DEBUG(OBJECT_DEBUG,
	  (stderr,
	   "ilu_ObjectOfSBH: SBH parse of <%s> unsatisfactory.\n",
	   sbh));
    return NIL;
  }
  if (mstid == NIL)
    {
#ifdef IIOP_PROTOCOL
      if (strncmp(sbh, "IOR:", 4) == 0 ||
	  strncmp(sbh, "iiop:", 5) == 0 ||
	  strncmp(sbh, "ior:", 4) == 0)
	{
	  DEBUG(OBJECT_DEBUG,
		(stderr, "ilu_ObjectOfSBH:  no type info found in <%s>.  Using static type \"%s\".\n",
		 sbh, class_name(static_type)));
	  c2 = NIL;
	}
      else
#endif /* def IIOP_PROTOCOL */
	{
	  DEBUG(OBJECT_DEBUG,
		(stderr,
		 "ilu_ObjectOfSBH:  Error, no object type found in SBH <%s>.\n", sbh));
	  return NIL;
	}
    }
  else
    {
      c2 = ilu_FindClassFromID(mstid);
      if (c2 == NIL)
	c2 = _ilu_FindMSKA(mstid);
    }

  s = _ilu_FindServer(serverID, TRUE, connectInfo, cinfolen, err);
  if (ILU_ERRNOK(*err))
    return NIL;
  if (!ilu_Check(s != NIL, err))
    return NIL;
  if (!ilu_Check(ih != NIL, err))
    return NIL;

  ilu_EnterServer(s, static_type);

  if (s->sr_objs == NIL) {
    DEBUG(CONNECTION_DEBUG,
	  (stderr, "ilu_ObjectOfSBH:  %s (id=%s, true=%d).\n",
	   "asked for object in closed server", serverID,
	   server_is_true(s)));
    ilu_ExitServer(s, static_type);
    ilu_free(ih);
    ilu_free(serverID);
    ilu_free(mstid);
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_svr_closed, NIL);
  }
  DEBUG(CONNECTION_DEBUG,
	(stderr,
	 "ilu_ObjectOfSBH:  Server for id %s, ci %*.*s is %p.\n",
	 serverID, cinfolen, cinfolen, connectInfo, s));

  ilu_free(serverID);
  o = _ilu_FindOrCreateObject(ih, s, c2, static_type, mstid, sbh, err);
  if (ILU_ERRNOK(*err)) {
    DEBUG(OBJECT_DEBUG,
	  (stderr,
    "ilu_ObjectOfSBH:  Can't create object <%s> on server <%s>.\n",
	   ih, server_id(s)));
  }
  ilu_free(mstid);
  ilu_free(ih);
  return o;
}

/*before: L1 = {};
  after:  result!=NIL => Inside(result's server, cl);
  after:  result==NIL => L1 = {};
  Main Remnant holds; L2 otherwise unconstrained*/
ilu_Object 
  ilu_FindOrCreateSurrogate (ilu_Server server,
			     ilu_string ih,
			     ilu_Class type,
			     ILU_ERRS((bad_locks, broken_locks, inv_objref,
				       internal)) * err)
{
  ilu_Object o = NIL;

  _ilu_AutoSetDebugLevel();

  if (!ilu_Check(ih != NIL, err))
    return NIL;
  if (!ilu_Check(type != NIL, err))
    return NIL;
  if (!ilu_Check(server != NIL, err))
    return NIL;

  ilu_EnterServer(server, type);

  if (server->sr_objs == NIL) {
    DEBUG(OBJECT_DEBUG,
	  (stderr, "ilu_FindOrCreateSurrogate:  %s (id=%s, true=%d).\n",
	   "asked for object in closed server", server->sr_id,
	   server_is_true(server)));
    ilu_ExitServer(server, type);
    return ILU_ERR_CONS1(inv_objref, err, minor, ilu_iom_svr_closed, NIL);
  }

  o = _ilu_FindOrCreateObject(ih, server, NIL, type, NIL, NIL, err);

  DEBUG(OBJECT_DEBUG,
	(stderr, "ilu_FindOrCreateSurrogate (server=\"%s\", ih=\"%s\", type=\"%s\") => %p\n",
	 server->sr_id, ih, type->cl_name, o));
  if (ILU_ERRNOK(*err)) {
    DEBUG(OBJECT_DEBUG,
	  (stderr, "ilu_FindOrCreateSurrogate:  Error <%s>\n", ILU_ERR_NAME(*err)));
  }
  return o;
}

#define ADDTO(s,c)	(s)[strlen(s)+1]=0;(s)[strlen(s)]=(c)

/*L1 >= {obj's server}; L1_sup < prmu*/
ilu_string ilu_SBHOfObject (ilu_Object obj)
{
  ilu_Server      s;

  if (obj == NIL)
    return (NIL);

  s = object_server(obj);
  _ilu_HoldMutex(server_lock(s));
  if (object_sbh(obj) != NIL)
    0;
  else {
    ilu_string      ans;
    char           *pinfo;
    ilu_string      tinfos;
    ilu_TransportInfo tinfo;
    int             i;
    char           *mstid;
    ilu_Error       lerr;

    if (object_is_true(obj)) {
      if (server_default_port(s) != NIL) {
	ilu_Port        port = server_default_port(s);
	pinfo = protocol_form_handle(port_protocol(port),
				     obj);
	if (pinfo == NIL)
	  return NIL;
	tinfo = port_tinfo(port);
      }
    } else {
      pinfo = s->sr_pinfo;
      tinfo = s->sr_tinfo;
    }

    mstid = ilu_MstidOfObject(obj);
    mstid = Encode(mstid, strlen(mstid), &lerr);
    if (mstid == NIL) {
      ilu_DebugPrintf("ilu_SBHOfObject:  out of memory\n");
      ILU_HANDLED(lerr);
      return NIL;
    }
    tinfos = _ilu_StringifyTinfo(tinfo, &lerr);
    if (tinfos == NIL) {
      ilu_DebugPrintf("ilu_SBHOfObject:  out of memory\n");
      ILU_HANDLED(lerr);
      return NIL;
    }
    i = (strlen(s->sr_id) + strlen(obj->ob_ih) + strlen(mstid)
	 + strlen(pinfo) + strlen(tinfos) + 9);
    ans = ilu_malloc(i);
    if (ans == NIL) {
      ilu_DebugPrintf("ilu_SBHOfObject:  out of memory\n");
      ilu_free(mstid);
      return NIL;
    }
    strcpy(ans, "ilu:");
    strcat(ans, s->sr_id);	/* BUG: should encode */
    ADDTO(ans, '/');
    strcat(ans, obj->ob_ih);	/* BUG: should encode */
    ADDTO(ans, ILU_TYPE_MARKER);
    strcat(ans, mstid);
    ADDTO(ans, ILU_CINFO_MARKER);
    strcat(ans, pinfo);
    ADDTO(ans, ILU_CINFO_DIVIDER);
    strcat(ans, tinfos);
    object_sbh(obj) = ans;
    ilu_free(mstid);
    ilu_free(tinfos);
    if (pinfo != s->sr_pinfo)
      ilu_free(pinfo);
  }
  return (object_sbh(obj));
}

/*L1, L2, Main unconstrained*/

ilu_string ilu_MstidOfObject (ilu_Object obj)
{
  if (obj == NIL)
    return NIL;
  if (obj->ob_mstid == NIL)
    obj->ob_mstid = class_unique_id(object_class(obj));
  return (obj->ob_mstid);
}

ilu_Class ilu_ClassOfObject (ilu_Object obj)
{
  if (obj == NIL)
    return (NIL);
  else
    return (object_class(obj));
}

ilu_boolean ilu_TrueInstanceP (ilu_Object obj)
{
  ilu_boolean s = FALSE;

  if (obj != NIL)
    {
      s = object_is_true(obj);
    }
  return (s);
}

ilu_Server ilu_ServerOfObject (ilu_Object obj)
{
  if (obj == NIL)
    return (NIL);
  else
    return (object_server(obj));
}

ilu_string ilu_IhOfObject (ilu_Object obj)
{
  if (obj == NIL)
    return (NIL);
  else
    return (object_ih(obj));
}

/*L1 >= {cmu, s}; L1_sup < trmu*/
static ILU_ERRS((BadDataStructure, KernelBroken, bad_locks, broken_locks))
     DestroyObject(ilu_Server s, ilu_Object obj)
{
  ilu_Class       c = object_class(obj);
  ilu_Error       err;
  ASSERT(c != NIL, buf,
	 (buf, "object.c:DestroyObject: class(%s/%s) == NIL",
	  obj->ob_server->sr_id, obj->ob_ih));
  ASSERT(obj->ob_ih != NIL, buf,
	 (buf, "object.c:DestroyObject: ih(%s/%s) == NIL",
	  obj->ob_server->sr_id, obj->ob_ih));
  _ilu_HoldMutex(ilu_cmu);
  _ilu_HoldMutex(server_lock(s));
  DEBUG(OBJECT_DEBUG,
	(stderr, "DestroyObject (server=\"%s\", obj=%p, \"%s\")\n", s->sr_id, obj, obj->ob_ih));
  if (object_collectible(obj) && server_is_true(s))
    _ilu_StopGCingTrueObj(obj);
  err = _ilu_ServerRemoveObject(s, obj);
  ILU_ERR_SWITCH(err) {
    ILU_SUCCESS_CASE;
    ILU_ERR_CASE4(BadDataStructure, KernelBroken, bad_locks, broken_locks)
      return err;
  } ILU_ERR_ENDSWITCH;
  FREETOKEN(object_ih(obj));
  if (object_sbh(obj) != NIL)
    FREETOKEN(object_sbh(obj));
  ilu_free(obj);
  return ILU_NO_ERR;
}

/*L1 >= {s}*/
ilu_refany ilu_GetLanguageSpecificObject (ilu_Object obj, ilu_cardinal lang)
{
  ilu_refany s = NIL;
  _ilu_Assert(lang<=_ilu_NLanguages, "ilu_GetLanguageSpecificObject: unknown language specified");
  if (obj != NIL)
    {
      _ilu_HoldMutex(server_lock(object_server(obj)));
      s = object_lspo(obj, lang);
    }
  return (s);
}

/*Inside(obj's server, st); L1 no higher than required by that*/
static
ILU_ERRS((BadDataStructure, KernelBroken, GcRegFailed,
	  bad_locks, broken_locks))
IUpdate(ilu_Object obj, ilu_Class st)
{
  ilu_Server      s = object_server(obj);
  ilu_Error       ans = ILU_INIT_NO_ERR;
  unsigned int    i;

  if (object_notifying(obj))
    return ILU_NO_ERR;
  if (object_collectible(obj) && !object_is_true(obj)) {
    register ilu_boolean should = (object_holds(obj) != 0);
    for (i = 0; (!should) && (i < _ilu_NLanguages); i++)
      should = (obj->ob_lspos[i] != NIL);
    object_notifying(obj) = TRUE;
    while (should != object_known(obj) && ILU_ERROK(ans)) {
      ilu_ExitServer(s, st);
      if (should)
	ans = _ilu_RegisterGCInterest(obj);
      else
	ans = _ilu_UnregisterGCInterest(obj);
      ilu_EnterServer(s, st);
      if (ILU_ERROK(ans))
	object_known(obj) = should;
      should = (object_holds(obj) != 0);
      for (i = 0; (!should) && (i < _ilu_NLanguages); i++)
	should = (obj->ob_lspos[i] != NIL);
    }
    object_notifying(obj) = FALSE;
  }
  if (ILU_ERRNOK(ans))
    return ans;
  if (object_holds(obj) != 0)
    return ILU_NO_ERR;
  for (i = 0; i < _ilu_NLanguages; i++) {
    if (obj->ob_lspos[i] != NIL)
      return ILU_NO_ERR;
  }
  if (object_is_true(obj) && object_collectible(obj)) {
    if (object_gclist(obj) != NIL && _ilu_vector_size(object_gclist(obj)) != 0)
      return ILU_NO_ERR;
    if (ilu_CoarseTime_Now() < object_lastRemote(obj) + obj->ob_timeout)
      return ILU_NO_ERR;
  }
  DEBUG(OBJECT_DEBUG,
     (stderr, "IUpdate:  deleting uninteresting object %s/%s (%p).\n",
      s->sr_id, obj->ob_ih, obj));
  return DestroyObject(object_server(obj), obj);
}

/*Inside(obj's server, obj's type)*/

ILU_ERRS((BadDataStructure, KernelBroken, GcRegFailed,
	  bad_locks, broken_locks))
_ilu_VIUpdate(ilu_Object obj)
{
  int             vi;
  unsigned int    i;

  vi = ((object_holds(obj) != 0)
	|| (object_collectible(obj) &&
	    (object_is_true(obj)
	     ? ((object_gclist(obj) != NIL &&
		 _ilu_vector_size(object_gclist(obj)) != 0)
		|| (ilu_CoarseTime_Now() <
		    object_lastRemote(obj) + obj->ob_timeout))
	     : (object_notifying(obj) && !object_known(obj)))));
  if (vi != object_intNoted(obj)) {
    object_intNoted(obj) = vi;
    for (i = 0;  i < _ilu_NLanguages;  i++)
      {
	if (theNoters[i] != NULLFN)
	  (*theNoters[i]) (obj, vi);
      }
  }
  return IUpdate(obj, obj->ob_class);
}

void ilu_RegisterLanguageSpecificObject (ilu_Object obj, ilu_refany lso, ilu_cardinal lang)
{
  ilu_SetLSO(obj, obj->ob_class, lso, lang);
  return;
}

/*Inside(obj's server, t); L1 no higher than required by that*/
void ilu_SetLSO (ilu_Object obj, ilu_Class t, ilu_refany lso, ilu_cardinal lang)
{
  ilu_Error       err;
  extern ilu_string _ilu_LangNames[];

  _ilu_Assert(lang <= _ilu_NLanguages, "ilu_SetLSO: unknown language passed");
  obj->ob_lspos[lang] = lso;
  DEBUG(OBJECT_DEBUG, (stderr, "ilu_SetLSO (obj=%p \"%s\", lso=%p, lang=%s)\n",
		       obj, obj->ob_ih, lso, _ilu_LangNames[lang]));
  err = IUpdate(obj, t);
  ILU_ERR_SWITCH(err) {
    ILU_SUCCESS_CASE;
    ILU_ERR_CASE(GcRegFailed, e)
      _ilu_Assert(lso == NIL,
		  "Couldn't register GC interest in collectible instance (in ilu_SetLSO)");
  } ILU_ERR_ENDSWITCH;
  ILU_HANDLED(err);
}

void 
ilu_DHolds(ilu_Object obj, ilu_integer dholds)
{
  ilu_integer new_holds = object_holds(obj) + dholds;
  _ilu_Assert(new_holds>=0,"holds on object goes negative");
  object_holds(obj) = new_holds;
  return;
}

ILU_ERRS((BadDataStructure, KernelBroken, GcRegFailed,
	  bad_locks, broken_locks))
ilu_DeltaHolds(ilu_Object obj, ilu_integer dholds)
{
  return _ilu_DeltaHolds(obj, dholds);
}

ILU_ERRS((BadDataStructure, KernelBroken, GcRegFailed,
	  bad_locks, broken_locks))
_ilu_DeltaHolds(ilu_Object obj, ilu_integer dholds)
{
  ilu_integer new_holds = object_holds(obj) + dholds;
  _ilu_Assert(new_holds>=0,"holds on object goes negative");
  object_holds(obj) = new_holds;
  return _ilu_VIUpdate(obj);
}

/* a bit of scaffolding code for debugging the class record */
/*L1, L2, Main unconstrained; for calling from debugger*/
#ifdef NOPE
static void dump_class_record(ilu_Class class)
{
	ilu_DebugPrintf ("dumping class record:\n");
	ilu_DebugPrintf ("class name: %s\n",class->cl_name);
	ilu_DebugPrintf ("class brand: %s\n",class->cl_brand ? class->cl_brand : "");
	ilu_DebugPrintf ("class id: %s\n",class->cl_unique_id ? class->cl_unique_id : "");
	ilu_DebugPrintf ("class singleton == \"%s\"",
	       class->cl_singleton ? class->cl_singleton : "(not a singleton)");
	ilu_DebugPrintf ("class collectible == 0x%x\n",class->cl_collectible);
	ilu_DebugPrintf ("class authentication: %s\n",
	       class->cl_authentication ? class->cl_authentication : "");
	ilu_DebugPrintf ("class methods table: 0x%x\n",class->cl_methods);
	ilu_DebugPrintf ("class method count: %d\n",class->cl_method_count);
	ilu_DebugPrintf ("class superclass record: 0x%x\n",class->superclass);
	ilu_DebugPrintf ("class superclass name: 0x%x (%s)\n",class->superclassname,
	       class->superclassname ? class->superclassname : "");
}
#endif

/*L1, L2, Main unconstrained*/

/*Main Invariant holds; L2 otherwise unconstrained*/
ilu_boolean ilu_PingObject(ilu_Object o, ilu_Connection * new_conn)
{
  ilu_Call_s      call_s;
  ilu_Call        call = &call_s;
  ilu_cardinal    estatus = 0;
  ilu_ProtocolException internal;
  ilu_cardinal    reqSize;
  ilu_Class       pclass = object_class(o);
  ilu_Server      s = object_server(o);
  ilu_boolean     status = FALSE;
  ILU_ERRS((IoErrs, bad_locks, inv_objref, no_resources)) lerr;

  if (class_singleton(pclass))
    return (ilu_FALSE);

  DEBUG(OBJECT_DEBUG,
	(stderr, "ilu_PingObject:  object <%s>/<%s>...\n",
	 s->sr_id, o->ob_ih));

  if (server_is_true(s)) {
    ilu_refany      lspo;

    _ilu_AcquireMutex(server_lock(s));
    lspo = object_lspo(o, server_true_language(s));
    _ilu_ReleaseMutex(server_lock(s));
    return (lspo != NIL);
  } else {
    if (!ilu_StartCall(call, s, _ilu_rootClass, _ilu_PingMethod, 0,
		       NIL, new_conn, &lerr)) {
      DEBUG(OBJECT_DEBUG,
	    (stderr, "ilu_PingObject:  ilu_StartCall failed.\n"));
      ILU_HANDLED(lerr);
      return (ilu_FALSE);
    }
    _ilu_AcquireMutex(server_lock(s));
    reqSize = ilu_SizeOfObjectID(call, o, TRUE, _ilu_rootClass, &lerr);
    _ilu_ReleaseMutex(server_lock(s));
    if (ILU_ERRNOK(lerr))
      goto dun;
    if (!ilu_StartRequest(call, reqSize, &lerr))
      goto dun;
    ilu_EnterServer(s, pclass);
    ilu_OutputObjectID(call, o, TRUE, _ilu_rootClass, &lerr);
    if (ILU_ERRNOK(lerr))
      goto dun;
    if (!ilu_FinishRequest(call, &lerr))
      goto dun;
    internal = ilu_GetReply(call, &estatus, &lerr);
    _ilu_Assert((!ILU_ERROK(lerr) ==
		 (internal == ilu_ProtocolException_Not)),
		"GetReply botch error raise");
    if (internal != ilu_ProtocolException_Success)
      goto dun;
    ilu_ReplyRead(call, &lerr);
    if (ILU_ERRNOK(lerr))
      goto dun;
    status = (estatus == 0);
    DEBUG(OBJECT_DEBUG,
	  (stderr, "ilu_PingObject:  returns %u and %lu => %s.\n",
	internal, estatus, status ? "Good object" : "Bad object"));
dun:
    ilu_FinishCall(call, &lerr);
    if (ILU_ERRNOK(lerr)) {
      ILU_HANDLED(lerr);
      return FALSE;
    }
    return status;
  }
}

/*L2    >=    {conn's callmu, iomu} before,
  L2 disjoint {conn's callmu, iomu} after*/
void
_ilu_HandlePing(ilu_Call call)
{
  ilu_Object      disc;
  ilu_Error       lerr = ILU_INIT_NO_ERR;

  ilu_InputObjectID(call, &disc, TRUE, _ilu_rootClass, &lerr);
  if (ILU_ERRNOK(lerr)) goto dun;
  if (disc != NIL) {
    lerr = ilu_DeltaHolds(disc, 1);
    ILU_MUST_BE_SUCCESS(lerr);
    ilu_ExitServer(object_server(disc), _ilu_rootClass);
  }
  if (!ilu_RequestRead(call, &lerr))
    goto dun;

  if ((disc != NIL) AND ilu_TrueInstanceP(disc)) {
    ilu_cardinal rsize = ilu_BeginSizingReply (call, FALSE, &lerr);
    if (ILU_ERRNOK(lerr))
      goto dun;
    if (!ilu_BeginReply(call, FALSE, rsize, &lerr))
      goto dun;
    if (!ilu_FinishReply(call, &lerr))
      goto dun;
  } else {
    ilu_cardinal asize = ilu_BeginSizingException (call, - (ilu_integer) ilu_ProtocolException_GarbageArguments, &lerr);
    if (ILU_ERRNOK(lerr)) goto dun;
    if (!ilu_BeginException(call, - (ilu_integer) ilu_ProtocolException_GarbageArguments,
			    asize, &lerr))
      goto dun;
    if (!ilu_FinishException(call, &lerr))
      goto dun;
  }
dun:
  ILU_HANDLED(lerr);
  if (disc != NIL) {
    ilu_Server      s = object_server(disc);
    ilu_Class       cl = object_class(disc);
    ilu_EnterServer(s, cl);
    lerr = ilu_DeltaHolds(disc, -1);
    ILU_MUST_BE_SUCCESS(lerr);
    ilu_ExitServer(s, cl);
  }
  return;
}
