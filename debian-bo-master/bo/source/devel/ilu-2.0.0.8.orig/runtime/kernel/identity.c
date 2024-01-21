/*
Copyright (c) 1991-1996 Xerox Corporation.  All Rights Reserved.  

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
/* $Id: identity.c,v 1.12 1996/07/03 01:02:50 janssen Exp $ */
/* Last edited by Mike Spreitzer June 6, 1996 8:50 am PDT */

#include "iluntrnl.h"

#include "connect.h"
#include "vector.h"
#include "call.h"

struct _ilu_Passport_s {
  struct ilu_vector_s v;
};

#define MAX_IDENTITY_TYPES	10

static ilu_IdentityType IdentityTypes[MAX_IDENTITY_TYPES] = {
  &ilu_NoIdentity_s,
  &ilu_ConnectionIdentity_s,
#ifdef SECURE_TRANSPORT
  &ilu_GSSIdentity_s,
#endif /* SECURE_TRANSPORT */
#ifdef SUNRPC_PROTOCOL
  &ilu_SunRPCAuthUnixIdentity_s,
#endif /* SUNRPC_PROTOCOL */
  NIL };

/* Locking unconstrained */

ilu_boolean
  ilu_RegisterIdentityType (struct _ilu_IdentityType_s *type,
			    ilu_Error *err)
{
  int i;

  for (i = 0;  i < MAX_IDENTITY_TYPES;  i++)
    {
      if (IdentityTypes[i] == NIL)
	{ 
	  DEBUG(AUTHENTICATION_DEBUG,
		(stderr, "ilu_RegisterIdentityType:  registered identity type \"%s\".\n",
		 type->it_name));
	  IdentityTypes[i] = type;
	  return ILU_CLER(*err);
	}
      else if (strcmp(IdentityTypes[i]->it_name, type->it_name) == 0)
	{
	  DEBUG(AUTHENTICATION_DEBUG,
		(stderr, "ilu_RegisterIdentityType:  identity type \"%s\" already registered.\n",
		 type->it_name));
	  return ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_identity_type_registered, ilu_FALSE);
	}
    }
  DEBUG(AUTHENTICATION_DEBUG,
	(stderr, "ilu_RegisterIdentityType:  too many identity types, registration of \"%s\" failed."
	 "  Max is %d.\n",
	 type->it_name, MAX_IDENTITY_TYPES));
  return ILU_ERR_CONS1(imp_limit, err, minor, ilu_ilm_max_identity_types, ilu_FALSE);
}

ilu_IdentityType
  ilu_FindIdentityTypeByName (char *name,
			      ilu_Error *err)
{
  int i;

  ILU_CLER(*err);
  for (i = 0;  i < MAX_IDENTITY_TYPES;  i++)
    if (strcmp(IdentityTypes[i]->it_name, name) == 0)
      return IdentityTypes[i];
  return NIL;
}

/*======================================================================

  NoIdentity

======================================================================*/

static ilu_cardinal
  _ilu_NoIdentity_StringForm (ilu_IdentityInfo info,
			      char *buffer,
			      ilu_cardinal bufferlen,
			      ilu_Error *err)
{
  if (bufferlen > 0)
    { *buffer = 0; ILU_CLER(*err); return 0; }
  else
    return (ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_small_buffer, 0));
}

static ilu_refany
  _ilu_NoIdentity_DuplicateData (ilu_IdentityInfo info,
				 ilu_Error *err)
{
  ILU_CLER(*err);
  return NIL;
}

static void
  _ilu_NoIdentity_FreeData (ilu_IdentityInfo info,
			    ilu_Error *err)
{
  ILU_CLER(*err);
}

struct _ilu_IdentityType_s ilu_NoIdentity_s = {
  "NoIdentity",
  _ilu_NoIdentity_StringForm,
  _ilu_NoIdentity_DuplicateData,
  _ilu_NoIdentity_FreeData,
  NULLFN, NULLFN };
  
/*======================================================================

  ConnectionIdentity

======================================================================*/

static ilu_cardinal
  _ilu_ConnectionIdentity_StringForm (ilu_IdentityInfo info,
				      char *buffer,
				      ilu_cardinal bufferlen,
				      ilu_Error *err)
{
  ilu_cardinal needed = _ilu_SafeStrlen((ilu_string)(info->ii_info)) + 1;

  if (bufferlen < needed)
    return (ILU_ERR_CONS1(bad_param, err, minor, ilu_bpm_small_buffer, needed));
  else
    { strcpy (buffer, (ilu_string)(info->ii_info)); ILU_CLER(*err); return needed-1; }
}

static ilu_refany
  _ilu_ConnectionIdentity_DuplicateData (ilu_IdentityInfo info,
					 ilu_Error *err)
{
  return (ilu_StrdupE((ilu_string) (info->ii_info), err));
}

static void
  _ilu_ConnectionIdentity_FreeData (ilu_IdentityInfo info,
				    ilu_Error *err)
{
  ilu_free(info->ii_info);
  ILU_CLER(*err);
}

struct _ilu_IdentityType_s ilu_ConnectionIdentity_s = {
  "ConnectionIdentity",
  _ilu_ConnectionIdentity_StringForm,
  _ilu_ConnectionIdentity_DuplicateData,
  _ilu_ConnectionIdentity_FreeData,
  NULLFN, NULLFN };
  

/*Main Invariant holds; L2 >= {call's conn's callmu}*/

void _ilu_AddConnIdentities (ilu_Call call, ilu_Error *err)
{
  ilu_Error lerr;
  ilu_Passport ppn = call->ca_caller;

  if (ppn == NIL)
    {
      ppn = ilu_CreatePassport(NIL, err);
      if (ILU_ERRNOK(*err)) return;
    }

  if (call_connection(call)->co_auth_info != NIL)
    {
      ilu_Passport pp = (ilu_Passport) (call_connection(call)->co_auth_info);
      ilu_IdentityInfo ident;

      if ((ident = ilu_FindIdentity(pp, ilu_ConnectionIdentity)) != NIL)
	{
	  ilu_AddIdentity (ppn, ident, err);
	  if (ILU_ERRNOK(*err))
	    {
	      ilu_DestroyPassport (ppn, &lerr);
	      ILU_HANDLED(lerr);
	      return;
	    }
	}

#ifdef SECURE_TRANSPORT
      if ((ident = ilu_FindIdentity(pp, ilu_GSSIdentity)) != NIL)
	{
	  ilu_AddIdentity (ppn, ident, ilu_FALSE, err);
	  if (ILU_ERRNOK(*err))
	    {
	      ilu_DestroyPassport(ppn, &lerr);
	      ILU_HANDLED(lerr);
	      return;
	    }
	}
#endif

    }
  if (call->ca_caller == NIL)
    call->ca_caller = ppn;
  ILU_CLER(*err);
}

/*L1, L2, Main unconstrained*/

extern ilu_Passport
  ilu_CreatePassport (const struct _ilu_IdentityInfo_s * info,
		      ILU_ERRS((no_memory)) *err)
/* creates and returns a passport, optionally containing the specified identity */
{
  ilu_Passport p = (ilu_Passport) _ilu_vector_new(2);
  if (p == NIL)
    return ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*p), NIL);
  else
    ILU_CLER(*err);
  if (info != NIL)
    {
      ilu_AddIdentity (p, info, err);
      if (ILU_ERRNOK(*err))
	{
	  ilu_free(p);
	  p = NIL;
	}
    }
  return p;
}

extern ilu_IdentityInfo
  ilu_CopyIdentity (const struct _ilu_IdentityInfo_s *info,
		    ILU_ERRS((no_memory, bad_param)) *err)
/* allocates and returns a copy of the ilu_IdentityInfo parameter */
{
  ilu_IdentityInfo i;
  ilu_refany dupinfo;

  dupinfo = (*info->ii_type->it_duplicate_data)(info->ii_info, err);
  if (ILU_ERRNOK(*err))
    return NIL;

  i = (ilu_IdentityInfo) ilu_MallocE(sizeof(*i), err);
  if (ILU_ERRNOK(*err))
    {
      ilu_Error localerr;
      (*info->ii_type->it_free_data)(dupinfo, &localerr);
      ILU_HANDLED(localerr);
      return NIL;
    }
  i->ii_type = info->ii_type;
  i->ii_owned_by_passport = ilu_FALSE;
  i->ii_info = dupinfo;
  return i;  
}

extern ilu_boolean
  ilu_AddIdentity (ilu_Passport p,
		   const struct _ilu_IdentityInfo_s *info,
		   ILU_ERRS((no_memory)) *err)
/* added identity to Passport.  Only one identity of each type is allowed.
   Returns ILU_ERROK() of the error parameter. */
{
  _ilu_vector_add((ilu_Vector) p, (ilu_refany) info);
  ILU_CLER(*err);
  return ILU_ERROK(*err);
}

extern ilu_IdentityInfo
  ilu_FindIdentity (ilu_Passport p,
		    ilu_IdentityType infotype)
/* return identity of specified type, if present.  Returns NIL if not present. */
{
  ilu_Vector v = (ilu_Vector) p;
  ilu_cardinal i;

  for (i = 0;  i < v->ve_size;  i++)
    if (((ilu_IdentityInfo)(v->ve_elements[i]))->ii_type == infotype)
      return ((ilu_IdentityInfo)(v->ve_elements[i]));
  return NIL;
}

extern ilu_boolean
  ilu_PickleIdentity (ilu_IdentityInfo i,
		      ilu_bytes *pickled_data,
		      ilu_cardinal *pickled_len,
		      ilu_Error *err)
{
  if (i->ii_type->it_pickle == NULLFN)
    { ILU_CLER(*err); return ilu_FALSE; };
  *pickled_len = (*i->ii_type->it_pickle)(i, pickled_data, err);
  return ILU_ERROK(*err);
}

extern ilu_IdentityInfo
  ilu_UnpickleIdentity (ilu_IdentityType i,
			ilu_bytes pickled_data,
			ilu_cardinal pickled_len,
			ilu_Error *err)
{
  if (i->it_unpickle == NULLFN)
    { ILU_CLER(*err); return NIL; }
  else
    return (i->it_unpickle)(pickled_data, pickled_len, err);
}

extern ilu_boolean
  ilu_DestroyPassport (ilu_Passport passport,
		       ilu_Error * err)
/* destroys the passport and frees any associated identities */
{
  ilu_IdentityInfo info;
  ilu_Vector vec = (ilu_Vector) passport;
  ilu_cardinal n;

  ILU_CLER(*err);
  for (n = 0;  n < vec->ve_size;  n++)
    {
      info = (ilu_IdentityInfo)(vec->ve_elements[n]);
      if (!info->ii_owned_by_passport)
	continue;
      (*info->ii_type->it_free_data)(info, err);
      ilu_free(info);
    }
  if (vec->ve_elements != NIL)
    ilu_free(vec->ve_elements);
  ilu_free(passport);
  return ILU_ERROK(*err);
}
