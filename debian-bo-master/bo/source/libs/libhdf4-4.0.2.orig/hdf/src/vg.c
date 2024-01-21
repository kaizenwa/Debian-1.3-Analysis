/****************************************************************************
 * NCSA HDF                                                                 *
 * Software Development Group                                               *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING file.                                                        *
 *                                                                          *
 ****************************************************************************/

#ifdef RCSID
static char RcsId[] = "@(#)1.44";
#endif
/* vg.c,v 1.15.4.1 1993/10/26 19:25:07 georgev Exp */
/*
FILE  
     vg.c
     HDF vdata routines and some vgroup routines
EXPORTED ROUTINES
     vnewref        -- returns a unique reference number
     VSelts         -- number of elements in a vdata
     VSgetinterlace -- returns the interlace type of the vdata
     VSsetinterlace -- sets the vdata's interlace to full or none
     VSgetfields    -- returns the fieldnames in a vdata
     VSfexist       -- tests for existence of one or more fields in a vdata
     VSsizeof       -- computes the byte size of the field(s) if a vdata
     VSdump         -- prints contents of a vdata (for debugging)
     VSsetname      -- associate a name with a vdata
     VSsetclass     -- assigns a class name to a vdata
     VSgetname      -- gets the vdata's name
     VSgetclass     -- gets the vdata's class name
     VSinquire      -- gets information about a vdata
     VSlone         -- returns an array of refs of all lone vdatas in the file
     Vlone          -- returns an array of refs of all lone vgroups in the file
     Vfind          -- looks in the file for a vgroup with a given name 
     VSfind         -- looks in the file for a vdata with a given name
     Vsetzap        -- maintaind for back compatibility
     VSgetclass     -- gets the vdata's class name
PRIVATE FUNCTIONS
     matchnocase    -- compares to strings, ignoring case

PRIVATE functions manipulate vsdir and are used only within this file.
PRIVATE data structures in here pertain to vdata in vsdir only.
 */

#include "vg.h"

/* Private functions */
#ifdef VDATA_FIELDS_ALL_UPPER
PRIVATE int32 matchnocase
            (char *strx, char *stry);
#endif /* VDATA_FIELDS_ALL_UPPER */

#ifdef VDATA_FIELDS_ALL_UPPER
/*-----------------------------------------------------------------
NAME
   matchnocase -  (PRIVATE) compares 2 strings, ignoring case 
USAGE
   int32 matchnocase(strx, stry)
   char *strx, *stry;   IN: strings to be compared.
RETURNS
   TRUE if match, else FALSE
DESCRIPTION
   Private routine. 
--------------------------------------------------------------------*/
PRIVATE int32 
matchnocase(char *strx, char *stry)
{
    int32       i, nx, ny;

    nx = HDstrlen(strx);
    ny = HDstrlen(stry);
    if (nx != ny)
        return (FALSE);     /* different lengths */

    for (i = 0; i < nx; i++, strx++, stry++)
        if (toupper(*strx) != toupper(*stry))
            return (FALSE);

    return (TRUE);
}   /* matchnocase */
#endif /* VDATA_FIELDS_ALL_UPPER */

/* ------------------------------------------------------------------
NAME
   VSelts -- get number of elements in a vdata
USAGE
   int32 VSelts(vkey)
   int32 vkey;      IN: vdata key
RETURNS
   On success returns the number of elements in the VDATA vkey; 
   returns FAIL  on error.
DESCRIPTION
--------------------------------------------------------------------- */
int32 
VSelts(int32 vkey)
{
    vsinstance_t *w;
    VDATA      *vs;
    int32      ret_value = SUCCEED;
    CONSTR(FUNC, "VSelts");

#ifdef HAVE_PABLO
  TRACE_ON(VS_mask, ID_VSelts);
#endif /* HAVE_PABLO */

    if (HAatom_group(vkey)!=VSIDGROUP)
        HGOTO_ERROR(DFE_ARGS, FAIL);

    /* locate vg's index in vgtab */
    if (NULL == (w = (vsinstance_t *) HAatom_object(vkey)))
        HGOTO_ERROR(DFE_NOVS, FAIL);

    vs = w->vs;
    if ((vs == NULL) || (vs->otag != DFTAG_VH))
        HGOTO_ERROR(DFE_ARGS, FAIL);

    ret_value = (vs->nvertices);

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

  /* Normal function cleanup */
#ifdef HAVE_PABLO
  TRACE_OFF(VS_mask, ID_VSelts);
#endif /* HAVE_PABLO */

  return ret_value;
}   /* VSelts */

/* ------------------------------------------------------------------
NAME
   VSgetinterlace -- gets interlace of the vdata vkey.
USAGE
   int32 VSgetinterlace(vkey)
   int32 vkey;     IN: vdata key
RETURNS
   On success returns the interlace (in the file) of the vdata vkey.
   returns FAIL on error.
DESCRIPTION
----------------------------------------------------------------------- */
int32 
VSgetinterlace(int32 vkey)
{
    vsinstance_t *w;
    VDATA      *vs;
    int32      ret_value = SUCCEED;
    CONSTR(FUNC, "VSgetinterlace");

#ifdef HAVE_PABLO
  TRACE_ON(VS_mask, ID_VSgetinterlace);
#endif /* HAVE_PABLO */

    if (HAatom_group(vkey)!=VSIDGROUP)
        HGOTO_ERROR(DFE_ARGS, FAIL);

    /* locate vg's index in vgtab */
    if (NULL == (w = (vsinstance_t *) HAatom_object(vkey)))
        HGOTO_ERROR(DFE_NOVS, FAIL);

    vs = w->vs;
    if (vs == NULL)
        HGOTO_ERROR(DFE_ARGS, FAIL);

    ret_value = ((int32) (vs->interlace));

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

  /* Normal function cleanup */
#ifdef HAVE_PABLO
  TRACE_OFF(VS_mask, ID_VSgetinterlace);
#endif /* HAVE_PABLO */

  return ret_value;
}   /* VSgetinterlace */

/*------------------------------------------------------------------
NAME
   VSsetinterlace --  sets the vdata's interlace to full or none.
USAGE
   int32 VSsetinterlace(vkey, interlace)
   int32 vkey;       IN: vdata key
   int32 interlace;  IN: interlace for storing data in an HDF file.
RETURNS
   SUCCEED/FAIL
DESCRIPTION
   The interlace may be one of FULL_INTERLACE or NO_INTERLACE.
--------------------------------------------------------------------*/
intn 
VSsetinterlace(int32 vkey, int32 interlace)
{
    vsinstance_t *w;
    VDATA      *vs;
    intn       ret_value = SUCCEED;
    CONSTR(FUNC, "VSsetinterlace");

#ifdef HAVE_PABLO
  TRACE_ON(VS_mask, ID_VSsetinterlace);
#endif /* HAVE_PABLO */

    if (HAatom_group(vkey)!=VSIDGROUP)
        HGOTO_ERROR(DFE_ARGS, FAIL);

    /* locate vg's index in vgtab */
    if (NULL == (w = (vsinstance_t *) HAatom_object(vkey)))
        HGOTO_ERROR(DFE_NOVS, FAIL);

    vs = w->vs;
    if (vs == NULL)
        HGOTO_ERROR(DFE_BADPTR, FAIL);

    if (vs->access == 'r')
        HGOTO_ERROR(DFE_RDONLY, FAIL);
    if (vs->nvertices > 0)
        HGOTO_ERROR(DFE_NORESET, FAIL);

    /* currently only 2 kinds allowed */
    if (interlace == FULL_INTERLACE || interlace == NO_INTERLACE)
      {
          vs->interlace = (int16) interlace;
          ret_value = SUCCEED;    /* ok */
      }
    else
        ret_value = FAIL;

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

  /* Normal function cleanup */
#ifdef HAVE_PABLO
  TRACE_OFF(VS_mask, ID_VSsetinterlace);
#endif /* HAVE_PABLO */

  return ret_value;
}   /* VSsetinterlace */

/*------------------------------------------------------------------
NAME
   VSgetfields -- returns the name (if any) of all the fields in the vdata
USAGE 
   int32 VSgetfields(vkey, fields)
   int32 vkey;      IN: vdata key
   char *fields;    OUT: storage for field names
RETURNS
   Returns -1 on error, else the no of fields in the vdata.
DESCRIPTION
   The fields are returned as a comma-separated string (e.g., "PX,PY").
----------------------------------------------------------------------*/
int32 
VSgetfields(int32 vkey, char *fields)
{
    int32       i;
    vsinstance_t *w;
    VDATA      *vs;
    int32      ret_value = SUCCEED;
    CONSTR(FUNC, "VSgetfields");

#ifdef HAVE_PABLO
  TRACE_ON(VS_mask, ID_VSgetfields);
#endif /* HAVE_PABLO */

    if (HAatom_group(vkey)!=VSIDGROUP)
        HGOTO_ERROR(DFE_ARGS, FAIL);

    /* locate vg's index in vgtab */
    if (NULL == (w = (vsinstance_t *) HAatom_object(vkey)))
        HGOTO_ERROR(DFE_NOVS, FAIL);

    vs = w->vs;
    if (vs == NULL)
        HGOTO_ERROR(DFE_BADPTR, FAIL);

    fields[0] = '\0';
    for (i = 0; i < vs->wlist.n; i++)
      {     /* build the comma-separated string */
          HDstrcat(fields, vs->wlist.name[i]);
          if (i < vs->wlist.n - 1)
              HDstrcat(fields, ",");
      }

    ret_value = ((int32) vs->wlist.n);

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

  /* Normal function cleanup */
#ifdef HAVE_PABLO
  TRACE_OFF(VS_mask, ID_VSgetfields);
#endif /* HAVE_PABLO */

  return ret_value;
}   /* VSgetfields */

/*------------------------------------------------------------------
NAME
   VSfexist -- tests for existence of 1 or more fields in a vdata.
USAGE
   int32 VSfexist(vkey, fields)
   int32 vkey;    IN: vdata key
   char *fields;  IN: Names of the fields to check for
RETURNS 
   RETURNS 1 if all fields exist; otherwise -1 is returned.
DESCRIPTION
   The argument 'fields' is a string of comma-separated fieldnames 
   (e.g. "PX,PY,PZ").
--------------------------------------------------------------------*/
intn 
VSfexist(int32 vkey, char *fields)
{
    char      **av, *s;
    int32       ac, i, j, found;
    DYN_VWRITELIST *w;
    vsinstance_t *wi;
    VDATA      *vs;
    intn       ret_value = SUCCEED;
    CONSTR(FUNC, "VSfexist");

#ifdef HAVE_PABLO
  TRACE_ON(VS_mask, ID_VSfexist);
#endif /* HAVE_PABLO */

    if (HAatom_group(vkey)!=VSIDGROUP)
        HGOTO_ERROR(DFE_ARGS, FAIL);

    /* locate vg's index in vgtab */
    if (NULL == (wi = (vsinstance_t *) HAatom_object(vkey)))
        HGOTO_ERROR(DFE_NOVS, FAIL);

    vs = wi->vs;

    /* call scanattrs to parse the string */
    if (scanattrs(fields, &ac, &av) < 0)
        HGOTO_ERROR(DFE_BADFIELDS, FAIL);

    if ((vs == NULL) || (ac < 1))
        HGOTO_ERROR(DFE_ARGS, FAIL);

    /* now check in vs's field table */
    w = &vs->wlist;
    for (i = 0; i < ac; i++)
      {
          found = 0;
          s = av[i];
          for (j = 0; j < w->n; j++)
            {
#ifdef VDATA_FIELDS_ALL_UPPER
                if (matchnocase(s, w->name[j]))
                  {
                      found = 1;
                      break;
                  }
#else
                if (HDstrcmp(s, w->name[j]) == 0)
                  {
                      found = 1;
                      break;
                  }
#endif /* VDATA_FIELDS_ALL_UPPER */
            }
          if (!found)
            HGOTO_DONE(FAIL);
      }

    ret_value = TRUE;

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

  /* Normal function cleanup */
#ifdef HAVE_PABLO
  TRACE_OFF(VS_mask, ID_VSfexist);
#endif /* HAVE_PABLO */

  return ret_value;
}	/* VSfexist */

/* -----------------------------------------------------------------
NAME
   VSsizeof - computes the byte size of the field(s) of a vdata.
USAGE
   int32 VSsizeof(vkey, fields)
   int32 vkey;   IN: vdata key.
   char *fields; IN: Name(s) of the fields to check.
RETURNS
   The byte size of the field(s), positive integer, on success; 
   otherwise, returns FAIL.
DESCRIPTION
   The size is the actual size for the local machine.

----------------------------------------------------------------- */
int32 
VSsizeof(int32 vkey, char *fields)
{
    int32       totalsize, ac, i, j, found;
    char      **av;
    vsinstance_t *w;
    VDATA      *vs;
    int32      ret_value = SUCCEED;
    CONSTR(FUNC, "VSsizeof");

#ifdef HAVE_PABLO
  TRACE_ON(VS_mask, ID_VSsizeof);
#endif /* HAVE_PABLO */

    if (HAatom_group(vkey)!=VSIDGROUP)
        HGOTO_ERROR(DFE_ARGS, FAIL);

    /* locate vg's index in vgtab */
    if (NULL == (w = (vsinstance_t *) HAatom_object(vkey)))
        HGOTO_ERROR(DFE_NOVS, FAIL);

    vs = w->vs;
    if (vs == NULL)
        HGOTO_ERROR(DFE_ARGS, FAIL);

    totalsize = 0;
    if (fields == NULL)
      {   /* count fieldsizes in vs */
        for (j = 0; j < vs->wlist.n; j++)	
            totalsize += vs->wlist.esize[j];
      }		
    else
      {
        if ((scanattrs(fields, &ac, &av) < 0) || (ac < 1))
            HGOTO_ERROR(DFE_ARGS, FAIL);
        for (i = 0; i < ac; i++)
          {   /* check fields in vs */
            for (found = 0, j = 0; j < vs->wlist.n; j++)	
                if (!HDstrcmp(av[i], vs->wlist.name[j]))
                  {
                    totalsize += vs->wlist.esize[j];
                    found = 1;
                    break;
                  }

            if (!found)
                HGOTO_ERROR(DFE_ARGS, FAIL);
          }	/* end for */
      }		/* end else */
    ret_value = (totalsize);

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

  /* Normal function cleanup */
#ifdef HAVE_PABLO
  TRACE_OFF(VS_mask, ID_VSsizeof);
#endif /* HAVE_PABLO */

  return ret_value;
}   /* VSsizeof */

/*----------------------------------------------------------------- 
NAME
   VSdump - prints contents of a vdata (for debugging) 
USAGE
   VOID VSdump(vkey)
   int32 vkey;   IN: vdata key.
RETURNS
   No return codes.
-------------------------------------------------------------------*/
VOID 
VSdump(int32 vkey)
{
    vkey = vkey;    /* suppress warning */
}   /* VSdump */

/*-------------------------------------------------------
NAME
   VSsetname - give a name to a vdata.
USAGE
   int32 VSsetname(vkey, vsname)
   int32 vkey;    IN: vdata key.
   char *vsname;  IN: name for the vdata.
RETURNS
   SUCCEED/FAIL
DESCRIPTION
   Truncates name to max length of VSNAMELENMAX
----------------------------------------------------------*/
int32 
VSsetname(int32 vkey, const char *vsname)
{
    vsinstance_t *w;
    VDATA      *vs;
    CONSTR(FUNC, "VSsetname");
    int         slen;
    int32       ret_value = SUCCEED;

#ifdef HAVE_PABLO
  TRACE_ON(VS_mask, ID_VSsetname);
#endif /* HAVE_PABLO */

    if (HAatom_group(vkey)!=VSIDGROUP)
        HGOTO_ERROR(DFE_ARGS, FAIL);

    /* locate vg's index in vgtab */
    if (NULL == (w = (vsinstance_t *) HAatom_object(vkey)))
        HGOTO_ERROR(DFE_NOVS, FAIL);

    vs = w->vs;
    if (vs == NULL)
        HGOTO_ERROR(DFE_BADPTR, FAIL);

    if ((slen = HDstrlen(vsname)) > VSNAMELENMAX)
      {
          HDstrncpy(vs->vsname, vsname, VSNAMELENMAX);
          vs->vsname[VSNAMELENMAX] = '\0';
      }
    else
        HDstrcpy(vs->vsname, vsname);
    vs->marked = TRUE;

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

  /* Normal function cleanup */
#ifdef HAVE_PABLO
  TRACE_OFF(VS_mask, ID_VSsetname);
#endif /* HAVE_PABLO */

  return ret_value;
}   /* VSsetname */

/* ------------------------------------------------------
NAME
   VSsetclass - give a class name to a vdata.
USAGE
   int32 VSsetclass(vkey, vsclass)
   int32 vkey;    IN: vdata key.
   char *vsclass;  IN: class name for the vdata.
RETURNS
   SUCCEED/FAIL
DESCRIPTION
   Truncates class name to max length of VSNAMELENMAX
----------------------------------------------------------*/
int32 
VSsetclass(int32 vkey, const char *vsclass)
{
    vsinstance_t *w;
    VDATA      *vs;
    CONSTR(FUNC, "VSsetclass");
    int         slen;
    int32       ret_value = SUCCEED;

#ifdef HAVE_PABLO
  TRACE_ON(VS_mask, ID_VSsetclass);
#endif /* HAVE_PABLO */

    if (HAatom_group(vkey)!=VSIDGROUP)
        HGOTO_ERROR(DFE_ARGS, FAIL);

    /* locate vg's index in vgtab */
    if (NULL == (w = (vsinstance_t *) HAatom_object(vkey)))
        HGOTO_ERROR(DFE_NOVS, FAIL);

    vs = w->vs;
    if (vs == NULL)
        HGOTO_ERROR(DFE_BADPTR, FAIL);

    if ((slen = HDstrlen(vsclass)) > VSNAMELENMAX)
      {
          HDstrncpy(vs->vsclass, vsclass, VSNAMELENMAX);
          vs->vsclass[VSNAMELENMAX] = '\0';
      }
    else
        HDstrcpy(vs->vsclass, vsclass);
    vs->marked = TRUE;

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

  /* Normal function cleanup */
#ifdef HAVE_PABLO
  TRACE_OFF(VS_mask, ID_VSsetclass);
#endif /* HAVE_PABLO */

  return ret_value;
}   /* VSsetclass */

/*------------------------------------------------------ 
NAME
   VSgetname - gets the vdata's name. 
USAGE
   int32 VSgetname(vkey, vsname)
   int32 vkey;    IN: vdata key.
   char *vsname;  OUT: storage for vdata name. 
RETURNS
   SUCCEED/FAIL
DESCRIPTION
----------------------------------------------------------*/
int32 
VSgetname(int32 vkey, char *vsname)
{
    vsinstance_t *w;
    VDATA      *vs;
    int32      ret_value = SUCCEED;
    CONSTR(FUNC, "VSgetname");

#ifdef HAVE_PABLO
  TRACE_ON(VS_mask, ID_VSgetname);
#endif /* HAVE_PABLO */

    if (HAatom_group(vkey)!=VSIDGROUP)
        HGOTO_ERROR(DFE_ARGS, FAIL);

    /* locate vg's index in vgtab */
    if (NULL == (w = (vsinstance_t *) HAatom_object(vkey)))
        HGOTO_ERROR(DFE_NOVS, FAIL);

    vs = w->vs;
    if (vs == NULL)
        HGOTO_ERROR(DFE_BADPTR, FAIL);

    HDstrcpy(vsname, vs->vsname);

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

  /* Normal function cleanup */
#ifdef HAVE_PABLO
  TRACE_OFF(VS_mask, ID_VSgetname);
#endif /* HAVE_PABLO */

  return ret_value;
}   /* VSgetname */

/*------------------------------------------------------ 
NAME
   VSgetclass - gets the vdata's class name.
USAGE
   int32 VSgetclass(vkey, vsclass)
   int32 vkey;      IN: vdata key.
   char *vsclass;  OUT: class name for the vdata.
RETURNS
   SUCCEED/FAIL
DESCRIPTION
---------------------------------------------------------- */
int32 
VSgetclass(int32 vkey, char *vsclass)
{
    vsinstance_t *w;
    VDATA      *vs;
    int32     ret_value = SUCCEED;
    CONSTR(FUNC, "VSgetclass");

#ifdef HAVE_PABLO
  TRACE_ON(VS_mask, ID_VSgetclass);
#endif /* HAVE_PABLO */

    if (HAatom_group(vkey)!=VSIDGROUP)
        HGOTO_ERROR(DFE_ARGS, FAIL);

    /* locate vg's index in vgtab */
    if (NULL == (w = (vsinstance_t *) HAatom_object(vkey)))
        HGOTO_ERROR(DFE_NOVS, FAIL);

    vs = w->vs;
    if (vs == NULL)
        HGOTO_ERROR(DFE_BADPTR, FAIL);

    HDstrcpy(vsclass, vs->vsclass);

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

  /* Normal function cleanup */
#ifdef HAVE_PABLO
  TRACE_OFF(VS_mask, ID_VSgetclass);
#endif /* HAVE_PABLO */

  return ret_value;
}   /* VSgetclass */

/* ------------------------------------------------------------------ 
NAME
   VSinquire - gets info about a vdata vkey:
USAGE
   int32 VSinquire(vkey, nelt, interlace, fields,eltsize, vsname)
   int32 vkey;         IN: vdata key.
   int32 *nelt;       OUT: number of vertices in the vdata.
   int32 *interlace;  OUT: interlace of the vdata.
   char  *fields;     OUT: field(s) name.
   int32 *eltsize;    OUT: byte size of elements (all fields) on local
                           machine. 
   char *vsname;      OUT: vdata's name.
RETURNS
   SUCCEED/FAIL

------------------------------------------------------------------------*/
intn 
VSinquire(int32 vkey, int32 *nelt, int32 *interlace,
          char *fields, int32 *eltsize, char *vsname)
{
  intn ret_value = SUCCEED;
  intn status;

#ifdef LATER
    CONSTR(FUNC, "VSinquire");
#endif

#ifdef HAVE_PABLO
  TRACE_ON(VS_mask, ID_VSinquire);
#endif /* HAVE_PABLO */

    if (fields) {
        status = VSgetfields(vkey, fields);
        ret_value = (status == FAIL)? FAIL: ret_value;
    }
    if (nelt)   {
        *nelt = VSelts(vkey);
        ret_value = (*nelt == FAIL)? FAIL: ret_value;
    }
    if (interlace)  {
        *interlace = VSgetinterlace(vkey);
        ret_value = (*interlace == FAIL)? FAIL: ret_value;
    }
    if (eltsize) {
        *eltsize = VSsizeof(vkey, fields);
        ret_value = (*eltsize == FAIL)? FAIL: ret_value;
    }
    if (vsname)  {
        status = VSgetname(vkey, vsname);
        ret_value = (status == FAIL)? FAIL: ret_value;
    }

#ifdef HAVE_PABLO
  TRACE_OFF(VS_mask, ID_VSinquire);
#endif /* HAVE_PABLO */

    return ret_value;   /* ok */
}   /* VSinquire */

/*-----------------------------------------------------------------
NAME
   VSlone - returns an array of refs of all lone vdatas in the file.
USAGE
   int32 VSlone(f, idarray, asize)
   HFILEID f;        IN: file id.
   int32 *idarray;   IN: array to contain the refs.
   int32 asize;      IN: size of idarray.
RETURNS
   Returns -1 if error; otherwise returns the total number of lone 
   vdatas in the file
DESCRIPTION
   If idarray is too small, routine will only fill idarray with up
   to asize worth of refs.
---------------------------------------------------------------------*/
int32 
VSlone(HFILEID f, int32 *idarray, int32 asize)
{
    uint8      *lonevdata;      /* lcl wrk area: stores flags of vdatas */
    int32       i, vgid, vsid, vstag;
    int32       vkey;
    int32       nlone;          /* total number of lone vdatas */
    int32       ret_value = SUCCEED;
    CONSTR(FUNC, "VSlone");

#ifdef HAVE_PABLO
  TRACE_ON(VS_mask, ID_VSlone);
#endif /* HAVE_PABLO */

    /* -- allocate space for vdata refs, init to zeroes -- */
    if (NULL == (lonevdata = (uint8 *) HDcalloc(MAX_REF , sizeof(uint8))))
        HGOTO_ERROR(DFE_NOSPACE, FAIL);

    /* -- look for all vdatas in the file, and flag (1) each -- */
    vsid = -1;
    while (-1L != (vsid = VSgetid(f, vsid)))    /* until no more vdatas */
        lonevdata[vsid] = 1;

    /* -- Look through all vgs, searching for vdatas -- */
    /* -- increment its index in lonevdata if found -- */
    vgid = -1;
    while (-1L != (vgid = Vgetid(f, vgid)))
      {     /* until no more vgroups */
        vkey = Vattach(f, vgid, "r");
        for (i = 0; i < Vntagrefs(vkey); i++)
          {
            Vgettagref(vkey, i, &vstag, &vsid);
            if (vstag == (int32) DFTAG_VH)
                lonevdata[vsid] = 0;
          }
        Vdetach(vkey);
      }

    /* -- check in lonevdata: it's a lone vdata if its flag is still 1 -- */
    nlone = 0;
    for (i = 0; i < MAX_REF; i++)
      {
        if (lonevdata[i])
          {
            if (nlone < asize)  /* insert into idarray up till asize */
                idarray[nlone] = i;
            nlone++;
          }
      }
    HDfree((VOIDP) lonevdata);

    ret_value = nlone;     /* return the TOTAL # of lone vdatas */

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

  /* Normal function cleanup */
#ifdef HAVE_PABLO
  TRACE_OFF(VS_mask, ID_VSlone);
#endif /* HAVE_PABLO */

  return ret_value;
}   /* VSlone */

/*----------------------------------------------------------------- 
NAME
   Vlone  - returns an array of refs of all lone vgroups in the file.
USAGE
   int32 Vlone(f, idarray, asize)
   HFILEID f;        IN: file id.
   int32 *idarray;   IN: array to contain the refs.
   int32 asize;      IN: size of idarray.
RETURNS
   Returns -1 if error; otherwise returns the total number of lone
   vgroups in the file
DESCRIPTION
   If idarray is too small, routine will only fill idarray with up
   to asize worth of refs.
---------------------------------------------------------------------*/
int32 
Vlone(HFILEID f, int32 *idarray, int32 asize)
{
    uint8      *lonevg;         /* local wrk area: stores flags of vgroups */
    int32       i;
    int32       vgid, vstag, id;
    int32       vkey;
    int32       nlone;          /* total number of lone vgroups */
    int32       ret_value = SUCCEED;
    CONSTR(FUNC, "Vlone");

#ifdef HAVE_PABLO
  TRACE_ON(V_mask, ID_Vlone);
#endif /* HAVE_PABLO */

    /* -- allocate space for vgroup refs, init to zeroes -- */
    if (NULL == (lonevg = (uint8 *) HDcalloc(MAX_REF , sizeof(uint8))))
        HGOTO_ERROR(DFE_NOSPACE, FAIL);

    /* -- look for all vgroups in the file, and flag (1) each -- */
    id = -1;
    while (-1L != (id = Vgetid(f, id)))     /* until no more vgroups */
        lonevg[id] = 1;

    /* -- Look through all vgs, searching for vgroups -- */
    /* -- increment its index in lonevg if found -- */
    vgid = -1;
    while (-1L != (vgid = Vgetid(f, vgid)))
      {     /* until no more vgroups */
        vkey = Vattach(f, vgid, "r");
        id = -1;
        for (i = 0; i < Vntagrefs(vkey); i++)
          {
            Vgettagref(vkey, i, &vstag, &id);
            if (vstag == DFTAG_VG)
                lonevg[id] = 0;
          }
        Vdetach(vkey);
      }

    /* -- check in lonevg: it's a lone vgroup if its flag is still 1 -- */
    nlone = 0;
    for (i = 0; i < MAX_REF; i++)
      {
        if (lonevg[i])
          {
            if (nlone < asize)  /* insert into idarray up till asize */
                idarray[nlone] = i;
            nlone++;
          }
      }
    HDfree((VOIDP) lonevg);

    ret_value = nlone;     /* return the TOTAL # of lone vgroups */

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

  /* Normal function cleanup */
#ifdef HAVE_PABLO
  TRACE_OFF(V_mask, ID_Vlone);
#endif /* HAVE_PABLO */

  return ret_value;
}   /* Vlone */

/* ----------------------------------------------------------------- 
NAME
   Vfind -- looks in the file and returns the ref of 
            the vgroup with name vgname 
USAGE
   int32 Vfind(f, vgname)
   HFILEID  f;    IN: file id.
   char *vgname;  IN: name of the vgroup.
RETURNS
   Returns 0 if not found, or error. Otherwise, returns the 
   vgroup's ref (a positive integer).
-----------------------------------------------------------------------*/
int32 
Vfind(HFILEID f, const char *vgname)
{
    int32       vgid = -1;
    vginstance_t    * v;
    int32       ret_value = 0;
#ifdef LATER
    CONSTR(FUNC, "Vfind");
#endif

#ifdef HAVE_PABLO
  TRACE_ON(V_mask, ID_Vfind);
#endif /* HAVE_PABLO */

    while (-1L != (vgid = Vgetid(f, vgid)))
      {
        if((v=vginst(f,(uint16)vgid))==NULL)
            HGOTO_DONE(0);
        if (!HDstrcmp(vgname, v->vg->vgname)) 
            HGOTO_DONE((int32)(v->vg->oref));  /* found the vdata */
      }

done:
  if(ret_value == 0)   
    { /* Error condition cleanup */

    } /* end if */

  /* Normal function cleanup */
#ifdef HAVE_PABLO
  TRACE_OFF(V_mask, ID_Vfind);
#endif /* HAVE_PABLO */

  return ret_value;
}	/* Vfind */

/*------------------------------------------------------------------
NAME
   VSfind -- looks in the file and returns the ref of the vdata 
             with name vsname 
USAGE
   int32 VSfine(f, vsname)
   HFILEID  f;      IN: file id.
   char *vsname;    IN: name of the vdata.
RETURNS
   Returns 0 if not found, or error. Otherwise, returns the vdata's 
   ref (a positive integer).
---------------------------------------------------------------------*/
int32 
VSfind(HFILEID f, const char *vsname)
{
    int32       vsid = -1;
    vsinstance_t    * v;
    int32 ret_value = 0;
#ifdef LATER
    CONSTR(FUNC, "VSfind");
#endif

#ifdef HAVE_PABLO
  TRACE_ON(VS_mask, ID_VSfind);
#endif /* HAVE_PABLO */

    while (-1L != (vsid = VSgetid(f, vsid)))
      {
        if((v=vsinst(f,(uint16)vsid))==NULL)
            HGOTO_DONE(0);
        if (!HDstrcmp(vsname, v->vs->vsname)) 
            HGOTO_DONE((int32)(v->vs->oref));  /* found the vdata */
      }

done:
  if(ret_value == 0)   
    { /* Error condition cleanup */

    } /* end if */

  /* Normal function cleanup */
#ifdef HAVE_PABLO
  TRACE_OFF(VS_mask, ID_VSfind);
#endif /* HAVE_PABLO */

  return ret_value;
}   /* VSfind */

/* ----------------------------------------------------------------- 
NAME
   Vfindclass -- looks in the file and returns the ref of 
            the vgroup with class vgclass 
USAGE
   int32 Vfind(f, vgclass)
   HFILEID  f;    IN: file id.
   char *vgclass;  IN: class of the vgroup.
RETURNS
   Returns 0 if not found, or error. Otherwise, returns the 
   vgroup's ref (a positive integer).
-----------------------------------------------------------------------*/
int32 
Vfindclass(HFILEID f, const char *vgclass)
{
    int32       vgid = -1;
    vginstance_t    * v;
    int32       ret_value = 0;
#ifdef LATER
    CONSTR(FUNC, "Vfind");
#endif

#ifdef HAVE_PABLO
  TRACE_ON(V_mask, ID_Vfindclass);
#endif /* HAVE_PABLO */

    while (-1L != (vgid = Vgetid(f, vgid)))
      {
        if((v=vginst(f,(uint16)vgid))==NULL)
            HGOTO_DONE(0);
        if (!HDstrcmp(vgclass, v->vg->vgclass)) 
            HGOTO_DONE((int32)(v->vg->oref));  /* found the vgroup */
      }

done:
  if(ret_value == 0)   
    { /* Error condition cleanup */

    } /* end if */

  /* Normal function cleanup */
#ifdef HAVE_PABLO
  TRACE_ON(V_mask, ID_Vfindclass);
#endif /* HAVE_PABLO */

  return ret_value;    
}	/* Vfindclass */

/*------------------------------------------------------------------
NAME
   VSfindclass -- looks in the file and returns the ref of the vdata 
             with class vsclass 
USAGE
   int32 VSfine(f, vsclass)
   HFILEID  f;      IN: file id.
   char *vsclass;    IN: class of the vdata.
RETURNS
   Returns 0 if not found, or error. Otherwise, returns the vdata's 
   ref (a positive integer).
---------------------------------------------------------------------*/
int32 
VSfindclass(HFILEID f, const char *vsclass)
{
    int32       vsid = -1;
    vsinstance_t    * v;
    int32       ret_value = 0;
#ifdef LATER
    CONSTR(FUNC, "VSfind");
#endif
#ifdef HAVE_PABLO
  TRACE_ON(VS_mask, ID_VSfindclass);
#endif /* HAVE_PABLO */

    while (-1L != (vsid = VSgetid(f, vsid)))
      {
        if((v=vsinst(f,(uint16)vsid))==NULL)
            HGOTO_DONE(0);          /* error */
        if (!HDstrcmp(vsclass, v->vs->vsclass)) 
            HGOTO_DONE((int32)(v->vs->oref));  /* found the vdata */
      }

done:
  if(ret_value == 0)   
    { /* Error condition cleanup */

    } /* end if */

  /* Normal function cleanup */
#ifdef HAVE_PABLO
  TRACE_ON(VS_mask, ID_VSfindclass);
#endif /* HAVE_PABLO */

  return ret_value;
}   /* VSfindclass */

/* ------------------------------- Vsetzap -------------------------------- */
/*
 * Vsetzap: Useless now. Maintained for back compatibility.
 */
VOID 
Vsetzap(void)
{
}
