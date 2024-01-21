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
static char RcsId[] = "@(#)1.20";
#endif

/* vhi.c,v 1.20 1996/05/16 18:42:16 koziol Exp */

/*
   * File
   *       vhi.c
   *       HDF Vset high-level access routines VHxxxx
   *       Feb 92 - update to use H-routines
   * Routines
   *       VHstoredata  -- stores data in a field of a vdata in an HDF file
   *       VHstoredatam -- stores data in a aggregated-typed field of a vdata
   *       VHmakegroup  -- makes a vgroup from tag/ref pairs
 */

#include "vg.h"

/* ------------------------ VHstoredata -------------------------------
   NAME
   VHstoredata -- stores data in a field of a new vdata
   USAGE
   int32 VHstoredata (f, field, buf, n, datatype, vsname, vsclass)
   HFILEID f;           IN: File id, returned from Hopen.
   char *  field;       IN: Name of the field.
   uint8   buf[];       IN: Buffer of data to be stored in the field.
   int32   n;           IN: Number of elements in buf to be written.
   inter   datatype;    IN: Number type of the data to be written.
   char *  vsname;      IN: Name of the new vdata.
   char *  vsclass;     IN: Class of the new vdata.

   RETURNS
   On success returns reference number of the new vdata, a positive integer;
   on failure returns -1.
   DESCRIPTION
   Stores 'n' elements of data from 'buf' as a field 'field' in a new vdata
   called 'vsname' into the already opened HDF file (with file id 'f').
   The datatype variable must be specified as a valid HDF type; n should not
   be zero or negative.
   Returns -1 if error; ref of that new vdata (a +ve integer) if successful.
   ------------------------------------------------------------------------- */

int32
VHstoredata(HFILEID f, char *field, uint8 buf[], int32 n, int32 datatype,
            const char *vsname, const char *vsclass)

{
#ifdef LATER
    CONSTR(FUNC, "VHstoredata");
#endif
    int32       order = 1;
    int32       ret_value;

#ifdef HAVE_PABLO
    TRACE_ON(VH_mask, ID_VHstoredata);
#endif /* HAVE_PABLO */
    ret_value = ((int32)VHstoredatam(f, field, buf, n, datatype, vsname, vsclass, order));

#ifdef HAVE_PABLO
    TRACE_OFF(VH_mask, ID_VHstoredata);
#endif /* HAVE_PABLO */
    return ret_value;
} /* end VHstoredata */

/* ----------------------- VHstoredatam ----------------------------
   NAME
   VHstoredatam -- Same as VHstoredata but allows aggregate-typed field.
   USAGE
   int32 VHstoredata (f, field, buf, n, datatype, vsname, vsclass, order)
   HFILEID f;           IN: File id, returned from Hopen.
   char *  field;       IN: Name of the field.
   uint8   buf[];       IN: Buffer of data to be stored in the field.
   int32   n;           IN: Number of elements in buf to be written.
   inter   datatype;    IN: Numter type of the data to be written.
   char *  vsname;      IN: Name of the new vdata.
   char *  vsclass;     IN: Class of the new vdata.
   int32   order;       IN: Order of the field.

   RETURNS
   On success returns reference number of the new vdata, a positive integer;
   on failure returns -1.
   DESCRIPTION
   Stores 'n' elements of data from 'buf' as a field 'field' in a new vdata
   called 'vsname' into the already opened HDF file (with file id 'f').
   The datatype variable must be specified as a valid HDF type;
   n should not be zero or negative.
   Returns -1 if error; ref of that new vdata (a +ve integer) if successful.
   --------------------------------------------------------------------------- */

int32
VHstoredatam(HFILEID f, char *field, uint8 buf[], int32 n, int32 datatype, const char *vsname, const char *vsclass, int32 order)
{
    CONSTR(FUNC, "VHstoredatam");
    int32       ref;
    int32       vs;
    int32       ret_value = SUCCEED;

#ifdef HAVE_PABLO
    TRACE_ON(VH_mask, ID_VHstoredatam);
#endif /* HAVE_PABLO */

    if ((vs = VSattach(f, -1, "w")) == FAIL)
        HGOTO_ERROR(DFE_CANTATTACH,FAIL);

    if ( VSfdefine(vs, field, datatype, order) == FAIL)
        HGOTO_ERROR(DFE_BADFIELDS,FAIL);

    if ( VSsetfields(vs, field) == FAIL)
        HGOTO_ERROR(DFE_BADFIELDS,FAIL);

    if (n != VSwrite(vs, buf, n, FULL_INTERLACE))
        HGOTO_ERROR(DFE_BADATTACH,FAIL);

    if(VSsetname(vs, vsname)==FAIL)
        HGOTO_ERROR(DFE_BADVSNAME,FAIL);

    if(VSsetclass(vs, vsclass)==FAIL)
        HGOTO_ERROR(DFE_BADVSCLASS,FAIL);

    ref = VSQueryref(vs);
    if(VSdetach(vs)==FAIL)
        HGOTO_ERROR(DFE_CANTDETACH,FAIL);

    ret_value = ((int32) ref);

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

  /* Normal function cleanup */
#ifdef HAVE_PABLO
    TRACE_OFF(VH_mask, ID_VHstoredatam);
#endif /* HAVE_PABLO */
  return ret_value;
}   /* VHstoredatam */

/* ------------------------ VHmakegroup ------------------------------- */
/*
   NAME
   VHmakegroup -- Creates a vgroup to store pairs of tag/ref.
   USAGE
   int32 VHmakegroup (f, tagarray, refarray , n, vgname, vgclass)
   HFILEID f;           IN: File id, returned from Hopen.
   int32   tagarray[];  IN: Array of tags to be stored in the vgroup.
   int32   refarray[];  IN: Array of refs to be stored in the vgroup.
   int32   n;           IN: Number of tags/refs in the tagarray/refarray.
   char    * vgname;    IN: Name of the new vgroup.
   char    * vgclass;   IN: Class of the new vgroup.

   RETURNS
   On success returns reference number of the new vgroup, a positive integer;
   on failure returns -1.
   DESCRIPTION
   Takes an array of tags and and array of refs and create a vgroup to
   store them. You tell it how many tag/ref pairs there are. You must
   also give the vgroup a name.i Creating EMPTY vgroups is allowed.
   VHmakegroup does bot check if a tag/ref is valid or exist,
   but ALL tag/ref pairs MUST be unique.

   Returns  -1 if error; ref of the new vgroup (a +ve integre) if ok.

   --------------------------------------------------------------------------- */

int32
VHmakegroup(HFILEID f, int32 tagarray[], int32 refarray[], int32 n, char *vgname, char *vgclass)
{
    int32       ref, i;
    int32       vg;
    int32       ret_value = SUCCEED;
    CONSTR(FUNC, "VHmakegroup");

#ifdef HAVE_PABLO
    TRACE_ON(VH_mask, ID_VHmakegroup);
#endif /* HAVE_PABLO */

    if (( vg = Vattach(f, -1, "w"))== FAIL)
        HGOTO_ERROR(DFE_CANTATTACH,FAIL);

    if(vgname!=NULL)
        if(Vsetname(vg, vgname)==FAIL)
            HGOTO_ERROR(DFE_BADVGNAME,FAIL);

    if(vgclass!=NULL)
        if(Vsetclass(vg, vgclass)==FAIL)
            HGOTO_ERROR(DFE_BADVGCLASS,FAIL);

    for (i = 0; i < n; i++)
      {
          if ( Vaddtagref(vg, tagarray[i], refarray[i]) == FAIL)
              HGOTO_ERROR(DFE_CANTADDELEM,FAIL);
      }

    ref = VQueryref(vg);
    if(Vdetach(vg)==FAIL)
        HGOTO_ERROR(DFE_CANTDETACH,FAIL);

    ret_value = (ref);

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

  /* Normal function cleanup */
#ifdef HAVE_PABLO
    TRACE_OFF(VH_mask, ID_VHmakegroup);
#endif /* HAVE_PABLO */

  return ret_value;
}   /* VHmakegroup */

/* ------------------------------------------------------------------ */
