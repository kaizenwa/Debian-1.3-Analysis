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
static char RcsId[] = "@(#)1.16";
#endif

/* mfanf.c,v 1.16 1996/01/21 23:17:12 georgev Exp */

/*-----------------------------------------------------------------------------
 * File:     mfanf.c
 * Purpose:  C-stubs for multi-file Fortran annotation routines
 * Invokes:  C-Routines in "mfan.c"
 * Contents: SEE annotation source/header files "mfan.c" and "mfan.h"
 *           for more info on the annotation interface.
 *
 *  NOTES: TYPE here refers to file/data label/description types 
 *         They are AN_FILE_LABEL, AN_FILE_DESC, AN_DATA_LABEL, AN_DATA_DESC
 *         THE tag/ref refers to data tag/ref
 *
 *  C-stub for corresponding Fortran call(OBSOLETE)
 *  -------------------------------------
 *    acstart    - start annotation access on file and return file handle
 *               - called by afstart() in "mfanff.f"
 *
 *  C-stubs directly callable by Fortran Users
 *  ------------------------------------------
 *    afstart    - start annotation access on file and return annotaton id
 *    affileinfo - get number of file/data annotations in file. 
 *                 Indices returned are used in afselect() calls.
 *    afend      - end access to annotation handling on file
 *    afcreate   - create a new data annotation and return a handle
 *    affcreate  - create a new file annotation and return a handle
 *    afselect   - returns an annotation handle(ann_id) from index for 
 *                 a particular annotation TYPE. This handle is then used for
 *                 calls like afwriteann(), afreadann(), afannlen(),..etc
 *    afnumann   - return number of annotations that match TYPE/tag/ref
 *    afannlist  - return list of handles that match TYPE/tag/ref
 *    afannlen   - get length of annotation given handle
 *    afwriteann - write annotation given handle
 *    afreadann  - read annotation given handle
 *    afendaccess- end access to annotation using handle
 *---------------------------------------------------------------------------*/

#include "hdf.h"
#include "mfan.h"

/*-----------------------------------------------------------------------------
  FUNCTION NAMEING CONVENTION:
  ---------------------------
  This file contains the HDF-style C stubs for the multi-file Annotation
  interface. They call the corresponding C-functions in "mfan.c"

  The basic routines called by fortran will be of the form afxxxx

  If only a C stub is needed it will be named nafxxxx have the FNAME()
  function applied to it.

  If a Fortran stub is also required the fortran stub will be called
  afxxxx(mfanff.f) and the one in here will be nacxxx and again be FNAME()ed

-----------------------------------------------------------------------------*/

#if 0
/*-----------------------------------------------------------------------------
 * Name:    acstart
 * Purpose: C-stub for afstart()
 * Inputs:  file_id: id of HDF file
 *          acc_mode: File access mode 
 *          namelen:  length of filename
 * Returns: file handle on SUCCEED and FAIL otherwise
 * Users:   afstart(mfanff.f)
 * Invokes: ANstart()
 *---------------------------------------------------------------------------*/
FRETVAL(intf)
nacstart(intf *file_id, intf *acc_mode, intf *namelen)
{
  char *fn;
  intf ret;

  /* Convert fortran string to C-usable string */
  if ((fn = HDf2cstring(filename, (intn) *namelen)) == NULL)
    return FAIL;

  ret = ANstart((int32)file_id, (int32) *acc_mode);

  HDfree((VOIDP) fn);

  return (ret);
} /* nacstart() */
#endif

/*-----------------------------------------------------------------------------
 * Name:    afstart
 * Purpose: Open file for annoation handling
 * Inputs:  file_id: id of HDF file
 * Returns: annotation interface handle on SUCCEED and FAIL otherwise
 * Users:   
 * Invokes: ANstart()
 *---------------------------------------------------------------------------*/
FRETVAL(intf)
nafstart(intf *file_id)
{
  intf ret;

  ret = ANstart((int32)*file_id);

  return (ret);
} /* nafstart() */

/*-----------------------------------------------------------------------------
 * Name:    affileinfo
 * Purpose: Get number of file/data annotations in file. 
 * Inputs:  IN an_id:     annotation interface handle
 *          OUT num_flabel: number of file labels in file
 *          OUT num_fdesc:  number of file descriptions in file
 *          OUT num_olabel: number of data labels in file
 *          OUT num_odesc:  number of data descriptions in file
 * Returns: see ANfileinfo()
 * Users:   Fortran Users
 * Invokes: ANfileinfo()
 *---------------------------------------------------------------------------*/
FRETVAL(intf)
naffileinfo(intf *an_id, intf *num_flabel, intf *num_fdesc, intf *num_olabel,
            intf *num_odesc)
{
#ifdef LATER
  CONSTR(FUNC, "affileinfo");
#endif /* LATER */
  intf  ret;
  int32 nflabel, nfdesc, nolabel, nodesc; 

  ret = ANfileinfo((int32)*an_id, &nflabel, &nfdesc, &nolabel, &nodesc);

  /* fill in values to return */
  *num_flabel = nflabel;
  *num_fdesc  = nfdesc;
  *num_olabel = nolabel;
  *num_odesc  = nodesc;

  return ret;
} /* naffileinfo() */

/*-----------------------------------------------------------------------------
 * Name:    afend    
 * Purpose: End access to annotation handling on file
 * Inputs:  file_id:
 * Returns: see ANend()
 * Users:   Fortran Users
 * Invokes: ANend()
 *---------------------------------------------------------------------------*/
FRETVAL(intf)
nafend(intf *an_id)
{
#ifdef LATER
  CONSTR(FUNC, "afend");
#endif /* LATER */

  return (intf)ANend((int32) *an_id);
} /* nafend() */

/*-----------------------------------------------------------------------------
 * Name:    afcreate
 * Purpose: Create a new data annotation and return an annotation handle 
 * Inputs:  an_id: annotation interface handle
 *          etag:    tag of data to annotate
 *          eref:    ref of data to annotate
 *          atype:   annotation type AN_DATA_LABEL, AN_DATA_DESC
 * Returns: see ANcreate()
 * Users:   Fortran Users
 * Invokes: ANcreate()
 *---------------------------------------------------------------------------*/
FRETVAL(intf)
nafcreate(intf *an_id, intf *etag, intf *eref, intf *atype)
{
#ifdef LATER
  CONSTR(FUNC, "afcreate");
#endif /* LATER */

  return (intf)ANcreate((int32)*an_id,(uint16)*etag,(uint16)*eref,(ann_type)*atype);
} /* nafcreate() */

/*-----------------------------------------------------------------------------
 * Name:    affcreate
 * Purpose: Create a new file annotation and return an annotation handle
 * Inputs:  an_id: annottion inteface handle
 *          atype:   annotation type AN_FILE_LABEL, AN_DATA_DESC
 * Returns: see ANcreatef()
 * Users:   Fortran Users
 * Invokes: ANcreatf()
 *---------------------------------------------------------------------------*/
FRETVAL(intf)
naffcreate(intf *an_id, intf *atype)
{
#ifdef LATER
  CONSTR(FUNC, "affcreate");
#endif /* LATER */

  return (intf)ANcreatef((int32)*an_id,(ann_type)*atype);
} /* naffcreate() */

/*-----------------------------------------------------------------------------
 * Name:    afselect
 * Purpose: returns an annotation handle(ann_id) from index for 
 *          a particular annotation TYPE. This handle is then used for
 *          calls like afwriteann(), afreadann(), afannlen(),..etc
 * Inputs:  an_id: annotation interface handle
 *          index:   index for particular annoation type. Usually based on
 *                   number of a particular type obtained from affileinfo()call.
 *                   ZERO based.
 *          atype:   annotation type AN_FILE_LABEL, AN_FILE_DESC, AN_DATA_LABEL
 *                   AN_DATA_DESC
 * Returns: see ANselect()
 * Users:   Fortran Users
 * Invokes: ANselect()
 *---------------------------------------------------------------------------*/
FRETVAL(intf)
nafselect(intf *an_id, intf *index, intf *atype)
{
#ifdef LATER
  CONSTR(FUNC, "afselect");
#endif /* LATER */

  return (intf)ANselect((int32)*an_id,(int32)*index, (ann_type)*atype);
} /* nafselect() */

/*-----------------------------------------------------------------------------
 * Name:    afnumann
 * Purpose: Return number of annotations that match TYPE/tag/ref
 * Inputs:  an_id: annotation interface handle
 *          atype:   annotation type AN_DATA_LABEL, AN_DATA_DESC
 *          etag:    data tag to match
 *          eref:    data ref to match
 * Returns: see ANnumann()
 * Users:   Fortran Users
 * Invokes: ANnumann()
 *---------------------------------------------------------------------------*/
FRETVAL(intf)
nafnumann(intf *an_id, intf *atype, intf *etag, intf *eref)
{
#ifdef LATER
  CONSTR(FUNC, "afnumann");
#endif /* LATER */

  return (intf)ANnumann((int32)*an_id,(ann_type)*atype,(uint16)*etag,(uint16)*eref);
} /* nafnumann() */

/*-----------------------------------------------------------------------------
 * Name:    afannlist
 * Purpose: Return list of handles that match TYPE/tag/ref
 * Inputs:  IN an_id: annotation inteface handle
 *          IN atype:   annotation type AN_DATA_LABEL, AN_DATA_DESC
 *          IN etag:    data tag to match
 *          IN eref:    data ref to match
 *          OUT alist[]: list of annotation handles found that match tag/ref
 * Returns: number of annoations found that match else FAIL. see ANannlist()
 * Users:   Fortran Users
 * Invokes: ANnumann(), ANannlist()
 *---------------------------------------------------------------------------*/
FRETVAL(intf)
nafannlist(intf *an_id, intf *atype, intf *etag, intf *eref, intf alist[])
{
  CONSTR(FUNC, "afannlist");
  intf  ret;
  int32 *tempanlist;
  intf  nanns;
  intn  i;

  /* Get number of annotations that match tag/ref pair */
  nanns =ANnumann((int32)*an_id,(ann_type)*atype,(uint16)*etag,(uint16)*eref);
  if (nanns < 0)
    HE_REPORT_RETURN("ANnumann: failed to any annotations", FAIL);

  /* create annlist with true int32s to maintain compatibility
  ** with machines that allocate less than 32 bits per int. */
  if ((tempanlist = (int32 *) HDmalloc(nanns * sizeof(int32))) == NULL)
    HRETURN_ERROR(DFE_NOSPACE, FAIL);

  /* Get list of annoation handles to return */
  ret = ANannlist((int32)*an_id,(ann_type)*atype,(uint16)*etag,(uint16)*eref,
                  tempanlist);
  if (ret < 0)
    HE_REPORT_RETURN("ANannlist:failed to any annotations", FAIL);

  /* move annotation handles into caller's alist */
  for (i = 0; i < nanns; i++)
    alist[i] = tempanlist[i];

  HDfree((VOIDP) tempanlist); /* free allocated space */

  return ret;
} /* nafannlist() */

/*-----------------------------------------------------------------------------
 * Name:    afannlen
 * Purpose: Get length of annotation given handle
 * Inputs:  an_id:annotation handle
 * Returns: see ANannlen()
 * Users:   Fortran Users
 * Invokes: ANannlen()
 *---------------------------------------------------------------------------*/
FRETVAL(intf)
nafannlen(intf *an_id)
{
#ifdef LATER
  CONSTR(FUNC, "afannlen");
#endif /* LATER */

    return (intf)ANannlen((int32)*an_id);
} /* nafannlen() */

/*-----------------------------------------------------------------------------
 * Name:    afwriteann
 * Purpose: Write annotation given handle
 * Inputs:  ann_id: annotation handle
 *          ann:   annotation to write out
 *          annlen:length of annotation
 * Returns: see ANwriteann()
 * Users:   Fortran Users
 * Invokes: ANwriteann()
 *---------------------------------------------------------------------------*/
FRETVAL(intf)
nafwriteann(intf *ann_id,_fcd ann, intf *annlen)
{
#ifdef LATER
  CONSTR(FUNC, "afwriteann");
#endif /* LATER */

    char       *iann = NULL;
    int32       cstr_len = 0;
    intf        status;

    iann = HDf2cstring(ann, (intn) *annlen);
    if (!iann)
        return(FAIL);
    cstr_len = HDstrlen(iann); /* length of C-string */

    status = ANwriteann((int32)*ann_id, (char *) _fcdtocp(ann), 
                        (int32)*annlen);

    HDfree(iann);

    return status;
} /* nafwriteann() */

/*-----------------------------------------------------------------------------
 * Name:    afreadann
 * Purpose: Read annotation given handle
 * Inputs:  ann_id:  annotation handle
 *          ann:    annotation read
 *          maxlen: maximum space allocted for "ann"
 * Returns: see ANreadann() (SUCCEED (0) if successful, else FAIL (-1))
 * Users:   Fortran Users
 * Invokes: ANreadann()
 *---------------------------------------------------------------------------*/
FRETVAL(intf)
nafreadann(intf *ann_id,_fcd ann, intf *maxlen)
{
    CONSTR(FUNC, "afreadann");
    char	*iann = NULL;
    intn        status;

    if (*maxlen)
        iann = (char *) HDmalloc((uint32) *maxlen + 1);

    if (!iann)
	HRETURN_ERROR(DFE_NOSPACE, FAIL);

    status = ANreadann((int32)*ann_id, iann, (int32)*maxlen);

    HDpackFstring(iann, _fcdtocp(ann), (intn) *maxlen);

    if (iann)
        HDfree(iann);

    return status;
} /* nafreadann() */

/*-----------------------------------------------------------------------------
 * Name:    afendaccess
 * Purpose: End access to annotation using handle
 * Inputs:  ann_id:annotation handle
 * Returns: see ANendaccess()
 * Users:   Fortran Users
 * Invokes: ANendaccess()
 *---------------------------------------------------------------------------*/
FRETVAL(intf)
nafendaccess(intf *ann_id)
{
#ifdef LATER
  CONSTR(FUNC, "afendaccess");
#endif /* LATER */

  return (intf)ANendaccess((int32)*ann_id);
} /* nafendaccess() */



