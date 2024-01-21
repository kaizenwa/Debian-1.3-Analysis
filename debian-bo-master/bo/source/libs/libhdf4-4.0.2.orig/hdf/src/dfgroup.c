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
static char RcsId[] = "@(#)1.26";
#endif

/* dfgroup.c,v 1.26 1996/06/19 21:22:26 koziol Exp */

/*-----------------------------------------------------------------------------
 * File:    dfgroup.c
 * Purpose: Low level functions for implementing groups
 * Invokes: df.c df.h
 * Contents:
 *  DFdiread   : read in the data identifier list from the group
 *  DFdiget    : get next data identifier from list
 *  DFdisetup  : get ready to store a list of data identifiers to write out
 *  DFdiput    : add a data identifier to the list to be written out
 *  DFdiwrite  : write out the list of data identifiers
 * Remarks: A group is a way of associating data elements with each other.
 *          It is a tag whose data is a list of tag/refs
 *          Each tag/ref combination is called a data identifier (DI).
 *---------------------------------------------------------------------------*/

#include "hdf.h"
#include "hfile.h"

#if 0
#define MAX_GROUPS 8
#endif

typedef struct DIlist_struct
  {
      uint8      *DIlist;
      intn        num;
      intn        current;
  }
DIlist     , *DIlist_ptr;

static DIlist_ptr Group_list[MAX_GROUPS] = {NULL};

#define GSLOT2ID(s) ((((uint32)GROUPTYPE & 0xffff) << 16) | ((s) & 0xffff))
#define VALIDGID(i) (((((uint32)(i) >> 16) & 0xffff) == GROUPTYPE) && \
                    (((uint32)(i) & 0xffff) < MAX_GROUPS))
#define GID2REC(i)  ((VALIDGID(i) ? (Group_list[(uint32)(i) & 0xffff]) : NULL))

/*-----------------------------------------------------------------------------
 * Name:    setgroupREC
 * Purpose: Add a group list into the internal structure and return an ID
 * Inputs:  list_rec: list to remember
 * Returns: FAIL on failure else a group ID to the list
 * Users:   other group routines
 * Invokes:
 * Remarks: Allocates internal storeage if necessary
 *---------------------------------------------------------------------------*/
PRIVATE int32
setgroupREC(DIlist_ptr list_rec)
{
    CONSTR(FUNC, "setgroupREC");
    int32       i;

    for (i = 0; i < MAX_GROUPS; i++)
        if (Group_list[i]==NULL)
          {
              Group_list[i] = list_rec;
              return GSLOT2ID(i);
          }

    HRETURN_ERROR(DFE_INTERNAL, FAIL);
}   /* setgroupREC */

/*-----------------------------------------------------------------------------
 * Name:    DFdiread
 * Purpose: Read a list of DIs into memory
 * Inputs:  file_id: HDF file pointer
 *          tag, ref: id of group which is to be read in
 * Returns: FAIL on failure else a group ID to the list
 * Users:   HDF systems programmers, DF8getrig, other routines
 * Invokes: HDvalidfid, DFIfind, DFgetelement
 * Remarks: assumes tag is a group
 *---------------------------------------------------------------------------*/

int32
DFdiread(int32 file_id, uint16 tag, uint16 ref)
{
    DIlist_ptr  new_list;
    CONSTR(FUNC, "DFdiread");
    int32       length;

    HEclear();

    if (!HDvalidfid(file_id))
        HRETURN_ERROR(DFE_ARGS, FAIL);

    /* Find the group. */
    length = Hlength(file_id, tag, ref);
    if (length == FAIL)
        HRETURN_ERROR(DFE_INTERNAL, FAIL);

    /* allocate a new structure to hold the group */
    new_list = (DIlist_ptr) HDmalloc((uint32) sizeof(DIlist));
    if (!new_list)
        HRETURN_ERROR(DFE_NOSPACE, FAIL);

    new_list->DIlist = (uint8 *) HDmalloc((uint32) length);
    if (!new_list->DIlist)
      {
          HDfree((VOIDP) new_list);
          HRETURN_ERROR(DFE_NOSPACE, FAIL);
      }

    new_list->num = (intn) (length / 4);
    new_list->current = 0;  /* no DIs returned so far */

    /* read in group */
    if (Hgetelement(file_id, tag, ref, (uint8 *) new_list->DIlist) < 0)
      {
          HDfree((VOIDP) new_list->DIlist);
          HDfree((VOIDP) new_list);
          HRETURN_ERROR(DFE_READERROR, FAIL);
      }
    return (int32) setgroupREC(new_list);
}

/*-----------------------------------------------------------------------------
 * Name:    DFdiget
 * Purpose: return next DI from the list of DIs in a group
 * Inputs:  list: handle to group (which is list of DIs)
 * Outputs: ptag: pointer to tag part of DI to be returned
 *          pref: pointer to ref part of DI to be returned
 * Returns: 0 on success, -1 on failure with error set
 * Users:   HDF systems programmers, DF8getrig, other routines
 * Invokes: none
 * Remarks: frees Dilist space when all DIs returned
 *---------------------------------------------------------------------------*/

intn
DFdiget(int32 list, uint16 *ptag, uint16 *pref)
{
    CONSTR(FUNC, "DFdiget");
    uint8      *p;
    DIlist_ptr  list_rec;

    list_rec = GID2REC(list);

    if (!list_rec)
        HRETURN_ERROR(DFE_ARGS, FAIL);
    if (list_rec->current >= list_rec->num)
        HRETURN_ERROR(DFE_INTERNAL, FAIL);

    /* compute address of Ndi'th di */
    p = (uint8 *) list_rec->DIlist + 4 * list_rec->current++;
    UINT16DECODE(p, *ptag);
    UINT16DECODE(p, *pref);

    if (list_rec->current == list_rec->num)
      {
          HDfree((VOIDP) list_rec->DIlist);    /*if all returned, free storage */
          HDfree((VOIDP) list_rec);
          Group_list[list & 0xffff] = NULL;     /* YUCK! BUG! */
      }
    return SUCCEED;
}

/*-----------------------------------------------------------------------------
 * Name:    DFdinobj
 * Purpose: return number of tag/refs in the group
 * Inputs:  list: handle to group (which is list of DIs)
 * Returns: number of tag/refs in the group on success,
 *  -1 on failure with error set
 * Users:   HDF systems programmers, hdp utility
 * Invokes: none
 * Remarks: nuttin'
 *---------------------------------------------------------------------------*/
intn
DFdinobj(int32 list)
{
    CONSTR(FUNC, "DFdinobj");
    DIlist_ptr  list_rec;

    list_rec = GID2REC(list);

    if (!list_rec)
        HRETURN_ERROR(DFE_ARGS, FAIL);

    return (list_rec->num);
}   /* DFdinobj() */

/*-----------------------------------------------------------------------------
 * Name:    DFdisetup
 * Purpose: setup space for storing a list of DIs to be written out
 * Inputs:  maxsize: maximum number of DIs expected in the list
 * Returns: FAIL on failure with error set
 *          else a group ID
 * Users:   HDF systems programmers, DF8putrig, other routines
 * Invokes: none
 * Remarks: This call should go away sometime.  Need better way to allocate
 *          space, possibly just use a big block of static space
 *---------------------------------------------------------------------------*/

int32
DFdisetup(int maxsize)
{
    CONSTR(FUNC, "DFdisetup");
    DIlist_ptr  new_list;

    new_list = (DIlist_ptr) HDmalloc((uint32) sizeof(DIlist));

    if (!new_list)
        HRETURN_ERROR(DFE_NOSPACE, FAIL);

    new_list->DIlist = (uint8 *) HDmalloc((uint32) (maxsize * 4));
    if (!new_list->DIlist)
      {
          HDfree((VOIDP) new_list);
          HRETURN_ERROR(DFE_NOSPACE, FAIL);
      }

    new_list->num = maxsize;
    new_list->current = 0;

    return setgroupREC(new_list);
}

/*-----------------------------------------------------------------------------
 * Name:    DFdiput
 * Purpose: add a DI to the list to be written out
 * Inputs:  tag, ref: DI to add
 * Returns: 0 on success, -1 on failure with error set
 * Users:   HDF systems programmers, DF8putrig, other routines
 * Invokes: none
 * Remarks: arg is tag/ref rather than DI for convenience
 *---------------------------------------------------------------------------*/

intn
DFdiput(int32 list, uint16 tag, uint16 ref)
{
    CONSTR(FUNC, "DFdiput");
    uint8      *p;
    DIlist_ptr  list_rec;

    list_rec = GID2REC(list);

    if (!list_rec)
        HRETURN_ERROR(DFE_ARGS, FAIL);
    if (list_rec->current >= list_rec->num)
        HRETURN_ERROR(DFE_INTERNAL, FAIL);

    /* compute address of Ndi'th di to put tag/ref in */
    p = (uint8 *) list_rec->DIlist + 4 * list_rec->current++;
    UINT16ENCODE(p, tag);
    UINT16ENCODE(p, ref);

    return SUCCEED;
}

/*-----------------------------------------------------------------------------
 * Name:    DFdiwrite
 * Purpose: Write DI list out to HDF file
 * Inputs:  file_id: HDF file pointer
 *          tag, ref: tag and ref of group whose contents is the list
 * Returns: 0 on success, -1 on failure with error set
 * Users:   HDF systems programmers, DF8putrig, other routines
 * Invokes: none
 * Remarks: frees storage for Dilist
 *---------------------------------------------------------------------------*/

intn
DFdiwrite(int32 file_id, int32 list, uint16 tag, uint16 ref)
{
    CONSTR(FUNC, "DFdiwrite");
    int32       ret;            /* return value */
    DIlist_ptr  list_rec;

    if (!HDvalidfid(file_id))
        HRETURN_ERROR(DFE_ARGS, FAIL);

    list_rec = GID2REC(list);

    if (!list_rec)
        HRETURN_ERROR(DFE_ARGS, FAIL);

    ret = Hputelement(file_id, tag, ref, list_rec->DIlist,
                      (int32) list_rec->current * 4);
    HDfree((VOIDP) list_rec->DIlist);
    HDfree((VOIDP) list_rec);
    Group_list[list & 0xffff] = NULL;   /* YUCK! BUG! */
    return (intn) ret;
}
