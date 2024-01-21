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

/* mfan.h,v 1.10 1996/05/23 21:53:08 georgev Exp */

/*------------------------------------------------------------------------------
 * File:    mfan.h
 * Purpose: header file for the Multi-file Annotation Interface
 * Invokes: 
 * Contents:
 *          Structure definitions: ANnode, ANentry, ANfile
 *          Constant definitions:  AN_DATA_LABEL, AN_DATA_DESC
 *          (-moved to hdf.h)       AN_FILE_LABEL, AN_FILE_DESC
 * Remarks: none
 *----------------------------------------------------------------------------*/

#ifndef _MFAN_H  /* avoid re-inclusion */
#define _MFAN_H

#include "hdf.h"
#include "tbbt.h" /* Threaded-balanced binary tree stuff */

#if 0
/* enumerated types of the varous annotation types 
 * NOTE: moved to hdf.h */
typedef enum 
{ 
  AN_DATA_LABEL = 0, /* Data label */
  AN_DATA_DESC,      /* Data description */
  AN_FILE_LABEL,     /* File label */
  AN_FILE_DESC       /* File description */
} ann_type;
#endif

/* This sturcture is used to find which file the annotation belongs to
 * and use the subsequent file specific annotation 'key' to find the 
 * annotation. The ANnodelist keeps track of all anotations across 
 * all the open files. */
typedef struct ANnode
{
  int32   file_id;  /* which file this annotation belongs to */
  int32   ann_key;  /* type/ref -used to find annotation in file's type tree*/
  intn    new_ann;  /* flag */
} ANnode;

/*
 * This structure is an entry in the label/desc tree
 * for a label/desc in the file, it gives the ref of the label/desc,
 * and the tag/ref of the data item to which the label/desc relates */
typedef struct ANentry
{
  int32   ann_id;      /* annotation id */
  uint16  annref;      /* ref of annotation */
  uint16  elmtag;      /* tag of data */
  uint16  elmref;      /* ref of data */
} ANentry;

/* Structure for each file opened to insert in tree */
typedef struct ANfile
{
  char    *filename;      /* File name */
  int32   access_mode;    /* access mode this file was opened with */
  intn    an_num[4];      /* Holds number of annotations found of each type */
  TBBT_TREE *an_tree[4];  /* tbbt trees for each type of annotation in file 
                           * i.e. file/data labels and descriptions */
} ANfile;

#ifdef MFAN_C
/* WE ARE IN MAIN ANNOTATION SOURCE FILE "mfan.c" */

/* PRIVATE variables and defintions */
EXPORT TBBT_TREE *ANfilelist = NULL; /* List of open files */
EXPORT TBBT_TREE *ANnodelist = NULL; /* List of all anotations across files */
PRIVATE intn    num_anns   = 0;    /* total number of annotations 
                                      i.e. all files */

/* Used to create unique 32bit keys from annotation type and reference number 
 *  This key is used to add nodes to ANnodelist. 
 *  ----------------------------
 *  | t(16bits) | r(16bits) |
 *  -----------------------------*/
#define AN_CREATE_KEY(t,r) ((((int32)t & 0xffff) << 16) | r)

/* Obtain Reference number from key */
#define AN_KEY2REF(k)      ((uint16)((int32)k & 0xffff))

/* Obtain Annotation type from key */
#define AN_KEY2TYPE(k)     ((int32)((int32)k >> 16))

#else /* !MFAN_C */
/* WE are NOT in main ANNOTATION source file
 * Nothing EXPORTED  */


/******************************************************************************
 NAME
      ANstart - open file for annotation handling

 DESCRIPTION
      Start annotation handling on the file return a annotation ID to the file.

 RETURNS
      A file ID or FAIL.
*******************************************************************************/
extern int32 ANstart(int32 file_id /* IN: file to start annotation access on */);

/******************************************************************************
 NAME
    ANfileinfo - Report high-level information about the ANxxx interface for a given file.

 DESCRIPTION
    Reports general information about the number of file and object(i.e. data)
    annotations in the file. This routine is generally used to find
    the range of acceptable indices for ANselect calls.

 RETURNS
    Returns SUCCEED if successful and FAIL othewise

*******************************************************************************/
extern intn  ANfileinfo(int32 an_id, /* IN:  annotation interface id */
                        int32 *n_file_label, /* OUT: the # of file labels */
                        int32 *n_file_desc, /* OUT: the # of file descriptions */
                        int32 *n_obj_label, /* OUT: the # of object labels */ 
                        int32 *n_obj_desc /* OUT: the # of object descriptions */);

/******************************************************************************
 NAME
     ANend - End annotation access to file file

 DESCRIPTION
     End annotation access to file.

 RETURNS
     SUCCEED if successful and  FAIL otherwise.
*******************************************************************************/
extern int32 ANend(int32 an_id /* IN: Annotation ID of file to close */);

/******************************************************************************
 NAME
	ANcreate - create a new element annotation and return a handle(id)

 DESCRIPTION
        Creates a data annotation, returns an 'an_id' to work with the new 
        annotation which can either be a label or description.
        Valid annotation types are AN_DATA_LABEL for data labels and 
        AN_DATA_DESC for data descriptions.

 RETURNS
        An ID to an annotation which can either be a label or description.
*******************************************************************************/
extern int32 ANcreate(int32 an_id, /* IN: annotation interface ID */
                      uint16 elem_tag, /* IN: tag of item to be assigned annotation */
                      uint16 elem_ref, /* IN: reference number of itme to be assigned ann*/
                      ann_type type /* IN: annotation type */);


/******************************************************************************
 NAME
	ANcreatef - create a new file annotation and return a handle(id)

 DESCRIPTION
        Creates a file annotation, returns an 'an_id' to work with the new 
        file annotation which can either be a label or description.
        Valid annotation types are AN_FILE_LABEL for file labels and
        AN_FILE_DESC for file descritpions.

 RETURNS
        An ID to an annotation which can either be a file label or description
*******************************************************************************/
extern int32 ANcreatef(int32 an_id, /* IN: annotation interface ID */
                       ann_type type /* IN:  annotation type */);

/******************************************************************************
 NAME
	ANselect - get an annotation ID from index of 'type'

 DESCRIPTION
        Get an annotation Id from index of 'type'.
        The position index is ZERO based

 RETURNS
        An ID to an annotation type which can either be a label or description 
*******************************************************************************/
extern int32 ANselect(int32 an_id, /* IN: annotation interface ID */
                      int32 index, /* IN: index of annottion to get ID for */
                      ann_type type /* IN: annotation type */);

/******************************************************************************
 NAME
   ANnumann - find number of annotation of 'type' that  match the given element tag/ref 

 DESCRIPTION
       Find number of annotation of 'type' for the given element 
       tag/ref pair.

 RETURNS
       number of annotation found if successful and FAIL (-1) otherwise

*******************************************************************************/
extern intn  ANnumann(int32 an_id, /* IN: annotation interface id */
                      ann_type type, /* IN: annotation type */
                      uint16 elem_tag, /* IN: tag of item of which this is annotation */
                      uint16 elem_ref /* IN: ref of item of which this is annotation*/);

/******************************************************************************
 NAME
   ANannlist - generate list of annotation ids of 'type' that match the given element tag/ref 

 DESCRIPTION
       Find and generate list of annotation ids of 'type' for the given 
       element tag/ref pair.

 RETURNS
       number of annotations ids found if successful and FAIL (-1) otherwise

*******************************************************************************/
extern intn  ANannlist(int32 an_id, /* IN: annotation interface id */
                       ann_type type, /* IN: annotation type */
                       uint16 elem_tag, /* IN: tag of item of which this is annotation */
                       uint16 elem_ref, /* IN: ref of item of which this is annotation*/
                       int32 ann_list[] /* OUT: array of ann_id's that match criteria.*/);

/******************************************************************************
 NAME
       ANannlen - get length of annotation givne annotation id

 DESCRIPTION
       Uses the annotation id to find ann_key & file_id

 RETURNS
       length of annotation if successful and FAIL (-1) otherwise

*******************************************************************************/
extern int32 ANannlen(int32 ann_id /* IN: annotation id */);

/******************************************************************************
 NAME
       ANwriteann - write annotation given ann_id

 DESCRIPTION
       Checks for pre-existence of given annotation, replacing old one if it
       exists. Writes out annotation.

 RETURNS
       SUCCEED (0) if successful and FAIL (-1) otherwise

*******************************************************************************/
extern int32 ANwriteann(int32 ann_id, /* IN: annotation id */
                        const char *ann, /* IN: annotation to write */
                        int32 annlen /* IN: length of annotation*/);

/******************************************************************************
 NAME
       ANreadann - read annotation given ann_id

 DESCRIPTION
       Gets tag and ref of annotation.  Finds DD for that annotation.
       Reads the annotation, taking care of NULL terminator, if necessary.

 RETURNS
       SUCCEED (0) if successful and FAIL (-1) otherwise

*******************************************************************************/
extern int32 ANreadann(int32 ann_id, /* IN: annotation id (handle) */
                       char *ann, /* OUT: space to return annotation in */
                       int32 maxlen /* IN: size of space to return annotation in */);

/******************************************************************************
 NAME
	ANendaccess - end access to an annotation given it's id

 DESCRIPTION
        Terminates access to an annotation. For now does nothing

 RETURNS
        SUCCEED or FAIL
*******************************************************************************/
extern intn  ANendaccess(int32 ann_id /* IN: annotation id */);

extern int32 ANget_tagref(int32 an_id, int32 index, ann_type type,
                          uint16 *ann_tag, uint16 *ann_ref);

extern int32 ANid2tagref(int32 ann_id, uint16 *ann_tag, uint16 *ann_ref);

extern int32 ANtagref2id(int32 an_id, uint16 ann_tag, uint16 ann_ref);

extern uint16 atype2tag(ann_type atype);

extern ann_type tag2atype(uint16 atag);

extern intn ANdestroy(void);
#endif /* !MFAN_C */

#endif /* _MFAN_H */
