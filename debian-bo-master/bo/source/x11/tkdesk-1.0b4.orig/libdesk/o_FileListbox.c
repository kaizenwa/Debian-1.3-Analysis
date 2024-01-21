/* ============================================================================
 *
 * File:	o_FileListbox.c
 * Project:	TkDesk
 * Started:	14.02.95
 * Changed:	14.02.95
 *
 * Description:	Contains optimizations of tcl code from the file
 *              ../tcldesk/FileListbox.tcl. These are implemented as
 *              tcl commands.
 *
 * Copyright (C) 1996  Christian Bolik
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 * See the file "COPYING" in the base directory of this distribution
 * for more.
 *
 * ----------------------------------------------------------------------------
 *
 * Functions:
 *
 *
 * ========================================================================= */

#include "libdesk.h"


/* ============================================================================
 * Name   : dsk_ppflist_Cmd (tcl: dsk_ppflist)
 * In     : ...
 * Out    : ...
 * Desc   : Postprocesses the file list read by dsk_ls. Does such things as
 *          tagging and file masking.
 * Side-FX: none
 * ------------------------------------------------------------------------- */

int dsk_ppflist_Cmd (clientData, interp, argc, argv)
     ClientData clientData;
     Tcl_Interp *interp;
     int argc;
     char *argv[];
{
    int i = 0, fargc, j, targc, k, typechar, largc,
	mtags = 0, mtargc, ntd, l, add_images, notmatch, notmatch_set = 0;
    char **fargv, *flist, fnametc[256], fname[256], cmd[256],
	*mask, maskbuf[64], **targv, *taglist, *tagmatch, buf[256], *this,
	**largv, *list, **mtargv, *bufp, bigbuf[1024], cmdarg[4][256],
	typec = '\0', escbuf[1024];
    char *cmdargv[4];
    Tcl_DString dbigcmd;

    for (i = 0; i < 4; i++)
	cmdargv[i] = cmdarg[i];
    i = 0; /* importaaaaant!!! */
    
    tcl_getvar ("this", this);

    /* get and split the taglist */
    tcl_getvar ("taglist", taglist);
    if (Tcl_SplitList (interp, taglist, &targc, &targv) == TCL_ERROR)
      return TCL_ERROR;

    tcl_getvar ("mask", mask);
    /*sprintf (maskbuf, "%s?", mask);*/
    strcpy (maskbuf, mask);
    ot_getboolean ("typechar", NULL, &typechar);
    ot_getboolean ("notrivialdirs", NULL, &ntd);
    ot_getboolean ("add_icons", NULL, &add_images);
    
    /* flist has been read by dsk_ls */
    tcl_getvar ("flist", flist);
    if (Tcl_SplitList (interp, flist, &fargc, &fargv) == TCL_ERROR)
      return TCL_ERROR;

    /*
     *  Loop through the file list. Skip files that don't match $mask.
     *  Append the index of each file to the tag list it matches.
     */
    for (j = 0; j < fargc; j++) {
	
	/* Split the file entry ($flist may be a long listing) */
	/*if (Tcl_SplitList (interp, fargv[j], &feargc, &feargv) == TCL_ERROR)
	  return TCL_ERROR;*/

	/* fnametc points to the filename with trailing type character */
	/*fnametc = feargv[feargc - 1];*/
	for (l = 0; fargv[j][l] && fargv[j][l] != '\\'; l++) {
	    fnametc[l] = fargv[j][l];
	    typec = fnametc[l];
	}
	fnametc[l] = 0;

	/* strip the type character from the filename */
	strcpy (cmdarg[0], "dsk_striptc");	    
	strcpy (cmdarg[1], fnametc);
	if (dsk_striptc_Cmd (NULL, interp, 2, cmdargv) == TCL_ERROR)
	  return TCL_ERROR;
	strcpy (fname, interp->result);

	/* skip the file entry if it doesn't match $mask
	   and is not a directory name */
	if (typec != '/' && typec != '-' && !Tcl_StringMatch (fname, maskbuf))
	    continue;

	/* loop through $taglist until a matching tag is found */
	notmatch = 1;
	for (k = 0; k < targc; k++) {
	    sprintf (buf, "%s,match", targv[k]);
	    tcl_getvar2 ("tags", buf, tagmatch);

	    /* if the filename matches this tag append its index */
	    if (Tcl_StringMatch (fnametc, tagmatch)) {
		sprintf (buf, "%d", i);
		if (!(Tcl_SetVar2 (interp, "mt", targv[k], buf,
		     TCL_LIST_ELEMENT | TCL_APPEND_VALUE | TCL_LEAVE_ERR_MSG)))
		  return TCL_ERROR;
		mtags = 1;
		notmatch = 0;
		break;
	    }
	}
	/* add_images: need also line-nr.s of "normal" files */
	if (notmatch && add_images) {
	    notmatch_set = 1;
	    sprintf (buf, "%d", i);
	    if (!(Tcl_SetVar (interp, "mt_notmatch", buf,
		    TCL_LIST_ELEMENT | TCL_APPEND_VALUE | TCL_LEAVE_ERR_MSG)))
		return TCL_ERROR;
	}

	/* append the file entry to the file list */
	if (typechar) {
	    strcpy (cmdarg[0], "dsk_striptc");
	    strcpy (cmdarg[1], "-keep");
	    strcpy (cmdarg[2], fargv[j]);
	    if (dsk_striptc_Cmd (NULL, interp, 3, cmdargv) == TCL_ERROR)
	      return TCL_ERROR;
	} else {
	    strcpy (cmdarg[0], "dsk_striptc");	    
	    strcpy (cmdarg[1], fargv[j]);
	    if (dsk_striptc_Cmd (NULL, interp, 2, cmdargv) == TCL_ERROR)
	      return TCL_ERROR;
	}
	unescape_chars (interp->result, escbuf);
	if (!(Tcl_SetVar (interp, "list", escbuf,
		   TCL_LIST_ELEMENT | TCL_APPEND_VALUE | TCL_LEAVE_ERR_MSG)))
	    return TCL_ERROR;

	/*free (feargv);*/
	i++;
    }

    /* set the label of the menu button of this FileListbox */
    strcpy (bigbuf, "$this.mb config -state normal -text \"[$this _mb_label]\"");
    if (Tcl_Eval (interp, bigbuf) == TCL_ERROR)
      return TCL_ERROR;

    /* repack the menu button (it may not be mapped) */
    sprintf (cmd, "pack %s.mb -in %s.fMb -before %s.lFiles -fill x "
	     "-side left -expand yes", this, this, this);
    tcl_invoke (cmd);

    tcl_getvar ("list", list);
    if (Tcl_SplitList (interp, list, &largc, &largv) == TCL_ERROR)
      return TCL_ERROR;
    if (!ntd && !strcmp(mask, "*"))
	largc -= 2;

    /* fill the list box with the filelist */
    Tcl_DStringInit (&dbigcmd);
    Tcl_DStringAppend (&dbigcmd, this, -1);
    Tcl_DStringAppend (&dbigcmd, ".dlb config -list {", -1);
    Tcl_DStringAppend (&dbigcmd, list, -1);
    Tcl_DStringAppend (&dbigcmd, "}", -1);
    /*tcl_invoke (dbigcmd.string);*/
    if (Tcl_Eval (interp, dbigcmd.string) == TCL_ERROR) {
	return TCL_ERROR;
    }
    Tcl_DStringFree (&dbigcmd);

    if (largc == 1) {
	sprintf (cmd, "%s.lFiles config -text \" 1 Item\"", this);
	tcl_invoke (cmd);
    } else {
	sprintf (cmd, "%s.lFiles config -text \" %d Items\"", this, largc);
	tcl_invoke (cmd);
    }

    /* now the tagging: */

    if (mtags) {
	tcl_invoke ("array names mt");
	strcpy (buf, interp->result);
	if (Tcl_SplitList (interp, buf, &mtargc, &mtargv) == TCL_ERROR)
	  return TCL_ERROR;

	for (k = 0; k < mtargc; k++) {
	    tcl_getvar2 ("mt", mtargv[k], bufp);
	    Tcl_DStringInit (&dbigcmd);
	    sprintf (buf, "%s.dlb tag add %s \"", this, mtargv[k]);
	    Tcl_DStringAppend (&dbigcmd, buf, -1);
	    Tcl_DStringAppend (&dbigcmd, bufp, -1);
	    Tcl_DStringAppend (&dbigcmd, "\"", -1);	    
	    tcl_invoke (dbigcmd.string);
	    Tcl_DStringFree (&dbigcmd);

	    if (add_images) {
		Tcl_DStringInit (&dbigcmd);
		sprintf (buf, "%s imginsert %s \"", this, mtargv[k]);
		Tcl_DStringAppend (&dbigcmd, buf, -1);
		Tcl_DStringAppend (&dbigcmd, bufp, -1);
		Tcl_DStringAppend (&dbigcmd, "\"", -1);	    
		tcl_invoke (dbigcmd.string);
		Tcl_DStringFree (&dbigcmd);
	    }
	}

	free (mtargv);
    }
    
    if (add_images && notmatch_set) {
	tcl_getvar ("mt_notmatch", bufp);
	Tcl_DStringInit (&dbigcmd);
	sprintf (buf, "%s imginsert \"\" \"", this);
	Tcl_DStringAppend (&dbigcmd, buf, -1);
	Tcl_DStringAppend (&dbigcmd, bufp, -1);
	Tcl_DStringAppend (&dbigcmd, "\"", -1);	    
	tcl_invoke (dbigcmd.string);
	Tcl_DStringFree (&dbigcmd);
    }
	
    free (fargv);
    free (targv);
    free (largv);

    Tcl_ResetResult (interp);
    return TCL_OK;
} /* dsk_ppflist */

