/* exp_closetcl.c - close tcl files */

/* isolated in it's own file since it has hooks into Tcl and exp_clib user */
/* might like to avoid dragging it in */

#include "expect_cf.h"
#include "tclInt.h"

void (*exp_close_in_child)() = 0;

#if TCL_MAJOR_VERSION == 7 && TCL_MINOR_VERSION >= 5
Tcl_Interp *exp_close_files_interp;
#endif

void
exp_close_tcl_files()
{
#if TCL_MAJOR_VERSION < 7 || TCL_MINOR_VERSION < 5
	int i;

	/* So much for close-on-exec.  Tcl doesn't mark its files that way */
	/* everything has to be closed explicitly. */

	for (i=3; i<tclNumFiles;i++) close(i);
#else /* Using Tcl 7.5 or greater.  */

	/* Majorly gross.  We essentially duplicate
	   Tcl_DeleteFileTable, with minor changes.  */
	Tcl_HashTable *hTablePtr;
	Tcl_HashEntry *hPtr;
	Tcl_HashSearch hSearch;
	TclOpenFile *oFilePtr;
	char *key;

	hTablePtr = (Tcl_HashTable *) Tcl_GetAssocData (exp_close_files_interp,
							"tclFileTable",
							(Tcl_InterpDeleteProc **) NULL);
	if (hTablePtr == (Tcl_HashTable *) NULL)
	  return;
	for (hPtr = Tcl_FirstHashEntry(hTablePtr, &hSearch);
	     hPtr != (Tcl_HashEntry *) NULL;
	     hPtr = Tcl_NextHashEntry(&hSearch)) {

	  /* Skip standard files.  */
	  key = Tcl_GetHashKey(hTablePtr,hPtr);
	  if (!strcmp(key, "stdin") || !strcmp(key, "stdout")
	      || !strcmp(key, "stderr"))
	    continue;

	  oFilePtr = (TclOpenFile *) Tcl_GetHashValue(hPtr);
	  if (oFilePtr == (TclOpenFile *) NULL)
	    continue;
	  if (oFilePtr->f != NULL)
	    close (fileno (oFilePtr->f));
	  if (oFilePtr->f2 != NULL)
	    close (fileno (oFilePtr->f2));
	}

#endif /* Tcl 7.5 or greater.  */
}
