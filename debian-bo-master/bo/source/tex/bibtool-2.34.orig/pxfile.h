/******************************************************************************
** $Id: pxfile.h,v 2.12 1996/02/22 21:43:51 gerd Exp gerd $
**=============================================================================
** 
** This file is part of BibTool.
** It is distributed under the GNU General Public License.
** See the file COPYING for details.
** 
** (c) 1996 Gerd Neugebauer
** 
** Net: gerd@informatik.uni-koblenz.de
** 
******************************************************************************/

#include <stdio.h>
#include "bibtool.h"


#ifdef MSDOS
#define DEFAULT_PATTERN "%s\\%s"
#else
#define DEFAULT_PATTERN "%s/%s"
#endif

extern char * px_filename;


extern FILE * px_fopen(
#ifdef __STDC__
	char  * name,
	char  * mode,
	char  **pattern,
	char  **path,
	int   (*show)()
#endif
	);

extern char **px_s2p(
#ifdef __STDC__
	char  * s,
	int   sep
#endif
	);

