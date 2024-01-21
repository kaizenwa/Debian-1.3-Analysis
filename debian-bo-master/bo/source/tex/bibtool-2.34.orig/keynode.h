/******************************************************************************
** $Id: keynode.h,v 2.10 1996/02/22 21:43:51 gerd Exp gerd $
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


 typedef struct kEYnODE
 { short int	  kn_type,
                  kn_pre,
                  kn_post;
   char           *kn_string;
#ifdef REGEX
   char           *kn_from,
		  *kn_to;
#endif
   struct kEYnODE *kn_next,
                  *kn_then,
                  *kn_else;
 } *KeyNode, SKeyNode;

#define NodeType(X)   ((X)->kn_type)
#define NodePre(X)    ((X)->kn_pre)
#define NodePost(X)   ((X)->kn_post)
#define NodeSymbol(X) ((X)->kn_string)
#define NodeNext(X)   ((X)->kn_next)
#define NodeThen(X)   ((X)->kn_then)
#define NodeElse(X)   ((X)->kn_else)
#ifdef REGEX
#define NodeFrom(X)   ((X)->kn_from)
#define NodeTo(X)     ((X)->kn_to)
#endif

#define NodeCountMask 0x100
#define NodePlusMask  0x200
#define NodeMinusMask 0x400
#define NodeSTRING    2048
#define NodeTEST      2049
#define NodeTESTneg   2050
#define NodeOR        2051
#define NodeSPECIAL   2052
