/******************************************************************************
** $Id: names.h,v 0.00 1996/04/18 18:31:22 gerd Exp $
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

#define NoName		0
#define NameFirst	1
#define NameLast	2
#define NameVon		3
#define NameJr		4
#define NameString	5

 typedef struct nameNODE			   /*                        */
 { int		   nn_type;			   /*                        */
   int		   nn_strip;			   /*                        */
   char		   *nn_pre;			   /*                        */
   char		   *nn_mid;			   /*                        */
   char		   *nn_post;			   /*                        */
   struct nameNODE *nn_next;			   /*                        */
 } SNameNode, *NameNode;			   /*                        */

#define NameType(NN)  ((NN)->nn_type)
#define NameStrip(NN) ((NN)->nn_strip)
#define NamePre(NN)   ((NN)->nn_pre)
#define NameMid(NN)   ((NN)->nn_mid)
#define NamePost(NN)  ((NN)->nn_post)
#define NextName(NN)  ((NN)->nn_next)
#define NameNULL      ((NameNode)0)

#ifdef __STDC__
#define _ARG(A) A
#else
#define _ARG(A) ()
#endif
 NameNode name_format _ARG((char *s));		   /* names.c                */
 char * pp_names _ARG((char *s,NameNode format,char *trans,int max,char *namesep,char *etal));/* names.c*/
 char * pp_list_of_names _ARG((char **wa,NameNode format,char *trans,int max,char *comma,char *and,char *namesep,char *etal));/* names.c*/

/*---------------------------------------------------------------------------*/
