/******************************************************************************
** $Id: macros.h,v 2.15 1996/02/22 21:43:51 gerd Exp gerd $
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

 typedef struct mACRO				   /*			     */
 { char		*mc_name;			   /*			     */
   char		*mc_value;			   /*			     */
   int		mc_used;			   /*			     */
   struct mACRO *mc_next;			   /*			     */
 } SMacro, *Macro;				   /*			     */

#define MacNull		(Macro)0

#define MacroName(M)	((M)->mc_name)
#define MacroVal(M)	((M)->mc_value)
#define MacroValue(M)	((M)->mc_value)
#define MacroCount(M)	((M)->mc_used)
#define NextMacro(M)	((M)->mc_next)


#ifdef __STDC__
#define _ARG(A) A
#else
#define _ARG(A) ()
#endif
 Macro new_macro _ARG((char *name,char *val,Macro next,int count));/* macros.c*/
 char * get_item _ARG((char * name,int type));	   /* macros.c               */
 char * get_key_name _ARG((char *symbol));	   /* macros.c               */
 char * look_macro _ARG((char *name,int add));	   /* macros.c               */
 void def_field_type _ARG((char * s));		   /* macros.c               */
 void def_item _ARG((char * name,char * value));   /* macros.c               */
 void def_macro _ARG((char *name,char *val,int count));/* macros.c           */
 void dump_mac _ARG((char *fname,int allp));	   /* macros.c               */
 void init_macros _ARG((void));			   /* macros.c               */
 void save_key _ARG((char * symbol,char * key));   /* macros.c               */
