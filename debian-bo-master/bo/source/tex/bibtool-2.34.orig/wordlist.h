/******************************************************************************
** $Id: wordlist.h,v 2.11 1996/02/22 21:43:51 gerd Exp gerd $
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

#ifndef WordNULL

 typedef struct wORDlIST
 { char            *wl_word;
   struct wORDlIST *wl_next;
 } SWordList, *WordList;

#define ThisWord(X) ((X)->wl_word)
#define NextWord(X) ((X)->wl_next)
#define WordNULL    ((WordList)0)

#ifdef __STDC__
#define _ARG(A) A
#else
#define _ARG(A) ()
#endif
 int find_word _ARG((char *s,WordList wl));	   /* wordlist.c             */
 void list_words _ARG((WordList wl));		   /* wordlist.c             */
 void save_word _ARG((char *s,WordList *wlp));	   /* wordlist.c             */

#endif
