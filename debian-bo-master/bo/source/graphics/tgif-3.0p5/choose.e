/*
 * Author:      William Chia-Wei Cheng (william@cs.ucla.edu)
 *
 * Copyright (C) 1990-1996, William Chia-Wei Cheng.
 *
 * Permission limited to the use, copy, display, distribute without
 * charging for a fee, and produce derivative works of "tgif" and
 * its documentation for not-for-profit purpose is hereby granted by
 * the Author, provided that the above copyright notice appears in
 * all copies made of "tgif" and that both the copyright notice
 * and this permission notice appear in supporting documentation,
 * and that the name of the Author not be used in advertising or
 * publicity pertaining to distribution of the software without
 * specific, written prior permission.  The Author makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied
 * warranty.  All other rights (including, but not limited to, the
 * right to sell "tgif", the right to sell derivative works of
 * "tgif", and the right to distribute "tgif" for a fee) are
 * reserved by the Author.
 *
 * THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, INDIRECT
 * OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
 * NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/choose.e,v 3.0 1996/05/06 16:04:07 william Exp $
 */

#ifndef _CHOOSE_E_
#define _CHOOSE_E_

#define NAMES_SIMPLE_SELECT_NAME 0
#define NAMES_COMPLEX_SELECT_NAME 1
#define NAMES_SELECT_FILE 2
#define NAMES_EDIT_ATTR 3
#define NAMES_EDIT_NAME 4

#define NAMES_LOOP_MANY 0
#define NAMES_LOOP_ONCE 1

typedef int (GetEntriesFunc)ARGS_DECL((DspList**, char***, int *pn_num_entries,
      int *pn_marked_index, char *inbuf, void*));
typedef int (AfterLoopFunc)ARGS_DECL((DspList**, char***, int *pn_num_entries,
      int *pn_marked_index, char *cur_buf, int btn_id, int selected_index,
      void*));

extern void	NamesSetTitle ARGS_DECL((char*));
extern void	ResetNamesInfo ARGS_DECL((void));
extern void	InitNamesInfo ARGS_DECL((void));
extern void	CleanUpNamesInfo ARGS_DECL((void));
extern void	NamesAddButton ARGS_DECL((char*, int));

extern void	NamesSetDefaultBtnId ARGS_DECL((int def_btn_id,
		                                int double_click_btn_id));
extern void	NamesSetStyle ARGS_DECL((int edit_style, int loop_once));
extern void	NamesSetEntries ARGS_DECL((DspList*, char**, int num_entries,
		                           int dont_free_entries,
		                           int marked_index, int leading));
extern void	NamesSetCallback ARGS_DECL((GetEntriesFunc *pf_before_loop,
		                            AfterLoopFunc *pf_after_loop));
extern void	NamesSetDir ARGS_DECL((char*));
extern int	Names ARGS_DECL((char *win_name, int *pn_selected_index,
		                 char *selected_str, int str_sz, void*));

#endif /*_CHOOSE_E_*/
