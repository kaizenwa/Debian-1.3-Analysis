/* $Id: rt-page.h,v 3.0 1992/12/14 00:14:12 davison Trn $
*/
/* The authors make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

#define PRESERVE_PAGE     0
#define FILL_LAST_PAGE    1

bool set_sel_mode _((char_int));
bool set_sel_sort _((char_int));
void set_selector _((int,int));
void init_pages _((bool_int));
bool first_page _((void));
bool last_page _((void));
bool next_page _((void));
bool prev_page _((void));
void display_page _((void));
void update_page _((void));
void output_sel _((int));

/* Stuff internal to rt-select.c */

#ifdef DOINIT

static int count_subject_lines _((SUBJECT*, int*));
static int count_thread_lines _((SUBJECT*, int*));
static void display_article _((ARTICLE*, char_int, int));
static void display_subject _((SUBJECT*, char_int, int));

#endif
