/* $Id: rthread.h,v 3.0 1992/12/14 00:14:15 davison Trn $
*/
/* The authors make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

EXT ART_NUM article_count INIT(0);
EXT int subject_count INIT(0);
EXT bool output_chase_phrase;
EXT char *references;
EXT bool ov_opened INIT(FALSE);

void thread_init _((void));
void thread_open _((void));
void thread_grow _((void));
void thread_close _((void));

void top_article _((void));
ARTICLE *first_art _((SUBJECT*));
ARTICLE *last_art _((SUBJECT*));
ARTICLE *bump_art _((ARTICLE*));
ARTICLE *next_art _((ARTICLE*));
ARTICLE *prev_art _((ARTICLE*));
void inc_art _((bool_int,bool_int));
void dec_art _((bool_int,bool_int));
bool next_art_with_subj _((void));
bool prev_art_with_subj _((void));
SUBJECT *next_subj _((SUBJECT*,int));
SUBJECT *prev_subj _((SUBJECT*,int));

void select_article _((ARTICLE*,int));
void select_arts_subject _((ARTICLE*,int));
void select_subject _((SUBJECT*,int));
void select_arts_thread _((ARTICLE*,int));
void select_thread _((ARTICLE*,int));
void select_subthread _((ARTICLE*,int));
void deselect_article _((ARTICLE*));
void deselect_arts_subject _((ARTICLE*));
void deselect_subject _((SUBJECT*));
void deselect_arts_thread _((ARTICLE*));
void deselect_thread _((ARTICLE*));
void deselect_all _((void));
void kill_arts_subject _((ARTICLE*,int));
void kill_subject _((SUBJECT*,int));
void kill_arts_thread _((ARTICLE*,int));
void kill_thread _((ARTICLE*,int));
void kill_subthread _((ARTICLE*,int));
void unkill_subject _((SUBJECT*));
void unkill_thread _((ARTICLE*));
void unkill_subthread _((ARTICLE*));
void clear_subject _((SUBJECT*));
void clear_thread _((ARTICLE*));
void clear_subthread _((ARTICLE*));

#define KF_UNSELECTED	0	/* leave selected articles, no KILL file */
#define KF_ALL		0x0001	/* Affect both selected and unselected */
#define KF_KILLFILE	0x0002	/* Save the command to the KILL file */

ARTICLE *subj_art _((SUBJECT*));
void visit_next_thread _((void));
void visit_prev_thread _((void));

bool find_parent _((bool_int));
bool find_leaf _((bool_int));
bool find_prev_sib _((void));
bool find_next_sib _((void));

void sort_subjects _((void));
void count_subjects _((int));
#define CS_RETAIN      0
#define CS_NORM        1
#define CS_RESELECT    2
#define CS_UNSELECT    3
#define CS_UNSEL_STORE 4

int subjorder_date _((SUBJECT**, SUBJECT**));
int subjorder_str _((SUBJECT**, SUBJECT**));
int threadorder_date _((SUBJECT**, SUBJECT**));
int threadorder_str _((SUBJECT**, SUBJECT**));

void sort_articles _((void));

int artorder_date _((ARTICLE**, ARTICLE**));
int artorder_str _((ARTICLE**, ARTICLE**));

time_t parsedate _((char*));

/* Stuff local to rthread.c. */

#ifdef DOINIT

static void build_artptrs _((void));

#endif
