/* $Id: rt-process.h,v 3.0 1992/12/14 00:14:15 davison Trn $
*/
/* The authors make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

int msgid_cmp _((char*,int,HASHDATUM));
ARTICLE *allocate_article _((ART_NUM));
bool valid_article _((ARTICLE*));
ARTICLE *get_article _((char*));
void thread_article _((ARTICLE*));
void merge_threads _((SUBJECT*, SUBJECT*));
