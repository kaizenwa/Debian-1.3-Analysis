/* 
 * $Source: /tmp_mnt/n/fs/grad1/jsp/src/jgraph/RCS/prio_list.h,v $
 * $Revision: 8.3 $
 * $Date: 92/11/30 11:42:34 $
 * $Author: jsp $
 */

/* Priority lists are just like normal lists of list.h and list.c, except
 * that their third field is a (int) prioriy.  The routines of list.c should all
 * be used except for insert, because it will always put an item at the 
 * end of a list.  Instead, use prio_insert, which will put the item
 * into its proper place in the list.  The last argument of prio_insert should
 * be TRUE if the list is to be kept in descending order; it should be FALSE
 * for ascending order.

 * Priority list should be:

struct prio_list {
  struct prio_list *flink;
  struct prio_list *blink;
  int prio;
  ...
  }

*/

/* void prio_insert(node, list, descending); */

