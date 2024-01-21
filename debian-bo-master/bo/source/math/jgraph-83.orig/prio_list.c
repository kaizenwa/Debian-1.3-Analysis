/* 
 * $Source: /tmp_mnt/n/fs/grad1/jsp/src/jgraph/RCS/prio_list.c,v $
 * $Revision: 8.3 $
 * $Date: 92/11/30 11:42:32 $
 * $Author: jsp $
 */

#include "list.h"
#include "prio_list.h"
#include <stdio.h>

typedef int Boolean;

/* A prioirity list is any list with the first three fields being flink, 
 * blink and prio.  Use the routines of list.c to do everything except
 * insertion */
 
typedef struct prio_list {
  struct prio_list *flink;
  struct prio_list *blink;
  int prio;
} *Prio_list;

/* Prio_insert inserts nodes into their proper places in priority lists.  It first 
 * checks for inserting into the head or tail, and then proceeds sequentially.
 * Thus, it is worst case linear, but for most cases constant time (right). */

prio_insert(node, list, desc)
Prio_list node;
Prio_list list;
Boolean desc;
{
  Prio_list p;

  /* Check nil and head of list */
  if (first(list) == nil(list) || 
      (!desc && first(list)->prio >= node->prio) ||
      (desc  && first(list)->prio <= node->prio) ) {
    node->blink = list;
    node->flink = list->flink;
    list->flink->blink = node;
    list->flink = node;
    return;
  }
  /* Check tail of list */
  if ((desc  && last(list)->prio >= node->prio) ||
      (!desc && last(list)->prio <= node->prio) ) {
    node->flink = list;
    node->blink = list->blink;
    list->blink->flink = node;
    list->blink = node;
    return;
  }
  /* Check the rest of the list sequentially */
  for(p = next(first(list));  ; p = next(p)) {
    if (p == nil(list)) fprintf(stderr, "inserting into tail did not work\n");
    if ((!desc && p->prio >= node->prio) ||
        (desc  && p->prio <= node->prio)) {
      node->flink = p;
      node->blink = p->blink;
      p->blink->flink = node;
      p->blink = node;
      return;
    }
  }
}
      

  
