/* 
 * $Source: /tmp_mnt/n/fs/grad1/jsp/src/jgraph/RCS/list.h,v $
 * $Revision: 8.3 $
 * $Date: 92/11/30 11:42:27 $
 * $Author: jsp $
 */

/* This is the header file for the list manipulation routines in list.c.
 * Any struct can be turned into a list as long as its first two fields are
 * flink and blink. */

typedef struct list {
  struct list *flink;
  struct list *blink;
} *List;

/* Nil, first, next, and prev are macro expansions for list traversal 
 * primitives. */

#define nil(l) (l)
#define first(l) (l->flink)
#define last(l) (l->blink)
#define next(n) (n->flink)
#define prev(n) (n->blink)

/* These are the routines for manipluating lists */

/* void insert(node list);     Inserts a node to the end of a list */
/* void delete_item(node);     Deletes an arbitrary node */
/* List make_list(node_size);  Creates a new list */
/* List get_node(list);        Allocates a node to be inserted into the list */
/* void free_node(node, list); Deallocates a node from the list */

