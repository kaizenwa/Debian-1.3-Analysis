/*
   Time-stamp: <96/07/19 20:17:46 yusuf>

   $Id: memory.c,v 1.6 1996/07/27 20:42:10 yusuf Exp $	

*/

#ifndef lint
static char vcid[] = "$Id: memory.c,v 1.6 1996/07/27 20:42:10 yusuf Exp $";
#endif /* lint */



/* My memory routines. Maintains a list of currently allocated
 * blocks and provides a way to delete them all in one go. Can
 * only alloc a maximum of MAX_ALLOCED blocks
*/ 


#include "memory.h"

#define MAX_ALLOCED 50


void *alloced[MAX_ALLOCED];

void init_memory() {
    int c;
    
    for (c=0; c<MAX_ALLOCED; c++)
      alloced[c] = NULL;
}

    
void *my_malloc(size_t size) {
    int c;

    for (c=0; c<MAX_ALLOCED; c++)		 /* look for unused block */
      if (alloced[c] == NULL)
        break;
    if (c == MAX_ALLOCED)			 /* too many blocks */
      return NULL;				 /* allocated - return error */

    alloced[c] = malloc(size);
    memset(alloced[c], 0, size);
    return alloced[c];
}

void *my_realloc(void *block, size_t size) {
    int c;
    
    for (c=0; c<MAX_ALLOCED; c++)		 /* look for block */
      if (alloced[c] == block)
        break;
    if (c == MAX_ALLOCED)			 /* couldn't find this block */
      return NULL;				 /* in my list */
    
    alloced[c] = realloc(alloced[c], size);
    return alloced[c];
}

void my_free(void *block) {
    int c;
    
    for (c=0; c<MAX_ALLOCED; c++)		 /* look for block */
      if (alloced[c] == block)
        break;
    if (c < MAX_ALLOCED) {
    	free(block);				 /* free block */
    	alloced[c] = NULL;			 /* remove from my list */
    }
    else exit(-10);
}

void my_free_all() {
    int c;
    
    for (c=0; c<MAX_ALLOCED; c++)
      if (alloced[c])
        my_free(alloced[c]);
}


