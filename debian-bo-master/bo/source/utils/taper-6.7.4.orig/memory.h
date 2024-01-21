/*
   Time-stamp: <96/07/19 20:17:50 yusuf>

   $Id: memory.h,v 1.6 1996/07/27 20:42:10 yusuf Exp $
*/



#include <stdlib.h>
#include <string.h>

void init_memory(void);
void *my_malloc(size_t size);
void *my_realloc(void *block, size_t size);
void my_free(void *block);
void my_free_all(void);
						 /* have it, you'll have to do it yourself */
