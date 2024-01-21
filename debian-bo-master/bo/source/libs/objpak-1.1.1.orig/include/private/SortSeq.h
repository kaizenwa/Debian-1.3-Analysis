
/*
 * ObjectPak Objective C Class Library
 */

#ifndef __OBJSORTSEQ_H__
#define __OBJSORTSEQ_H__

#include "ObjPak.h"

@interface ObjSortSeq : ObjPak
{
    objbbt_t	top;
    objbbt_t	next;
    objbbt_t	prev;
}
- setUpSort:aSort;
+ over:aSort;
- copy;
- free;
- (unsigned) size;
- next;
- peek;
- previous;
- first;
- last;

@end

#endif /* __OBJSORTSEQ_H__ */

