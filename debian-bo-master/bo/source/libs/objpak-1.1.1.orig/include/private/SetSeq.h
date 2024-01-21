
/*
 * ObjectPak Objective C Class Library
 */

#ifndef __OBJSETSEQ_H__
#define __OBJSETSEQ_H__

#include "ObjPak.h"

typedef struct objsetseq {
    objset_t	set;
    int		offset;
} *objsetseq_t;

@interface ObjSetSeq : ObjPak
{
    struct objsetseq	value;
}
- (objsetseq_t) objsetseqValue;
+ over:aSet;
- copy;
- free;
- (unsigned) size;
- next;
- peek;
- previous;
- first;
- last;

@end

#endif /* __OBJSETSEQ_H__ */

