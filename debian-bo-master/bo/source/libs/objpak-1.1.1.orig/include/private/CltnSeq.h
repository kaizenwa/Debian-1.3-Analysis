
/*
 * ObjectPak Objective C Class Library
 */

#ifndef __OBJCOLSEQ_H__
#define __OBJCOLSEQ_H__

#include "ObjPak.h"

@interface ObjCltnSeq : ObjPak
{
    id		collection;
    unsigned	offset;
}
- setUpCollection:aCol;
+ over:aCol;
- copy;
- free;
- (unsigned) size;
- next;
- peek;
- previous;
- first;
- last;

@end

#endif /* __OBJCOLSEQ_H__ */

