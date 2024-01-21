
/*
 * ObjectPak Objective C Class Library
 */

#ifndef __OBJKEYSEQ_H__
#define __OBJKEYSEQ_H__

#include "ObjPak.h"

@interface ObjKeySeq : ObjPak
{
    id	carrier;
}

- setUpCarrier:aCarrier;
+ over:aCarrier;
- copy;
- free;

- (unsigned) size;

- next;
- peek;
- previous;
- first;
- last;

@end

#endif /* __OBJKEYSEQ_H__ */

