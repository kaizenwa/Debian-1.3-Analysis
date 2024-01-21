
/*
 * ObjectPak Objective C Class Library
 */

#ifndef __OBJVALSEQ_H__
#define __OBJVALSEQ_H__

#include "ObjPak.h"

@interface ObjValSeq : ObjPak
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

#endif /* __OBJVALSEQ_H__ */

