
/*
 * ObjectPak Objective C Class Library
 */

#ifndef __OBJSEQ_H__
#define __OBJSEQ_H__

#include "ObjPak.h"

@interface ObjSeq : ObjPak
{
    id		carrier;
}

- copy;
- free;

- (unsigned) size;

- next;
- peek;
- previous;
- first;
- last;

- printToFile:(FILE *)aFile;

@end

#endif /* __OBJSEQ_H__ */

