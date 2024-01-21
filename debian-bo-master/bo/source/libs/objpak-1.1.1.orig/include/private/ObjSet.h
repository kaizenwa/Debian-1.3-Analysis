
/*
 * ObjectPak Objective C Class Library
 */

#ifndef __OBJSET_H__
#define __OBJSET_H__

#include "ObjPak.h"

typedef struct objset {
    int		count;
    int		capacity;
    id*		ptr;
} *objset_t;

@interface ObjSet : ObjPak
{
    struct objset	value;
}

+ new;
- copy;
- deepCopy;
- emptyYourself;
- freeContents;
- free;

- (objset_t) objsetValue;
- (unsigned) size;
- (BOOL) isEmpty;
- eachElement;

- (unsigned) hash;
- (BOOL) isEqual:aSet;

- add:anObject;
- addNTest:anObject;
- filter:anObject;
- replace:anObject;

- remove:oldObject;

- addContentsTo:aCol;
- addContentsOf:aCol;
- removeContentsOf:aCol;
- removeContentsFrom:aCol;

- find:anObject;
- (BOOL) contains:anObject;
- (unsigned) occurrencesOf:anObject;

- intersection:aSet;
- union:aSet;
- difference:aSet;

- printToFile:(FILE *)aFile;

#ifdef __NeXT__
- write:(NXTypedStream *)stream;
- read:(NXTypedStream *)stream;
#endif /* __NeXT__ */

@end

#endif /* __OBJSET_H__ */

