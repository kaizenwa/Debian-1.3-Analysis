
/*
 * ObjectPak Objective C Class Library
 */

#ifndef __OBJCOL_H__
#define __OBJCOL_H__

#include "ObjPak.h"

typedef struct objcol {
    int		count;
    int		capacity;
    id*		ptr;
} *objcol_t;

@interface ObjCltn : ObjPak
{
    struct objcol	value;
}

+ new;
- copy;
- deepCopy;
- emptyYourself;
- freeContents;
- free;

- (unsigned) size;
- (BOOL) isEmpty;
- (unsigned) lastOffset;
- eachElement;
- firstElement;
- lastElement;

- (unsigned) hash;
- (BOOL) isEqual:aCltn;

- add:anObject;
- addFirst:newObject;
- addLast:newObject;
- addIfAbsent:anObject;
- addIfAbsentMatching:anObject;

- at:(unsigned )anOffset insert:anObject;
- insert:newObject after:oldObject;
- insert:newObject before:oldObject;

- after:anObject;
- before:anObject;
- at:(unsigned )anOffset;
- at:(unsigned )anOffset put:anObject;

- removeFirst;
- removeLast;
- removeAt:(unsigned )anOffset;
- remove:oldObject;

- addContentsTo:aCol;
- addContentsOf:aCol;
- removeContentsOf:aCol;
- removeContentsFrom:aCol;

- find:anObject;
- findMatching:anObject;
- findSTR:(STR )strValue;
- (BOOL) contains:anObject;
- (unsigned) offsetOf:anObject;

- printToFile:(FILE *)aFile;

#ifdef __NeXT__
- write:(NXTypedStream *)stream;
- read:(NXTypedStream *)stream;
#endif /* __NeXT__ */

@end

#endif /* __OBJCOL_H__ */

