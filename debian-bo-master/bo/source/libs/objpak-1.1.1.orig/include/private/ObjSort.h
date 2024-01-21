
/*
 * ObjectPak Objective C Class Library
 */

#ifndef __OBJSORT_H__
#define __OBJSORT_H__

#include "ObjPak.h"

typedef struct objbbt {
    struct objbbt*	ulink;
    struct objbbt*	rlink;
    struct objbbt*	llink;
    int			balance;
    id			key;
} *objbbt_t;

@interface ObjSort : ObjPak
{
    struct objbbt	value;
    SEL			cmpSel;
}

+ new;
+ newDictCompare;
- setUpCmpSel:(SEL)aSel;
+ newCmpSel:(SEL)aSel;
- copy;
- deepCopy;
- emptyYourself;
- freeContents;
- free;

- (objbbt_t) objbbtTop;
- (SEL) comparisonSelector;
- (unsigned) size;
- (BOOL) isEmpty;
- eachElement;

- (unsigned) hash;
- (BOOL) isEqual:aSort;

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

- printToFile:(FILE *)aFile;

#ifdef __NeXT__

- write:(NXTypedStream *)stream;
- read:(NXTypedStream *)stream;
#endif /* __NeXT__ */

@end

#endif /* __OBJSORT_H__ */

