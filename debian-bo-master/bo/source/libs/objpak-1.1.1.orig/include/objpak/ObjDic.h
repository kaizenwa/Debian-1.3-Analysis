
/*
 * ObjectPak Objective C Class Library
 */

#ifndef __OBJDIC_H__
#define __OBJDIC_H__

#include "ObjPak.h"

@interface ObjDic : ObjPak
{
    id		associations;
}

+ new;
- copy;
- deepCopy;
- emptyYourself;
- freeContents;
- free;

- (unsigned) size;
- (BOOL) isEmpty;
- (BOOL) includesKey:aKey;

- (unsigned) hash;
- (BOOL) isEqual:aDic;

- atKey:aKey;
- atKeySTR:(STR)strKey;
- atKey:aKey put:anObject;
- atKeySTR:(STR)strKey put:anObject;
- eachKey;
- eachValue;

- printToFile:(FILE *)aFile;

#ifdef __NeXT__
- write:(NXTypedStream *)stream;
- read:(NXTypedStream *)stream;
#endif /* __NeXT__ */

@end

#endif /* __OBJDIC_H__ */

