
/*
 * ObjectPak Objective C Class Library
 */

#ifndef __OBJSTR_H__
#define __OBJSTR_H__

#include "ObjPak.h"

typedef struct objstr {
    int		count;
    int		capacity;
    char*	ptr;
} *objstr_t;

@interface ObjStr : ObjPak
{
    struct objstr	value;
}

+ new;
+ str:(STR)strValue;
+ sprintf:(STR)format,...;
- copy;
- deepCopy;
- free;

- (int) compare:aStr;
- (int) compareSTR:(STR)strValue;
- (unsigned) hash;
- (int) dictCompare:aStr;
- (BOOL) isEqual:aStr;
- (BOOL) isEqualSTR:(STR)strValue;

- (objstr_t) objstrValue;
- (unsigned) size;
- (char) charAt:(unsigned)anOffset;
- (char) charAt:(unsigned)anOffset put:(char)aChar;

- (STR) strcat:(STR)aBuffer;
- concatSTR:(STR)strValue;
- assignSTR:(STR)strValue;
- assignSTR:(STR)strValue length:(unsigned)nChars;

- (double) asDouble;
- (int) asInt;
- (long) asLong;
- asSTR:(STR)aBuffer maxSize:(int)aSize;
- (STR) str;
- (STR) strCopy;

- toLower;
- toUpper;

- printToFile:(FILE *)aFile;

#ifdef __NeXT__
- write:(NXTypedStream *)stream;
- read:(NXTypedStream *)stream;
#endif /* __NeXT__ */

@end

#endif /* __OBJSTR_H__ */

