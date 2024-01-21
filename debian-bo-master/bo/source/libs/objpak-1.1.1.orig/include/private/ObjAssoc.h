
/*
 * ObjectPak Objective C Class Library
 */

#ifndef __OBJASSOC_H__
#define __OBJASSOC_H__

#include "ObjPak.h"

@interface ObjAssoc : ObjPak
{
    id	key;
    id	value;
}

- setUpKey:aKey value:aValue;
+ key:aKey;
+ key:aKey value:aValue;

- key;
- (STR) str;
- value;

- (unsigned) hash;
- (BOOL) isEqual:anAssoc;
- (int) compare:anAssoc;

- value:aValue;

- printToFile:(FILE *)aFile;

#ifdef __NeXT__
- write:(NXTypedStream *)stream;
- read:(NXTypedStream *)stream;
#endif /* __NeXT__ */

@end

#endif /* __OBJASSOC_H__ */


