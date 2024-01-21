
/*
 * ObjectPak Objective C Class Library
 */

#ifndef __OBJPAK_H__
#define __OBJPAK_H__

#include <stdio.h>

#ifdef __NeXT__
#include <objc/Object.h>
#endif
#ifdef __STPSTN__
#include <Object.h>
#endif
#ifdef __GNUOBJC__
#include <objc/Object.h>
#endif

@interface ObjPak : Object
{
}

+ (STR) objpakRevision;

- deepCopy;

- (BOOL) isEqual:anObject;
- (BOOL) notEqual:anObject;
- (int) compare:anObject;

- printToFile:(FILE *)aFile;

#ifdef __STPSTN__
- printOn:(IOD)anIOD;
#endif /* __STPSTN__ */

#ifdef __NeXT__
- printToStream:(NXStream *)aStream;
- (void) printForDebugger:(NXStream *)aStream;
#endif /* __NeXT__ */

@end

#endif /* __OBJPAK_H__ */

