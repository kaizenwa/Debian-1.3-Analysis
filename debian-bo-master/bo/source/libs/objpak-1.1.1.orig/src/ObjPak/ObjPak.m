
/*
 * ObjectPak Objective C Class Library
 */

#include <objpak.h>

@implementation ObjPak
/*****************************************************************************
 *
 * Version
 *
 ****************************************************************************/

+ (STR) objpakRevision
{
    return __objpak_revision__;
}

/*****************************************************************************
 *
 * Copying
 *
 ****************************************************************************/


- deepCopy
{
    return [super copy];
}

/*****************************************************************************
 *
 * Comparing
 *
 ****************************************************************************/


- (BOOL) isEqual:anObject
{
    [self notImplemented:_cmd];return NO;
}

- (BOOL) notEqual:anObject
{
    return [self isEqual:anObject] == NO;
}

- (int) compare:anObject
{
    [self notImplemented:_cmd];return 0;
}

/*****************************************************************************
 *
 * Printing
 *
 ****************************************************************************/

- printToFile:(FILE *)aFile
{
    return [self subclassResponsibility:_cmd];
}


#ifdef __STPSTN__
- printOn:(IOD)anIOD
{
    return [self printToFile:anIOD];
}
#endif /* __STPSTN__ */

#ifdef __NeXT__
- printToStream:(NXStream *)aStream
{
    FILE *aFile = tmpfile();[self printToFile:aFile];
    rewind(aFile);while (!feof(aFile)) { NXPutc(aStream,fgetc(aFile)); }
    fclose(aFile);return self;
}
- (void) printForDebugger:(NXStream *)aStream
{
    [self printToStream:aStream];NXPrintf(aStream," (%s)",[self name]);
}
#endif /* __NeXT__ */

@end

