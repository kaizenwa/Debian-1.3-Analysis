
/*
 * ObjectPak Objective C Class Library
 */

#include <objpak.h>

@implementation ObjSeq
/*****************************************************************************
 *
 * Creation
 *
 ****************************************************************************/

- setUpCarrier:aCarrier
{
    carrier = aCarrier;return self;
}

+ over:aCarrier
{
    self = [super new];[self setUpCarrier:aCarrier];return self;
}

- copy
{
    return [isa over:[carrier copy]];
}

- free
{
    carrier = [carrier free];return [super free];
}

/*****************************************************************************
 *
 * Interrogation
 *
 ****************************************************************************/

- (unsigned) size
{
    return [carrier size];
}

/*****************************************************************************
 *
 * Accessing
 *
 ****************************************************************************/

- next
{
    return [carrier next];
}

- peek
{
    return [carrier peek];
}

- previous
{
    return [carrier previous];
}

- first
{
    return [carrier first];
}

- last
{
    return [carrier last];
}

/*****************************************************************************
 *
 * Printing
 *
 ****************************************************************************/

- printToFile:(FILE *)aFile
{
    id aMember;
    while ((aMember = [self next])) {
	[aMember printToFile:aFile];fprintf(aFile,"\n");
    }
    return self;
}

@end

