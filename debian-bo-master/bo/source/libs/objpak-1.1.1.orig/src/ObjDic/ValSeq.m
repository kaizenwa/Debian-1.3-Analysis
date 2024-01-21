
/*
 * ObjectPak Objective C Class Library
 */

#include <objpak.h>

@implementation ObjValSeq
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
    return [[carrier next] value];
}

- peek
{
    return [[carrier peek] value];
}

- previous
{
    return [[carrier previous] value];
}

- first
{
    return [[carrier first] value];
}

- last
{
    return [[carrier last] value];
}

@end

