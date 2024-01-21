
/*
 * ObjectPak Objective C Class Library
 */

#include <objpak.h>

@implementation ObjKeySeq
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
    return [[carrier next] key];
}

- peek
{
    return [[carrier peek] key];
}

- previous
{
    return [[carrier previous] key];
}

- first
{
    return [[carrier first] key];
}

- last
{
    return [[carrier last] key];
}

@end

