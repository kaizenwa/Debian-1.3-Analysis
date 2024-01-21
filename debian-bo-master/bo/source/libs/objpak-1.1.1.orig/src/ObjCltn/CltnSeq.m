
/*
 * ObjectPak Objective C Class Library
 */

#include <objpak.h>

@implementation ObjCltnSeq

- setUpCollection:aCol
{
    collection = aCol;offset = 0;return self;
}

+ over:aCol
{
    self = [super new];[self setUpCollection:aCol];return self;
}

- copy
{
    return [super copy];
}

- free
{
    return [super free];
}

- (unsigned) size
{
    return [collection size];
}

- next
{
    return (offset < [collection size])?[collection at:offset++]:nil;
}

- peek
{
    return (offset < [collection size])?[collection at:offset+1]:nil;
}

- previous
{
    assert(offset <= [collection size]);
    return (0 < offset)?[collection at:offset-1]:nil;
}

- first
{
    return [collection firstElement];
}

- last
{
    return [collection lastElement];
}

@end

