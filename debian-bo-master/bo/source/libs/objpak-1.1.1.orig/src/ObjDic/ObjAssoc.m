
/*
 * ObjectPak Objective C Class Library
 */

#include <objpak.h>

@implementation ObjAssoc

/*****************************************************************************
 *
 * Creation
 *
 ****************************************************************************/

- setUpKey:aKey value:aValue
{
    key = aKey;
    value = aValue;
    return self;
}

+ key:aKey
{
    return [self key:aKey value:nil];
}

+ key:aKey value:aValue
{
    self = [super new];[self setUpKey:aKey value:aValue];return self;
}

/*****************************************************************************
 *
 * Interrogation
 *
 ****************************************************************************/

- key
{
    return key;
}

- (STR) str
{ 
    return [key str]; 
}

- value
{
    return value;
}

/*****************************************************************************
 *
 * Comparison
 *
 ****************************************************************************/

- (unsigned) hash
{
    return [key hash];
}

- (BOOL) isEqual:anAssoc
{
    return (self == anAssoc)?YES:[key isEqual:[anAssoc key]];
}

- (int) compare:anAssoc
{
    return [key compare:[anAssoc key]];
}

/*****************************************************************************
 *
 * Assignment
 *
 ****************************************************************************/

- value:aValue
{
    id tmp = value; value = aValue; return tmp; 
}

/*****************************************************************************
 *
 * Printing
 *
 ****************************************************************************/

- printToFile:(FILE *)aFile
{
    [key printToFile:aFile];fprintf(aFile,"\t");[value printToFile:aFile];
    return self;
}

/******************************************************************************
 *
 * NextStep Read & Write
 *
 *****************************************************************************/

#ifdef __NeXT__
- write:(NXTypedStream *)stream
{
    [super write:stream];
    NXWriteObject(stream,key);
    NXWriteObject(stream,value);
    return self;
}

- read:(NXTypedStream *)stream
{
    [super read:stream];
    key = NXReadObject(stream);
    value = NXReadObject(stream);
    return self;
}

#endif /* __NeXT__ */

@end

