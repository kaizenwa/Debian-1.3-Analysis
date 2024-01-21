
/*
 * ObjectPak Objective C Class Library
 */

#include <objpak.h>

@implementation ObjDic

/*****************************************************************************
 *
 * Creation
 *
 ****************************************************************************/


static void setUp(id *associations)
{
    *associations = [ObjSet new];
}

+ new
{
    self = [super new];setUp([self associationsRef]);return self;
}

- copy
{
    self = [super copy];
    associations = [associations copy];
    return self;
}

- deepCopy
{
    self = [super deepCopy];
    associations = [associations deepCopy];
    return self;
}

- emptyYourself
{
    [associations emptyYourself];
    return self;
}

- freeContents
{
    [associations freeContents];
    return self;
}

- free
{
    associations = [associations free];return [super free];
}

/*****************************************************************************
 *
 * Interrogation
 *
 ****************************************************************************/


- associations
{
    return associations;
}

- (id *) associationsRef
{
    return &associations;
}

- (unsigned) size
{
    return [associations size];
}

- (BOOL) isEmpty
{
    return [associations isEmpty];
}

- (BOOL) includesKey:aKey
{
    return (BOOL) ([self associationAt: aKey] != nil); 
}

/*****************************************************************************
 *
 * Comparing
 *
 ****************************************************************************/


- (unsigned) hash
{
    return [associations hash];
}

- (BOOL) isEqual:aDic
{
    return [associations isEqual:[aDic associations]];
}

/*****************************************************************************
 *
 * Indexed Access
 *
 ****************************************************************************/


- associationAt:aKey
{
    id result,association;
    association = [ObjAssoc key:aKey];
    result = [associations find:association];
    association = [association free];return result;
}

- atKey:aKey
{
    id association = [self associationAt:aKey];
    return (association)?[association value]:nil;
}

- atKeySTR:(STR)strKey
{
    return [self atKey:[ObjStr str:strKey]];
}

- atKey:aKey put:anObject
{
    return [[associations filter:[ObjAssoc key:aKey]] value:anObject];
}

- atKeySTR:(STR)strKey put:anObject
{
    return [self atKey:[ObjStr str:strKey] put:anObject];
}

- eachKey
{
    id aCarrier = [ObjKeySeq over:[associations eachElement]];
    return [ObjSeq over:aCarrier];
}
- eachValue
{
    id aCarrier = [ObjValSeq over:[associations eachElement]];
    return [ObjSeq over:aCarrier];
}
/*****************************************************************************
 *
 * Printing
 *
 ****************************************************************************/

- printToFile:(FILE *)aFile
{
    [[[associations eachElement] printToFile:aFile] free];return self;
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
    NXWriteObject(stream,associations);
    return self;
}

- read:(NXTypedStream *)stream
{
    [super read:stream];
    associations = NXReadObject(stream);
    return self;
}

#endif /* __NeXT__ */

@end

