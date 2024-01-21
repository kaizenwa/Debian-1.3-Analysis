
/*
 * ObjectPak Objective C Class Library
 */

#include <objpak.h>

@implementation ObjSet

/*****************************************************************************
 *
 * Creation
 *
 ****************************************************************************/

static void objptr_zero(id *p,int c)
{
    while (c--) *p++ = nil;
}

static int objptr_count(id *p,int c)
{
    int n = 0;
    while (c--) if (*p++) n++;
    return n;
}

static void objset_init(objset_t self,int n,int c)
{
    assert(0 <= n && n <= c);
    self->count    = n;
    self->capacity = c;
    self->ptr      = (id *)malloc(c * sizeof(id));
}

static void objset_initzero(objset_t self,int c)
{
    objset_init(self,0,16);
    objptr_zero(self->ptr,c);
}

+ new
{
    self = [super new];objset_initzero([self objsetValue],16);return self;
}

static void objptr_copy(id *p,id *q,int c)
{
    while (c--) *p++ = *q++;
}

static void objset_copy(objset_t dst,objset_t src)
{
    objset_init(dst,src->count,src->capacity);
    objptr_copy(dst->ptr,src->ptr,src->capacity);
}

- copy
{
    id aCopy = [super copy];
    objset_copy([aCopy objsetValue],[self objsetValue]);
    return aCopy;
}

static void objptr_deepcopy(id *p,id *q,int c)
{
    while (c--) { id obj = *q++;*p++ = (obj)?[obj deepCopy]:nil; }
}

static void objset_deepcopy(objset_t dst,objset_t src)
{
    objset_init(dst,src->count,src->capacity);
    objptr_deepcopy(dst->ptr,src->ptr,src->capacity);
}

- deepCopy
{
    id aCopy = [super deepCopy];
    objset_deepcopy([aCopy objsetValue],[self objsetValue]);
    return aCopy;
}

static void objset_empty(objset_t self)
{
    self->count = 0;objptr_zero(self->ptr,self->capacity);
}

- emptyYourself
{
    objset_empty([self objsetValue]);return self;
}

static void objptr_clear(id *p,int c)
{
    while (c--) { id obj = *p;*p++ = (obj)?[obj free]:nil; }
}

static void objset_freecontents(objset_t self)
{
    self->count = 0;objptr_clear(self->ptr,self->capacity);
    assert(objptr_count(self->ptr,self->capacity) == 0);
}

- freeContents
{
    objset_freecontents([self objsetValue]);return self;
}

static void objset_clear(objset_t self)
{
    self->count = 0;self->capacity = 0;free(self->ptr);self->ptr = NULL;
}

- free
{
    objset_clear([self objsetValue]);return [super free];
}

/*****************************************************************************
 *
 * Interrogation
 *
 ****************************************************************************/

- (objset_t) objsetValue
{
    return &value;
}

static int objset_count(objset_t self)
{
    assert(objptr_count(self->ptr,self->capacity) == self->count);
    return self->count;
}

- (unsigned) size
{
    return (unsigned)objset_count([self objsetValue]);
}

- (BOOL) isEmpty
{
    return objset_count([self objsetValue]) == 0;
}

- eachElement
{
    id aCarrier = [ObjSetSeq over:self];
    return [ObjSeq over:aCarrier];
}

/*****************************************************************************
 *
 * Comparing
 *
 ****************************************************************************/

- (unsigned) hash
{
    [self notImplemented:_cmd]; return 0;
}

- (BOOL) isEqual:aSet
{
    [self notImplemented:_cmd]; return NO;
}

/*****************************************************************************
 *
 * Adding
 *
 ****************************************************************************/

static id *objptr_find(id *p,id obj,int n)
{
    id *begin = p;
    id *now   = p + ([obj hash] % n);
    id *end   = p + n;
    
    for(;n--;now++) {
	if (now >= end) now = begin;
	if (*now == nil || [*now isEqual:obj]) return now;
    }
    
    fprintf(stderr,"objset_find: table full shouldn't happen");return NULL;
}

static id *objset_find(objset_t self,id obj)
{
    assert(obj != nil);return objptr_find(self->ptr,obj,self->capacity);
}

- add:anObject
{
    [self addNTest:anObject];
    return self;
}

static BOOL objset_needsexpand(objset_t self)
{
    return 2 * self->count > self->capacity;
}

static void objptr_rehash(id *new,int newc,id *old,int oldc)
{
    while(oldc--) {
	id obj = *old++;
	id *newend = new + newc;
	if (obj) {
	    id *pos = new + ([obj hash] % ((unsigned)newc));
	    while (*pos) { pos++;if (pos == newend) pos = new; }
	    *pos = obj;
	}
    }
}

static void objset_rehash(objset_t self)
{
    int c;
    id *new,*old;
    
    c = self->capacity;old = self->ptr;new = (id *)malloc(c * sizeof(id));
    
    assert(objptr_count(old,c) == self->count);
    objptr_zero(new,c);objptr_rehash(new,c,old,c);
    assert(objptr_count(new,c) == self->count);
    
    free(old); self->ptr = new;
}

static void objset_expand(objset_t self)
{
    id *new,*old;
    int newc,oldc;
    
    oldc = self->capacity;old = self->ptr;
    newc = 1 + 2 * oldc; new = (id *)malloc(newc * sizeof(id));
    
    assert(objptr_count(old,oldc) == self->count);
    objptr_zero(new,newc);objptr_rehash(new,newc,old,oldc);
    assert(objptr_count(new,newc) == self->count);
    
    free(old); self->ptr = new; self->capacity = newc;
}

static id objset_add(objset_t self,id obj)
{
    id *p;
    
    if (objset_needsexpand(self)) objset_expand(self);
    
    if (*(p = objset_find(self,obj))) {
	return nil;
    } else {
	self->count++;return *p = obj;
    }
}

- addNTest:anObject
{
    if (anObject) {
	return objset_add([self objsetValue],anObject); 
    } else {
	return nil;
    }
}

static id objset_filter(objset_t self,id obj)
{
    id *p;
    
    if (objset_needsexpand(self)) objset_expand(self);
    
    if (*(p = objset_find(self,obj))) {
	obj = [obj free];return *p;
    } else {
	self->count++;return *p = obj;
    }
}

- filter:anObject
{
    if (anObject) {
	return objset_filter([self objsetValue],anObject); 
    } else {
	return nil;
    }
}

static id objset_replace(objset_t self,id obj)
{
    id *p;
    
    if (objset_needsexpand(self)) objset_expand(self);
    
    if (*(p = objset_find(self,obj))) {
	id tmp = *p;*p = obj;return tmp;
    } else {
	self->count++;*p = obj;return nil;
    }
}

- replace:anObject
{
    if (anObject) {
	return objset_replace([self objsetValue],anObject); 
    } else {
	return nil;
    }
}

/*****************************************************************************
 *
 * Removing
 *
 ****************************************************************************/

static id objset_remove(objset_t self,id obj)
{
    id *p;
    
    if (*(p = objset_find(self,obj))) {
	id tmp = *p;
	*p = nil;self->count--;
	objset_rehash(self);return tmp;
    } else {
	return nil;
    }
}

- remove:oldObject
{
    if (oldObject) {
	return objset_remove([self objsetValue],oldObject); 
    } else {
	return nil;
    }
}

/*****************************************************************************
 *
 * Adding and Removing Contents
 *
 ****************************************************************************/

- addContentsTo:aCol
{
    id elements;
    id anElement;
    
    elements = [self eachElement];
    while ((anElement = [elements next])) [aCol add:anElement];
    elements = [elements free];
    
    return aCol;
}

- addContentsOf:aCol
{
    id elements;
    id anElement;
    
    elements = [aCol eachElement];
    while ((anElement = [elements next])) [self add:anElement];
    elements = [elements free];
    
    return self;
}

- removeContentsOf:aCol
{
    if (self == aCol) {
	return [self emptyYourself];
    } else {
	id elements;
	id anElement;
	
	elements = [aCol eachElement];
	while ((anElement = [elements next])) [self remove:anElement];
	elements = [elements free];
	
	return self;
    }
}

- removeContentsFrom:aCol
{
    id elements;
    id anElement;
    
    elements = [self eachElement];
    while ((anElement = [elements next])) [aCol remove:anElement];
    elements = [elements free];
    
    return self;
}
/*****************************************************************************
 *
 * Locating
 *
 ****************************************************************************/

- find:anObject
{
    return (anObject)?*objset_find([self objsetValue],anObject):nil;
}

- (BOOL) contains:anObject
{
    return (BOOL)([self find:anObject] ? YES : NO); 
}

- (unsigned) occurrencesOf:anObject
{
    return (unsigned)([self find:anObject] ? 1 : 0); 
}

/*****************************************************************************
 *
 * Combining
 *
 ****************************************************************************/

- intersection:aSet
{
    if (self == aSet) {
	return [self copy];
    } else {
	id anElement,elements;
	id intersection = [isa new];
	
	elements = [self eachElement];
	while ((anElement = [elements next])) {
	    if ([aSet find:anElement]) [intersection add:anElement];
	}
	elements = [elements free];
	
	return intersection;
    }
}

- union:aSet
{
    if (self == aSet) {
	return [self copy];
    } else {
	return [[self copy] addContentsOf:aSet];
    }
}

- difference:aSet
{
    if (self == aSet) {
	return [isa new];
    } else {
	id difference = [self copy];
	[aSet removeContentsFrom:difference];
	return difference;
    }
}

/*****************************************************************************
 *
 * Printing
 *
 ****************************************************************************/

- printToFile:(FILE *)aFile
{
    [[[self eachElement] printToFile:aFile] free];return self;
}

/*****************************************************************************
 *
 * NextStep Read & Write
 *
 ****************************************************************************/

#ifdef __NeXT__
static void objptr_write(NXTypedStream *stream,id *a,int n)
{
    while (n--) { id obj = *a++; if (obj) NXWriteObject(stream,obj); }
}

static void objset_write(NXTypedStream *stream,objset_t self)
{
    int n = self->count;
    NXWriteType(stream,"i",&n);
    objptr_write(stream,self->ptr,self->capacity);
}

static void objset_read(NXTypedStream *stream,objset_t self)
{
    int n;
    NXReadType(stream,"i",&n);
    objset_init(self,0,2 * n + 1);
    while (n--) { id obj = NXReadObject(stream);objset_add(self,obj); }
}

- write:(NXTypedStream *)stream
{
    [super write:stream];objset_write(stream,&value);return self;
}

- read:(NXTypedStream *)stream
{
    [super read:stream];objset_read(stream,&value);return self;
}

#endif /* __NeXT__ */

@end

