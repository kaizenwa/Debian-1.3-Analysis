
/*
 * ObjectPak Objective C Class Library
 */

#include <objpak.h>

#define DEFAULT_CAPACITY (16)

@implementation ObjCltn

/*****************************************************************************
 *
 * Creation
 *
 ****************************************************************************/

static void objcol_init(objcol_t self,int n,int c)
{
    assert(0 <= n && n <= c);
    self->count    = n;
    self->capacity = c;
    self->ptr      = (id *)malloc(c * sizeof(id));
}

+ new
{
    self = [super new];
    objcol_init([self objcolValue],0,DEFAULT_CAPACITY);
    return self;
}

static void objptr_copy(id *p,id *q,int c)
{
    while (c--) *p++ = *q++;
}

static void objcol_copy(objcol_t dst,objcol_t src)
{
    objcol_init(dst,src->count,src->count);
    objptr_copy(dst->ptr,src->ptr,src->count);
}

- copy
{
    id aCopy = [super copy];
    objcol_copy([aCopy objcolValue],[self objcolValue]);
    return aCopy;
}

static void objptr_deepcopy(id *p,id *q,int c)
{
    while (c--) { id obj = *q++;*p++ = (obj)?[obj deepCopy]:nil; }
}

static void objcol_deepcopy(objcol_t dst,objcol_t src)
{
    objcol_init(dst,src->count,src->count);
    objptr_deepcopy(dst->ptr,src->ptr,src->count);
}

- deepCopy
{
    id aCopy = [super deepCopy];
    objcol_deepcopy([aCopy objcolValue],[self objcolValue]);
    return aCopy;
}

static void objcol_empty(objcol_t self)
{
    self->count = 0;
}

- emptyYourself
{
    objcol_empty([self objcolValue]);return self;
}

static void objptr_clear(id *p,int c)
{
    while (c--) { id obj = *p;*p++ = (obj)?[obj free]:nil; }
}

static void objcol_freecontents(objcol_t self)
{
    objptr_clear(self->ptr,self->count);self->count = 0;
}

- freeContents
{
    objcol_freecontents([self objcolValue]);return self;
}

static void objcol_clear(objcol_t self)
{
    self->count = 0;self->capacity = 0;free(self->ptr);self->ptr = NULL;
}

- free
{
    objcol_clear([self objcolValue]);return [super free];
}

/*****************************************************************************
 *
 * Interrogation
 *
 ****************************************************************************/

- (objcol_t) objcolValue
{
    return &value;
}

- (unsigned) size
{
    return (unsigned)([self objcolValue]->count);
}

- (BOOL) isEmpty
{
    return [self objcolValue]->count == 0;
}

- (unsigned) lastOffset
{
    return [self objcolValue]->count - 1;
}

- eachElement
{
    id aCarrier = [ObjCltnSeq over:self];return [ObjSeq over:aCarrier];
}

static id objptr_first(id *p,int n)
{
    return (n)?p[0]:nil;
}

static id objcol_first(objcol_t self)
{
    return objptr_first(self->ptr,self->count);
}

- firstElement
{
    return objcol_first([self objcolValue]);
}

static id objptr_last(id *p,int n)
{
    return (n)?p[n-1]:nil;
}

static id objcol_last(objcol_t self)
{
    return objptr_last(self->ptr,self->count);
}

- lastElement
{
    return objcol_last([self objcolValue]);
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

static BOOL objptr_eq(id *p,id *q,int n)
{
    while (n--) if ([*p++ notEqual:*q++]) return NO;
    return YES;
}

static BOOL objcol_eq(objcol_t a,objcol_t b)
{
    if (a->count == b->count) {
	return objptr_eq(a->ptr,b->ptr,a->count);
    } else {
	return NO;
    }
}

- (BOOL) isEqual:aCltn
{
    return (self==aCltn)?YES:objcol_eq([self objcolValue],[aCltn objcolValue]);
}

/*****************************************************************************
 *
 * Adding
 *
 ****************************************************************************/

static BOOL objcol_needsexpand(objcol_t self)
{
    assert(self->count <= self->capacity);return self->count == self->capacity;
}

static void objcol_expand(objcol_t self)
{
    self->capacity = 1 + 2*self->capacity;
    self->ptr      = (id *)realloc(self->ptr,sizeof(id) * self->capacity);
}

static int objptr_addlast(id *p,id obj,int n)
{
    p[n] = obj;return n+1;
}

static void objcol_addlast(objcol_t self,id obj)
{
    if (objcol_needsexpand(self)) objcol_expand(self);
    self->count = objptr_addlast(self->ptr,obj,self->count);
}

- add:anObject
{
    if (anObject) {
	objcol_addlast([self objcolValue],anObject);
	return self;
    } else {
	return self;
    }
}

static int objptr_addfirst(id *p,id obj,int n)
{
    int m = n;
    p += n;while (m--) { id *q = p - 1;*p = *q;p = q; }
    *p = obj;return n + 1;
}

static void objcol_addfirst(objcol_t self,id obj)
{
    if (objcol_needsexpand(self)) objcol_expand(self);
    self->count = objptr_addfirst(self->ptr,obj,self->count);
}

- addFirst:newObject
{
    if (newObject) {
	objcol_addfirst([self objcolValue],newObject);
	return self;
    } else {
	return self;
    }
}

- addLast:newObject
{
    return [self add:newObject];
}

- addIfAbsent:anObject
{
    if ([self find:anObject] == nil) [self add:anObject];
    return self;
}

- addIfAbsentMatching:anObject
{
    if ([self findMatching:anObject] == nil) [self add:anObject];
    return self;
}

/*****************************************************************************
 *
 * Insertion
 *
 ****************************************************************************/

static int objptr_insert(id *p,id obj,int i,int n)
{
    if (i==n) {
	return objptr_addlast(p,obj,n);
    } else {
	return i + objptr_addfirst(p + i,obj,n - i);
    }
}

static void objcol_insert(objcol_t self,id obj,int i)
{
    if (objcol_needsexpand(self)) objcol_expand(self);
    self->count = objptr_insert(self->ptr,obj,i,self->count);
}

- boundsError:(unsigned)anOffset in:(SEL)aSelector
{
    fprintf(stderr,"Bounds Error\n");exit(1);
}

- at:(unsigned )anOffset insert:anObject
{
    if (anObject) {
	if (anOffset > [self size]) [self boundsError:anOffset in:_cmd];
	objcol_insert([self objcolValue],anObject,(int)anOffset);
	return self;
   } else {
	return self;
    }
}

- couldntFind:anObject in:(SEL)aSelector
{
    fprintf(stderr,"Couldn't Find Object Error\n");exit(1);
}

- insert:newObject after:oldObject
{
    if (newObject) {
	unsigned offset = [self offsetOf:oldObject];
	if (offset == (unsigned)-1) [self couldntFind:oldObject in:_cmd];
	return [self at:offset+1 insert:newObject];
    } else {
	return self;
    }
}

- insert:newObject before:oldObject
{
    if (newObject) {
	unsigned offset = [self offsetOf:oldObject];
	if (offset == (unsigned)-1) [self couldntFind:oldObject in:_cmd];
	return [self at:offset-1 insert:newObject];
    } else {
	return self;
    }
}

/*****************************************************************************
 *
 * Relative Accessing
 *
 ****************************************************************************/

- after:anObject
{
    unsigned offset = [self offsetOf:anObject];
    if (offset == (unsigned)-1) return [self couldntFind:anObject in:_cmd];
    return (offset == [self lastOffset])?nil:[self at:offset+1];
}

- before:anObject
{
    unsigned offset = [self offsetOf:anObject];
    if (offset == (unsigned)-1) return [self couldntFind:anObject in:_cmd];
    return (offset == 0)?nil:[self at:offset-1];
}

static id objcol_at(objcol_t self,int i)
{
    assert(0 <= i && i < self->count);return (self->ptr)[i];
}

- at:(unsigned )anOffset
{
    if (anOffset > [self lastOffset]) [self boundsError:anOffset in:_cmd];
    return objcol_at([self objcolValue],anOffset);
}

static id objcol_atput(objcol_t self,int i,id obj)
{
    id tmp;assert(0 <= i && i < self->count);
    tmp = (self->ptr)[i];(self->ptr)[i] = obj;return tmp;
}

- at:(unsigned )anOffset put:anObject
{
    if (anOffset > [self lastOffset]) [self boundsError:anOffset in:_cmd];
    return objcol_atput([self objcolValue],anOffset,anObject);
}

/*****************************************************************************
 *
 * Removing
 *
 ****************************************************************************/

static id objptr_removefirst(id *p,int n)
{
    id obj = *p;
    n--;while (n--) { id *q = p + 1;*p = *q;p = q; }
    return obj;
}

static id objcol_removefirst(objcol_t self)
{
    if (self->count) {
	id obj = objptr_removefirst(self->ptr,self->count);
	self->count--;return obj;
    } else {
	return nil;
    }
}

- removeFirst
{
    return objcol_removefirst([self objcolValue]);
}

static id objptr_removelast(id *p,int n)
{
    return p[n - 1];
}

static id objcol_removelast(objcol_t self)
{
    if (self->count) {
	id obj = objptr_removelast(self->ptr,self->count);
	self->count--;return obj;
    } else {
	return nil;
    }
}

- removeLast
{
    return objcol_removelast([self objcolValue]);
}

static id objptr_removeat(id *p,int i,int n)
{
    if (i == n - 1) {
	return objptr_removelast(p,n);
    } else {
	return objptr_removefirst(p + i,n - i);
    }
}

static id objcol_removeat(objcol_t self,int i)
{
    if (self->count) {
	id obj = objptr_removeat(self->ptr,i,self->count);
	self->count--;return obj;
    } else {
	return nil;
    }
}

- removeAt:(unsigned )anOffset
{
    if (anOffset > [self lastOffset]) [self boundsError:anOffset in:_cmd];
    return objcol_removeat([self objcolValue],anOffset);
}

- remove:oldObject
{
    unsigned offset = [self offsetOf:oldObject];
    if (offset == (unsigned)-1) return nil;
    return [self removeAt:offset];
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

static id objptr_find(id *p,id obj,int n)
{
    int i;
    for(i=0;i<n;i++) if (*p++ == obj) return *p;
    return nil;
}

static id objcol_find(objcol_t self,id obj)
{
    return objptr_find(self->ptr,obj,self->count);
}

- find:anObject
{
    return objcol_find([self objcolValue],anObject);
}

static id objptr_findmatch(id *p,id obj,int n)
{
    int i;
    for(i=0;i<n;i++) if ([*p++ isEqual:obj]) return *p;
    return nil;
}

static id objcol_findmatch(objcol_t self,id obj)
{
    return objptr_findmatch(self->ptr,obj,self->count);
}

- findMatching:anObject
{
    return objcol_findmatch([self objcolValue],anObject);
}

static id objptr_findstr(id *p,STR s,int n)
{
    int i;
    for(i=0;i<n;i++) if ([*p++ isEqualSTR:s]) return *p;
    return nil;
}

static id objcol_findstr(objcol_t self,STR s)
{
    return objptr_findstr(self->ptr,s,self->count);
}

- findSTR:(STR )strValue
{
    return objcol_findstr([self objcolValue],strValue);
}

- (BOOL) contains:anObject
{
    return [self find:anObject] != nil;
}

static int objptr_offset(id *p,id obj,int n)
{
    int i;
    for(i=0;i<n;i++) if (*p++ == obj) return i;
    return -1;
}

static int objcol_offset(objcol_t self,id obj)
{
    return objptr_offset(self->ptr,obj,self->count);
}

- (unsigned) offsetOf:anObject
{
    return (unsigned)objcol_offset([self objcolValue],anObject);
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
    while (n--) NXWriteObject(stream,*a++);
}

static void objptr_read(NXTypedStream *stream,id *a,int n)
{
    while (n--) *a++ = NXReadObject(stream);
}

static void objcol_write(NXTypedStream *stream,objcol_t self)
{
    int n = self->count;
    NXWriteType(stream,"i",&n);
    objptr_write(stream,self->ptr,n);
}

static void objcol_read(NXTypedStream *stream,objcol_t self)
{
    int n;
    NXReadType(stream,"i",&n);
    objcol_init(self,n,n);
    objptr_read(stream,self->ptr,n);
}

- write:(NXTypedStream *)stream
{
    [super write:stream];objcol_write(stream,&value);return self;
}

- read:(NXTypedStream *)stream
{
    [super read:stream];objcol_read(stream,&value);return self;
}

#endif /* __NeXT__ */

@end

