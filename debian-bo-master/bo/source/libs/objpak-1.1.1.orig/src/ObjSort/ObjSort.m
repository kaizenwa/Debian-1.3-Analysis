
/*
 * ObjectPak Objective C Class Library
 */

#include <objpak.h>

#ifndef NDEBUG
#define CHECK_BALANCE
#endif

@implementation ObjSort

/*****************************************************************************
 *
 * Creation
 *
 ****************************************************************************/

static objbbt_t objbbt_alloc()
{
    return (objbbt_t)malloc(sizeof(struct objbbt));
}

static objbbt_t objbbt_init(objbbt_t self,id key)
{
    self->ulink   = NULL;
    self->llink   = NULL;
    self->rlink   = NULL;
    self->key     = key;
    self->balance = 0;
    return self;
}

static objbbt_t objbbt_new(id key)
{
    return objbbt_init(objbbt_alloc(),key);
}

static int objbbt_sign(objbbt_t ulink,objbbt_t self)
{
    assert(ulink->llink == self || ulink->rlink == self);
    return (ulink->llink == self)?-1:+1;
}

static objbbt_t objbbt_slink(objbbt_t self,int sign)
{
    assert(sign == +1 || sign == -1);return (sign>0)?self->rlink:self->llink;
}

static void objbbt_setllink(objbbt_t self,objbbt_t node)
{
    self->llink = node;if (node) node->ulink = self;
}

static void objbbt_setrlink(objbbt_t self,objbbt_t node)
{
    self->rlink = node;if (node) node->ulink = self;
}

static void objbbt_setslink(objbbt_t self,objbbt_t node,int sign)
{
    if (sign>0) objbbt_setrlink(self,node); else objbbt_setllink(self,node);
}

static void objbbt_freeobjects(objbbt_t self)
{
    if (self->llink) objbbt_freeobjects(self->llink);
    if (self->rlink) objbbt_freeobjects(self->rlink);
    self->key = [self->key free];
}

static objbbt_t objbbt_free(objbbt_t self)
{
    if (self->llink) self->llink = objbbt_free(self->llink);
    if (self->rlink) self->rlink = objbbt_free(self->rlink);
    free(self);return NULL;
}

+ new
{
    return [self newCmpSel:@selector(compare:)];
}

+ newDictCompare
{
    return [self newCmpSel:@selector(dictCompare:)];
}

- setUpCmpSel:(SEL)aSel
{
    cmpSel = aSel;
    objbbt_init(&value,(id)0xdeadbeaf);
    return self;
}

+ newCmpSel:(SEL)aSel
{
    self = [super new];[self setUpCmpSel:aSel];return self;
}

- copy
{
    return [[isa new] addContentsOf:self];
}

- deepCopy
{
    id aSeq,elt;
    id aCopy = [isa new];
    
    aSeq = [self eachElement];
    while ((elt = [aSeq next])) [aCopy add:[elt deepCopy]];
    aSeq = [aSeq free];

    return aCopy;
}

- emptyYourself
{
    if (value.llink) value.llink = objbbt_free(value.llink);
    return self;
}

- freeContents
{
    if (value.llink) {
	objbbt_freeobjects(value.llink);
	value.llink = objbbt_free(value.llink);
    }
    return self;
}

- free
{
    if (value.llink) value.llink = objbbt_free(value.llink);
    return [super free];
}

/*****************************************************************************
 *
 * Interrogation
 *
 ****************************************************************************/

- (objbbt_t) objbbtTop
{
    return value.llink;
}

- (SEL) comparisonSelector
{
    return cmpSel;
}

static int objbbt_size(objbbt_t self)
{
    int size = 1;
    if (self->llink) size += objbbt_size(self->llink);
    if (self->rlink) size += objbbt_size(self->rlink);
    return size;
}

- (unsigned) size
{
    return (value.llink)?objbbt_size(value.llink):0;
}

- (BOOL) isEmpty
{
    return value.llink == NULL;
}

- eachElement
{
    id aCarrier = [ObjSortSeq over:self];return [ObjSeq over:aCarrier];
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

- (BOOL) isEqual:aSort
{
    [self notImplemented:_cmd]; return NO;
}

/*****************************************************************************
 *
 * Adding
 *
 ****************************************************************************/

static int objbbt_cmp(objbbt_t self,id key,SEL cmpSel,objbbt_t *offset)
{
    objbbt_t link;
    int cmp = (int)[key perform:cmpSel with:self->key];
    
    if (cmp == 0) { *offset = self;return cmp; }
    if (cmp  < 0) { link = self->llink; }
    if (cmp  > 0) { link = self->rlink; }
    
    return (link)?objbbt_cmp(link,key,cmpSel,offset):(*offset=self,cmp);
}

static int objbbt_cmpne(objbbt_t self,id key,SEL cmpSel,objbbt_t *offset)
{
    objbbt_t link;
    int cmp = (int)[key perform:cmpSel with:self->key];

    if (cmp == 0) { cmp = +1; }
    if (cmp  < 0) { link = self->llink; }
    if (cmp  > 0) { link = self->rlink; }

    return (link)?objbbt_cmpne(link,key,cmpSel,offset):(*offset=self,cmp);
}

static int objbbt_height(objbbt_t self)
{
    if (self) {
        int a,b;
	a = objbbt_height(self->llink);
	b = objbbt_height(self->rlink);
	assert(self->balance == (b - a));
	return 1 + ((a>b)?a:b);
    } else {
	return 0;
    }
}

static objbbt_t objbbt_bnode(objbbt_t top,objbbt_t new)
{
    objbbt_t ulink;
    while (new != top && new->balance == 0) new = new->ulink;
    assert(new->balance != 0 || new == top);
    return new;
}

static int objbbt_adjust(objbbt_t bnode,objbbt_t new)
{
    int sign = 0;
    objbbt_t ulink;
    
    while (1) {
	ulink = new->ulink;
	sign  = objbbt_sign(ulink,new);
	if (ulink == bnode) break;
	assert(ulink->balance == 0);ulink->balance = sign;new = ulink;
    }
    
    return sign;
}

/*
 *                A                       B
 *               / \                     / \
 *              a   B       --->        A   \
 *             /   / \                 / \   c
 *                b   c               a   b   \
 *               /     \             /     \   \
 *                      \
 */

static void objbbt_sglrot(objbbt_t A,objbbt_t B,int sign)
{
    objbbt_t U = A->ulink;
    objbbt_setslink(A,objbbt_slink(B,-sign),sign);
    objbbt_setslink(B,A,-sign);
    A->balance = 0;B->balance = 0;objbbt_setslink(U,B,objbbt_sign(U,A));
}
    
/*                                            
 *                A                            X      
 *               / \                        /     \
 *              a   B       --->           A       B   
 *             /   / \                    / \     / \ 
 *                X   d                  a   b   c   d
 *               / \   \                /     \ /     \  
 *              b   c                                
 *             /     \
 */

static void objbbt_dblrot(objbbt_t A,objbbt_t B,int sign)
{
    objbbt_t U = A->ulink;
    objbbt_t X = objbbt_slink(B,-sign);
    
    objbbt_setslink(B,objbbt_slink(X,sign),-sign);
    objbbt_setslink(X,B,+sign);
    objbbt_setslink(A,objbbt_slink(X,-sign),sign);
    objbbt_setslink(X,A,-sign);
    
    if (X->balance == +sign) { A->balance = -sign; B->balance =     0; }
    if (X->balance ==     0) { A->balance =     0; B->balance =     0; }
    if (X->balance == -sign) { A->balance =     0; B->balance = +sign; }
    
    X->balance = 0;objbbt_setslink(U,X,objbbt_sign(U,A));
}

static void objbbt_rot(objbbt_t A,int sign)
{
    objbbt_t B = objbbt_slink(A,sign);
    assert(sign == A->balance && sign != 0 && B->balance != 0);
    if (sign == +B->balance) return objbbt_sglrot(A,B,sign);
    if (sign == -B->balance) return objbbt_dblrot(A,B,sign);
}

static void objbbt_rebalance(objbbt_t top,objbbt_t new)
{
    int sign;
    objbbt_t bnode;
    
    bnode = objbbt_bnode(top,new);
    sign  = objbbt_adjust(bnode,new);
    
    if (bnode->balance == 0)     { bnode->balance = sign;return;  }
    if (bnode->balance == -sign) { bnode->balance = 0;return;     }
    if (bnode->balance == +sign) { return objbbt_rot(bnode,sign); }
}

static void objbbt_addfirst(objbbt_t self,id key)
{
    objbbt_setllink(self,objbbt_new(key));
}

static void objbbt_addat(objbbt_t top,id key,int cmp,objbbt_t offset)
{
    objbbt_t new = objbbt_new(key);
    
    assert(cmp < 0 || cmp > 0);
    if (cmp < 0) objbbt_setllink(offset,new);
    if (cmp > 0) objbbt_setrlink(offset,new);
    
    objbbt_rebalance(top,new);
    
#ifdef CHECK_BALANCE
    assert(objbbt_height(top) > 0);
#endif
}

static void objbbt_add(objbbt_t top,id key,SEL selCmp)
{
    objbbt_t offset = NULL;
    int cmp = objbbt_cmpne(top,key,selCmp,&offset);
    objbbt_addat(top,key,cmp,offset);
}

- add:anObject
{
    if (anObject) {
	if (value.llink) {
	    objbbt_add(value.llink,anObject,cmpSel);
	    return self;
	} else {
	    objbbt_addfirst(&value,anObject);
	    return self;
	}
    } else {
	return nil;
    }
}

static id objbbt_addnfind(objbbt_t top,id key,SEL selCmp)
{
    objbbt_t offset = NULL;
    int cmp = objbbt_cmpne(top,key,selCmp,&offset);
    if (cmp == 0) {
	return offset->key;
    } else {
	objbbt_addat(top,key,cmp,offset);
	return key;
    }
}

- addNTest:anObject
{
    if (anObject) {
	if (value.llink) {
	    id res = objbbt_addnfind(value.llink,anObject,cmpSel);
	    return (res == anObject)?anObject:nil;
	} else {
	    objbbt_addfirst(&value,anObject);
	    return anObject;
	}
    } else {
	return nil;
    }
}

- filter:anObject
{
    if (anObject) {
	if (value.llink) {
	    id res = objbbt_addnfind(value.llink,anObject,cmpSel);
	    return (res == anObject)?anObject:([anObject free],res);
	} else {
	    objbbt_addfirst(&value,anObject);
	    return anObject;
	}
    } else {
	return nil;
    }
}

static id objbbt_replace(objbbt_t top,id key,SEL selCmp)
{
    objbbt_t offset = NULL;
    int cmp = objbbt_cmpne(top,key,selCmp,&offset);
    if (cmp == 0) {
	id tmp = offset->key;offset->key = key;return tmp;
    } else {
	objbbt_addat(top,key,cmp,offset);
	return nil;
    }
}

- replace:anObject
{
    if (anObject) {
	if (value.llink) {
	    return objbbt_replace(value.llink,anObject,cmpSel);
	} else {
	    objbbt_addfirst(&value,anObject);
	    return nil;
	}
    } else {
	return nil;
    }
}

/*****************************************************************************
 *
 * Removing
 *
 ****************************************************************************/

- remove:oldObject
{
    if (oldObject) {
	return [self notImplemented:_cmd]; 
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

static id objbbt_find(objbbt_t self,id key,SEL cmpSel)
{
    int cmp;
    objbbt_t offset = NULL;
    
    if ((cmp = objbbt_cmp(self,key,cmpSel,&offset))) {
	return nil;
    } else {
	assert([key isEqual:offset->key]);return offset->key;
    }
}

- find:anObject
{
    if (anObject) {
	return (value.llink)?objbbt_find(value.llink,anObject,cmpSel):nil;
    } else {
	return nil;
    }
}

- (BOOL) contains:anObject
{
    return (BOOL)([self find:anObject] ? YES : NO); 
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

static void writeElements(NXTypedStream *stream,id self)
{
    id elt,aSeq;
    aSeq = [self eachElement];
    while ((elt = [aSeq next])) NXWriteObject(stream,elt);
    aSeq = [aSeq free];    
}

- write:(NXTypedStream *)stream
{
    int n = [self size];
    [super write:stream];NXWriteType(stream,"i",&n);
    writeElements(stream,self);return self;
}

- read:(NXTypedStream *)stream
{
    int n;
    [super read:stream];
    NXReadType(stream,"i",&n);while (n--) { [self add:NXReadObject(stream)]; }
    return self;
}

#endif /* __NeXT__ */

@end

