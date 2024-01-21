
/*
 * ObjectPak Objective C Class Library
 */

#include <objpak.h>

@implementation ObjSortSeq
static objbbt_t objbbt_first(objbbt_t self)
{
    while (self->llink) self = self->llink;
    return self;
}

static objbbt_t objbbt_last(objbbt_t self)
{
    while (self->rlink) self = self->rlink;
    return self;
}

- setUpSort:aSort
{
    top    = [aSort objbbtTop];
    prev   = NULL;
    next   = (top)?objbbt_first(top):NULL;
    return self;
}

+ over:aSort
{
    self = [super new];[self setUpSort:aSort];return self;
}

- copy
{
    return [super copy];
}

- free
{
    return [super free];
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
    return (top)?objbbt_size(top):0;
}

static objbbt_t objbbt_nextulink(objbbt_t self,objbbt_t top)
{
    while (self != top) {
	objbbt_t ulink;
	ulink = self->ulink;
	if (self == ulink->llink) return ulink;
	self  = ulink;
    }
    
    return NULL;
}

static objbbt_t objbbt_next(objbbt_t self,objbbt_t top)
{
    objbbt_t link;

    if ((link = self->rlink)) {
	return objbbt_first(link);
    } else {
	return objbbt_nextulink(self,top);
    }
}

- next
{
    if (next) {
	prev = next;
	next = objbbt_next(next,top);
	return prev->key;
    } else {
	return nil;
    }
}

- peek
{
    return (next)?next->key:nil;
}

- previous
{
    return (prev)?prev->key:nil;
}

- first
{
    if (top) {
	objbbt_t first = objbbt_first(top);return first->key;
    } else {
	return nil;
    }
}

- last
{
    if (top) {
	objbbt_t last = objbbt_last(top);return last->key;
    } else {
	return nil;
    }
}

@end

