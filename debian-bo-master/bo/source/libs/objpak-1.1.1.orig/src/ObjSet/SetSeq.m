
/*
 * ObjectPak Objective C Class Library
 */

#include <objpak.h>

@implementation ObjSetSeq
- (objsetseq_t) objsetseqValue
{
    return &value;
}

static void objsetseq_init(objsetseq_t self,id aSet)
{
    self->set    = [aSet objsetValue];
    self->offset = 0;
}

+ over:aSet
{
    self = [super new];objsetseq_init([self objsetseqValue],aSet);return self;
}

- copy
{
    return [super copy];
}

- free
{
    return [super free];
}

static int objset_size(objset_t self)
{
    return self->count;
}

static int objsetseq_size(objsetseq_t self)
{
    return objset_size(self->set);
}

static id objset_at(objset_t self,int i)
{
    assert(0 <= i && i < self->capacity);return (self->ptr)[i];
}

- (unsigned) size
{
    return (unsigned)objsetseq_size([self objsetseqValue]);
}

static int objptr_match(id *p,int i,int n)
{
    while (i < n) if (p[i]) return i;else i++;
    return -1;
}

static int objset_match(objset_t self,int i)
{
    return objptr_match(self->ptr,i,self->capacity);
}

static id objsetseq_next(objsetseq_t self)
{
    int i = objset_match(self->set,self->offset);
    if (i == -1) {
	return nil;
    } else {
	id obj = objset_at(self->set,i);self->offset=i+1;return obj;
    }
}

- next
{
    return objsetseq_next([self objsetseqValue]);
}

static id objsetseq_peek(objsetseq_t self)
{
    int i = objset_match(self->set,self->offset);
    if (i == -1) {
	return nil;
    } else {
	return objset_at(self->set,i);
    }
}

- peek
{
    return objsetseq_peek([self objsetseqValue]);
}

static int objptr_prev(id *p,int i)
{
    while (--i) if (p[i]) return i;
    return -1;
}

static int objset_prev(objset_t self,int i)
{
    return objptr_prev(self->ptr,i);
}

static id objsetseq_prev(objsetseq_t self)
{
    int i = objset_prev(self->set,self->offset);
    return (i == -1)?nil:objset_at(self->set,i);
}

- previous
{
    return objsetseq_prev([self objsetseqValue]);
}

static id objsetseq_first(objsetseq_t self)
{
    int i = objset_match(self->set,0);
    return (i==-1)?nil:objset_at(self->set,i);
}

- first
{
    return objsetseq_first([self objsetseqValue]);
}

static int objset_last(objset_t self)
{
    return objset_prev(self,self->capacity);
}

static id objsetseq_last(objsetseq_t self)
{
    int i = objset_last(self->set);return (i==-1)?nil:objset_at(self->set,i);
}

- last
{
    return objsetseq_last([self objsetseqValue]);
}

@end

