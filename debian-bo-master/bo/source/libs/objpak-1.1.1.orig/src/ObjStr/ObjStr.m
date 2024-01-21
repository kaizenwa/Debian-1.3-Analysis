
/*
 * ObjectPak Objective C Class Library
 */


#include <objpak.h>

#define DEFAULT_CAPACITY (16)

@implementation ObjStr

/*****************************************************************************
 *
 * Creation
 *
 ****************************************************************************/

static int str_len(char *s)
{
    int len = 0;while (*s++) len++;return len;
}

static void str_ncpy(char *dst,char *src,int n)
{
    int c;while (n-- && (c = *src++)) *dst++ = c;*dst++ = '\0';
}

static void str_cpy(char *dst,char *src)
{
    int c;while ((c = *src++)) *dst++ = c;*dst++ = '\0';
}

static void objstr_init(objstr_t self,char *s,int n,int c)
{
    assert(0 <= n && n+1 <= c);
    
    self->count    = n;
    self->capacity = c;
    self->ptr      = (char *)malloc(c);
    
    str_ncpy(self->ptr,s,n);self->ptr[n] = '\0';
}

+ new
{
    self = [super new];
    objstr_init([self objstrValue],"",0,DEFAULT_CAPACITY);
    return self;
}

static void objstr_initstr(objstr_t self,char *s,int n)
{
    objstr_init(self,s,n,n+1);
}

+ str:(STR)strValue
{
    if (strValue) {
	self = [super new];
	objstr_initstr([self objstrValue],strValue,str_len(strValue));
	return self;
    } else {
	return [self new];
    }
}

+ sprintf:(STR)format,...
{
    va_list ap;
    char aBuffer[BUFSIZ];

    va_start(ap,format);
    vsprintf(aBuffer,format,ap);
    va_end(ap);
    
    return [self str:aBuffer];
}

static void objstr_copy(objstr_t dst,objstr_t src)
{
    int n = src->count;
    
    assert(n == str_len(src->ptr) && 0 <= n && n < src->capacity);
    
    dst->count    = n;
    dst->capacity = n + 1;
    dst->ptr      = (char *)malloc(n + 1);str_cpy(dst->ptr,src->ptr);
}

- copy
{
    id aCopy = [super copy];
    objstr_copy([aCopy objstrValue],[self objstrValue]);
    return aCopy;
}

- deepCopy
{
    return [self copy];
}

static void objstr_clear(objstr_t self)
{
    free(self->ptr);self->ptr = NULL;
}

- free
{
    objstr_clear([self objstrValue]);return [super free];
}

/*****************************************************************************
 *
 * Comparison
 *
 ****************************************************************************/

static int str_cmp(char *s1,char *s2)
{
    int r;
    int c1, c2;

    while (1) {
	c1 = *s1++;
	c2 = *s2++;
	if (c1  == '\0') return (c2 == 0)?0:-1;
	if (c2  == '\0') return 1;
	if (r = c1 - c2) return r;
    }
}

static int objstr_cmp(objstr_t a,objstr_t b)
{
    return str_cmp(a->ptr,b->ptr);
}

- (int) compare:aStr
{
    return (self==aStr)?0:objstr_cmp([self objstrValue],[aStr objstrValue]);
}

static int objstr_cmpstr(objstr_t a,char *b)
{
    return str_cmp(a->ptr,b);
}

- (int) compareSTR:(STR)strValue
{
    return objstr_cmpstr([self objstrValue],strValue);
}

static unsigned str_hash(char *s)
{
    unsigned hash = 0;
    
    while (1) {
	if (*s == '\0') break;else hash ^= *s++;
	if (*s == '\0') break;else hash ^= (*s++ << 8);
	if (*s == '\0') break;else hash ^= (*s++ << 16);
	if (*s == '\0') break;else hash ^= (*s++ << 24);
    }
    
    return hash;
}

static unsigned objstr_hash(objstr_t self)
{
    return str_hash(self->ptr);
}

- (unsigned) hash
{
    return objstr_hash([self objstrValue]);
}

static int str_dictcmp(char *s1,char *s2)
{
    int r;
    int c1, c2;

    while (1) {
	while((c1 = *s1++) && !isalnum(c1))
	    ;
	while((c2 = *s2++) && !isalnum(c2))
	    ;
	if(c1 == '\0') 
	     return (c2 == 0)?0:-1;
	if(c2 == '\0') 
	    return 1;
	if(isupper(c1)) 
	    c1 = tolower(c1);
	if(isupper(c2)) 
	    c2 = tolower(c2);
	if(r = c1 - c2)
	    return r;
    }
}

static int objstr_dictcmp(objstr_t a,objstr_t b)
{
    return str_dictcmp(a->ptr,b->ptr);
}

- (int) dictCompare:aStr
{
    return objstr_dictcmp([self objstrValue],[aStr objstrValue]);
}

- (BOOL) isEqual:aStr
{
    return (self==aStr)?YES:([self compare:aStr] == 0);
}

- (BOOL) isEqualSTR:(STR)strValue
{
    return [self compareSTR:strValue] == 0;
}

/*****************************************************************************
 *
 * Interrogation
 *
 ****************************************************************************/

- (objstr_t) objstrValue
{
    return &value;
}

- (unsigned) size
{
    return [self objstrValue]->count;
}

static char objstr_charat(objstr_t self,int i)
{
    if (0 <= i && i < self->count) {
	return (self->ptr)[i];
    } else {
	return 0;
    }
}

- (char) charAt:(unsigned)anOffset
{
    return objstr_charat([self objstrValue],anOffset);
}

static char strputchar(char *self,char c)
{
    char r = *self;*self = c;return r;
}

static char objstr_putcharat(objstr_t self,int i,char c)
{
    if (0 <= i && i < self->count) {
	return strputchar(self->ptr + i,c);
    } else {
	return 0;
    }
}

- (char) charAt:(unsigned)anOffset put:(char)aChar
{
    return objstr_putcharat([self objstrValue],anOffset,aChar);
}

/*****************************************************************************
 *
 * Concatenation
 *
 ****************************************************************************/

- (STR) strcat:(STR)aBuffer
{
    str_cpy(aBuffer + str_len(aBuffer),[self str]);return aBuffer;
}

static void objstr_expand(objstr_t self)
{
    assert(self->count + 1 <= self->capacity);
    self->capacity = 1 + self->capacity * 2;
    self->ptr      = (char *)realloc(self->ptr,self->capacity);
}

static void objstr_concat(objstr_t self,char *s,int n)
{
    assert(n == str_len(s));
    while (self->count + n + 1 > self->capacity) objstr_expand(self);
    str_cpy(self->ptr + self->count,s);
}

- concatSTR:(STR)strValue
{
    objstr_concat([self objstrValue],strValue,str_len(strValue));
    return self;
}

static void objstr_assign(objstr_t self,char *s,int n)
{
    assert(n <= str_len(s));
    while (n + 1 > self->capacity) objstr_expand(self);
    str_ncpy(self->ptr,s,n);(self->ptr)[n] = '\0';
}

- assignSTR:(STR)strValue
{
    objstr_assign([self objstrValue],strValue,str_len(strValue));
    return self;
}

- assignSTR:(STR)strValue length:(unsigned)nChars
{
    objstr_assign([self objstrValue],strValue,nChars);
    return self;
}

/*****************************************************************************
 *
 * Format Conversions
 *
 ****************************************************************************/

- (double) asDouble
{
    return atof([self str]);
}

- (int) asInt
{
    return atoi([self str]);
}

- (long) asLong
{
    return atol([self str]);
}

- asSTR:(STR)aBuffer maxSize:(int)aSize
{
    str_ncpy(aBuffer,[self str],aSize);return self;
}

- (STR) str
{
    return [self objstrValue]->ptr;
}

- (STR) strCopy
{
    struct objstr aCopy;
    objstr_copy(&aCopy,[self objstrValue]);
    return aCopy.ptr;
}

/*****************************************************************************
 *
 * Conversions
 *
 ****************************************************************************/

static void strtolower(char *s)
{
    char c;while ((c = *s)) { *s++ = tolower(c); }
}

static void objstr_tolower(objstr_t self)
{
    strtolower(self->ptr);
}

- toLower
{
    objstr_tolower([self objstrValue]); return self;
}

static void strtoupper(char *s)
{
    char c;while ((c = *s)) { *s++ = toupper(c); }
}

static void objstr_toupper(objstr_t self)
{
    strtoupper(self->ptr);
}

- toUpper
{
    objstr_toupper([self objstrValue]); return self;
}

/*****************************************************************************
 *
 * Printing
 *
 ****************************************************************************/

- printToFile:(FILE *)aFile
{
    fputs([self str],aFile);return self;
}

/*****************************************************************************
 *
 * NextStep Read & Write
 *
 ****************************************************************************/

#ifdef __NeXT__
static void objstr_write(NXTypedStream *stream,objstr_t self)
{
    int n = self->count;
    char *str = self->ptr;
    NXWriteType(stream,"i",&n);
    NXWriteType(stream,"*",&str);
}

static void objstr_read(NXTypedStream *stream,objstr_t self)
{
    int n;
    char *str;
    NXReadType(stream,"i",&n);
    NXReadType(stream,"*",&str); 
    self->count = n;self->capacity = n;self->ptr = str;
}

- write:(NXTypedStream *)stream
{
    [super write:stream];objstr_write(stream,&value);return self;
}

- read:(NXTypedStream *)stream
{
    [super read:stream];objstr_read(stream,&value);return self;
}

#endif /* __NeXT__ */

@end

