#include <u.h>
#include <libc.h>
#include <libg.h>
#include <frame.h>
#include <text.h>

static Rune	l1[] =		{ '{', '[', '(', '<', 0253, 0};
static Rune	l2[] =		{ '\n', 0};
static Rune	l3[] =		{ '\'', '"', '`', 0};
Rune		*left[]=	{ l1, l2, l3, 0};

static Rune	r1[] =		{'}', ']', ')', '>', 0273, 0};
static Rune	r2[] =		{'\n', 0};
static Rune	r3[] =		{'\'', '"', '`', 0};
Rune		*right[]=	{ r1, r2, r3, 0};

static ulong	cp;

static int
wgetc(Text *t)
{
	if(cp < t->length)
		return t->text[cp++];
	return 0;
}

static int
wbgetc(Text *t)
{
	if(cp > 0)
		return t->text[--cp];
	return 0;
}

static Rune *
strrune(Rune *s, Rune c)
{
	Rune c1;

	if(c == 0) {
		while(*s++)
			;
		return s-1;
	}

	while(c1 = *s++)
		if(c1 == c)
			return s-1;
	return 0;
}

static int
clickmatch(Text *t, int cl, int cr, int dir)
{
	int c, nest;

	nest=1;
	while((c=(dir>0? wgetc(t) : wbgetc(t))) > 0) {
		if(cl=='\n' && c==0x04)	/* EOT is trouble */
			return 1;
		if(c == cr){
			if(--nest == 0)
				return 1;
		}else if(c == cl)
			nest++;
	}
	return cl=='\n' && nest==1;
}

void
doubleclick(Text *t, ulong p0)
{
	int c, i;
	Rune *r, *l;

	t->p0 = t->p1 = p0;
	for(i=0; left[i]; i++){
		l = left[i];
		r = right[i];
		/* try left match */
		if(p0 == 0){
			cp = p0;
			c = '\n';
		}else{
			cp = p0-1;
			c = wgetc(t);
		}
		if(strrune(l, c)){
			if(clickmatch(t, c, r[strrune(l, c)-l], 1)){
				t->p0 = p0;
				t->p1 = cp-1;
				if(c=='\n' && (cp--,wgetc(t))!=0x04)	/* EOT is trouble */
					t->p1++;
			}
			return;
		}
		/* try right match */
		if(p0 == t->length){
			cp = p0;
			c='\n';
		}else{
			cp =  p0+1;
			c = wbgetc(t);
		}
		if(strrune(r, c)){
			if(clickmatch(t, c, l[strrune(r, c)-r], -1)){
				t->p0 = cp;
				if(c!='\n' || cp!=0 || (cp=0,wgetc(t))=='\n')
					t->p0++;
				t->p1 = p0+(p0<t->length && c=='\n');
			}
			return;
		}
	}
	/* try filling out word to right */
	cp = p0;
	while(alnum(wgetc(t)))
		t->p1++;
	/* try filling out word to left */
	cp = p0;
	while(alnum(wbgetc(t)))
		t->p0--;
}

int
alnum(int c)
{
	/*
	 * Hard to get absolutely right.  Use what we know about ASCII
	 * and assume anything above the Latin control characters is
	 * potentially an alphanumeric.
	 */
	if(c <= ' ')
		return 0;
	if(0x7F<=c && c<=0xA0)
		return 0;
	if(utfrune("!\"#$%&'()*+,-./:;<=>?@`[\\]^{|}~", c))
		return 0;
	return 1;
}
