#include <u.h>
#include <libc.h>
#include <libg.h>
#include <frame.h>
#include <text.h>

#define MAX(x, y) ((x > y) ? x : y)
#define	BUTTON(n)	(1<<(n-1))

static Menu	edit;
static char	*items[] = { "cut", "paste", "snarf", "exch", (char *)0 };

ulong
_backnl(Text *t, long p, ulong n)
{
	int	c;

	if (!n)
		return p;

	while (n-- > 0 && p > 0)
		while (--p > 0)
		{
			c = t->text[p];
			if (c == '\n')
				break;
		}
	if (p <= 0)
		return 0;
	return p+1;
}

Text *
textalloc(Bitmap *b, Rectangle r, Font *font)
{
	Text		*t;
	Rectangle	area, q;

	t = (Text *)malloc(sizeof(Text));
	if (!t)
		berror("textalloc: Text malloc");
	t->r = r;
	t->b = b;
	t->alloced = 512;
	t->text = (Rune *)calloc(t->alloced, sizeof(Rune));
	if (!t->text)
		berror("textalloc: calloc");
	t->length = 0;
	t->base = 0;
	t->p0 = 0;
	t->p1 = 0;
	t->pout = 0;
	t->pmark = 0;
	t->modified = 0;
	t->scrolling = 0;
	t->snarfed = (Rune *)0;
	t->snarflen = 0;
	t->data = (char *)0;

	area = inset(r, 4);
	q = area;
	/* q.min.x += 1; */
	q.max.x = q.min.x + 12;
	area.min.x += 15;
	t->scroll = scrollalloc(b, q);
	frinit(&t->f, area, font, b);
	texthighlight(t, t->p0, t->p1, F&~D);

	border(b, r, 1, F);
	return t;
}

void
textfree(Text *t)
{
	free(t->text);
	scrollfree(t->scroll);
	if (t->snarfed)
		free(t->snarfed);
	free(t);
}

void
texthit(Text *text, Event *e, ulong keys)
{
	Rune		runes[2];

	switch (keys)
	{
	case Ekeyboard:
		if (e->kbdc == '\b')
		{
			if (text->p1 != text->p0)
				textdelete(text, text->p0, text->p1);
			if (text->p0 > 0)
			{
				if (text->p0 == text->base)
					textset(text, _backnl(text, text->base, 3));
				textdelete(text, text->p0-1, text->p0);
			}
		}
		else
		{
			if (text->p1 != text->p0)
				textdelete(text, text->p0, text->p1);
			runes[0] = e->kbdc;
			runes[1] = '\0';
			textinsert(text, runes, runes+1, text->p0);
		}
		textshow(text, text->p0, text->scrolling);
		break;
	case Emouse:
		if (ptinrect(e->mouse.xy, text->scroll->r))
		{
			if (scrollhit(text->scroll, e))
				textscroll(text, text->scroll->buttons);
		}
		else if (e->mouse.buttons & BUTTON(1))
		{
			textselect(text, &e->mouse);
		}
		else if (e->mouse.buttons & BUTTON(2))
		{
			if (!edit.item)
				edit.item = items;
			switch (menuhit(2, &e->mouse, &edit))
			{
			case 0: /* cut */
				textsnarf(text, text->p0, text->p1);
				textdelete(text, text->p0, text->p1);
				break;
			case 2: /* snarf */
				textsnarf(text, text->p0, text->p1);
				break;
			case 1: /* paste */
				textpaste(text, text->p0, text->p1);
				break;
			case 3: /* exch */
				break;
			}
		}
		break;
	}
}

void
textinsert(Text *t, Rune *r1, Rune *r2, ulong p)
{
	ulong	n;

	if (p > t->length)
		p = t->length;
	else if (p < 0)
		p = 0;
	if ((n = r2-r1) <= 0)
		return;
	/*
	 * trucate stuff should probably go around here somewhere
	 * although I'd rather leave that to the user
	 */
	if (t->length + n > t->alloced)
	{
		t->alloced += (t->length + n - t->alloced) / 512 * 512 + 512;
		t->text = (Rune *)realloc(t->text, t->alloced*sizeof(Rune));
		if (!t->text)
			berror("textinsert: realloc");
	}
	if (p < t->length)
		memmove(t->text+p+n, t->text+p, (t->length-p)*sizeof(Rune));
	t->length += n;
	t->modified++;
	memmove(t->text+p, r1, n*sizeof(Rune));

	if ((p >= t->base) && (p <= t->end))
	{
		frinsert(&t->f, r1, r2, p - t->base);
		t->end = frcharofpt(&t->f, t->f.r.max) + t->base;
	}
	if (p <= t->p1)
		t->p1 += n;
	if (p <= t->p0)
		t->p0 += n;
	if (p <= t->pout)
		t->pout += n;
	if (p <= t->pmark)
		t->pmark += n;
	
	scrollset(t->scroll, t->base, t->end-t->base, t->length);
}

void
textinsertchar(Text *t, uchar *s1, uchar *s2, ulong p)
{
	Rune	*r, *q;
	
	if (s2 <= s1)
		return;
	r = (Rune *)calloc(s2-s1, sizeof(Rune));
	if (!r)
		berror("textinsertchar: calloc");
	for (q = r; s1 < s2; q++)
		if (*s1 < Runeself)
			*q = *s1++;
		else
			s1 += chartorune(q, (char *) s1);
	textinsert(t, r, q, p);
	free(r);
}

void
textselect(Text *t, Mouse *mouse)
{
	frselect(&t->f, mouse);
	t->p0 = t->f.p0 + t->base;
	t->p1 = t->f.p1 + t->base;
}

void
texthighlight(Text *t, ulong p0, ulong p1, Fcode fc)
{
	frselectp(&t->f, F&~D);
	t->p0 = p0;
	t->p1 = p1;
	p0 = (p0 < t->base) ? t->base : p0;
	p1 = (p1 < t->base) ? t->base : p1;
	t->f.p0 = ((p0 > t->end) ? t->end : p0) - t->base;
	t->f.p1 = ((p1 > t->end) ? t->end : p1) - t->base;
	frselectp(&t->f, fc);
}

void
textdelete(Text *t, ulong p0, ulong p1)
{
	if (p1 <= p0)
		return;

	memmove(t->text+p0, t->text+p1, (t->length-p1)*sizeof(Rune));
	t->length -= p1-p0;
	t->modified++;

	if (t->p1 > p1)
		t->p1 -= p1-p0;
	else if (t->p1 > p0)
		t->p1 = p0;
	if (t->p0 > p1)
		t->p0 -= p1-p0;
	else if (t->p0 > p0)
		t->p0 = p0;
	if (t->pout > p1)
		t->pout -= p1-p0;
	else if (t->pout > p0)
		t->pout = p0;
	if (t->pmark > p1)
		t->pmark -= p1-p0;
	else if (t->pmark > p0)
		t->pmark = p0;

	if ((p1 >= t->base) && (p0 < t->end))
	{
		ulong		q0 = p0, q1 = p1;

		if (q0 < t->base)
			q0 = t->base;
		if (q1 > t->end)
			q1 = t->end;

		frdelete(&t->f, q0 - t->base, q1 - t->base);
	}

	if (t->base > p1)
		t->base -= p1-p0;
	else if (t->base > p0)
		t->base = p0;

	textfill(t);
	scrollset(t->scroll, t->base, t->end-t->base, t->length);
}

void
textset(Text *t, ulong base)
{
	long		d;

	if (base > t->length)
		base = t->length;
	d = base - t->base;

	if ((d >= 0) && (d < t->f.nchars))
		frdelete(&t->f, 0, d);
	else if ((d < 0) && (-d < t->f.nchars))
		frinsert(&t->f, t->text+base, t->text+t->base, 0);
	else
		frdelete(&t->f, 0, t->f.nchars);

	t->base = base;
	textfill(t);
	scrollset(t->scroll, t->base, t->end-t->base, t->length);
}

void
textfill(Text *t)
{
	ulong		l;

	l = t->base + t->f.nchars;
	if (l < t->length)
		frinsert(&t->f, t->text+l, t->text+t->length, t->f.nchars);
	t->end = frcharofpt(&t->f, t->f.r.max) + t->base;
	texthighlight(t, t->p0, t->p1, F&~D);
	/* textshow(t, t->length, 0); */
}

void
textshow(Text *t, ulong length, ulong scroll)
{
	ulong		l;

	if (!scroll)
		return;
	if ((length < t->base) || (t->base + t->f.nchars < length))
	{
		l = _backnl(t, length, MAX(0, t->f.maxlines-3));

		if (!((l < t->base) && (length >= t->base)))
			textset(t, l);
		while (t->base + t->f.nchars < length)
			textjump(t, 3);
	}
}

void
textjump(Text *t, int lines)
{
	ulong		base;

	base = t->base + frcharofpt(&t->f,
			Pt(t->f.r.min.x, t->f.r.min.y + lines*t->f.font->height));
	textset(t, base);
}

void
textsetrects(Text *t, Rectangle r, Bitmap *b)
{
	Rectangle	area, q;
	Font		*font;

	font = t->f.font;
	frclear(&t->f);
	t->r = r;
	t->b = b;
	area = inset(r, 4);
	q = area;
	/* q.min.x += 1; */
	q.max.x = q.min.x + 12;
	area.min.x += 15;
	t->scroll->r = q;
	t->scroll->b = b;
	frinit(&t->f, area, font, b);
	bitblt(b, r.min, b, r, Zero);
	border(b, r, 1, F);
	frinsert(&t->f, t->text+t->base, t->text+t->length, 0);
	t->end = frcharofpt(&t->f, t->f.r.max) + t->base;
	scrollset(t->scroll, t->base, t->end-t->base, t->length);
	scrollupdate(t->scroll, 1);
	texthighlight(t, t->p0, t->p1, F&~D);
}

void
textscroll(Text *t, int buttons)
{
	int	n;
	Scroll *s;
	int	fwdbut, bkwdbut;

#ifdef _LIBXG_EXTENSION
	if (scrollfwdbut() == 3) {
		fwdbut = BUTTON3;
		bkwdbut = BUTTON1;
	} else {
		fwdbut = BUTTON1;
		bkwdbut = BUTTON3;
	}
#else
	fwdbut = BUTTON3;
	bkwdbut = BUTTON1;
#endif
	s = t->scroll;
	n = 0;
	if (buttons & bkwdbut) {
		s->n = s->n / t->f.font->height;
		if (!s->n)
			return;
		n = _backnl(t, t->base-1, s->n);
	} else if (buttons & BUTTON2) {
		s->n = (t->length * s->n) / Dy(inset(s->r, 1));
		if (s->n) {
			for (n = s->n; n < t->length; n++) {
				if (n > s->n+256)
					break;
				if (t->text[n] == '\n') {
					n++;
					break;
				}
			}
		}
	} else if (buttons & fwdbut) {
		s->n = t->base+frcharofpt(&t->f, Pt(s->r.min.x, s->r.min.y+s->n));
		n = s->n;
	}
	if (n < 0)
		n = 0;
	else if (n > t->length)
		n = t->length;
	textset(t, n);
}

void
textsnarf(Text *t, ulong p0, ulong p1)
{
	ulong	n;

	n = p1-p0;
	if (n <= 0)
		return;
	if (t->snarfed)
		free(t->snarfed);
	t->snarfed=(Rune *)calloc(n, sizeof(Rune));
	if (!t->snarfed)
		berror("textsnarf: calloc");
	t->snarflen = n;
	memcpy(t->snarfed, t->text+p0, n*sizeof(Rune));
}

void
textpaste(Text *t, ulong p0, ulong p1)
{
	if (!t->snarflen)
		return;
	if (p1 != p0)
		textdelete(t, p0, p1);
	textinsert(t, t->snarfed, t->snarfed+t->snarflen, p0);
}

ulong
utftotext(Rune *r, char *s1, char *s2)
{
	Rune	*q;
	char	*t;
	
	if (s2 <= s1)
		return 0;
	for (t = s1, q = r; t < s2; q++)
		if (*(uchar *)t < Runeself)
			*q = *t++;
		else
			t += chartorune(q, t);
	return t-s1;
}

ulong
texttoutf(char *s, Rune *r1, Rune *r2)
{
	Rune	*q;
	char	*t;
	
	if (r2 <= r1)
		return 0;
	for (t = s, q = r1; q < r2; q++)
		if (*(uchar *)q < Runeself)
			*t++ = *q;
		else
			t += runetochar(t, q);
	return t-s;
}
