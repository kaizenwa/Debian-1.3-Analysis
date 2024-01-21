#include <u.h>
#include <libc.h>
#include <libg.h>
#include <frame.h>
#include <text.h>

static Bitmap	*_dkgrey;

#define MAX(x, y) ((x) > (y) ? (x) : (y))

static
Rectangle
_getthumb(Scroll *s)
{
	Rectangle	r;

	r = inset(s->r, 1);
	if (s->extent < s->max)
	{
		r.min.y += (Dy(s->r)*s->thumb)/s->max;
		r.max.y = r.min.y + MAX(2, (Dy(s->r)*s->extent)/s->max);
	}
	return r;
}

void
scrollupdate(Scroll *s, int force)
{
	Rectangle	r, q, inside;
	Bitmap		*image;
	
	if (eqrect(q = _getthumb(s), s->_q) && !force)
		return;
	s->_q = q;
	r = s->r;
#if 0
	image = balloc(r, 0);
#else
	image = s->b;
#endif
	border(image, r, 1, F);
	inside = inset(r, 1);
	if (s->extent < s->max)
	{
		if (!_dkgrey)
		{
			_dkgrey = balloc(Rect(0, 0, 4, 4), 0);
			bitblt(_dkgrey, _dkgrey->r.min, _dkgrey, _dkgrey->r, F);
			point(_dkgrey, Pt(0, 0), 0, Zero);
			point(_dkgrey, Pt(0, 2), 0, Zero);
			point(_dkgrey, Pt(2, 1), 0, Zero);
			point(_dkgrey, Pt(2, 3), 0, Zero);
		}
		texture(image, inside, _dkgrey, S);
		if (rectclip(&q, inside))
			bitblt(image, q.min, image, q, Zero);
	} else {
		bitblt(image, inside.min, image, inside, Zero);
	}
#if 0
	bitblt(s->b, image->r.min, image, image->r, S);
	bfree(image);
	bflush();
#endif
}

Scroll *
scrollalloc(Bitmap *b, Rectangle r)
{
	Scroll	*s;

	s = (Scroll *)malloc(sizeof(Scroll));
	s->b = b;
	s->data = (char *)0;
	s->r = r;
	s->thumb = s->extent = s->max = 0;
	s->_q = Rect(0, 0, 0, 0);
	scrollset(s, 0, 0, 0);

	return s;
}

void
scrollfree(Scroll *scroll)
{
	free(scroll);
}

void
scrollset(Scroll *s, ulong thumb, ulong extent, ulong max)
{
	s->thumb = thumb;
	s->extent = extent;
	s->max = max;

	scrollupdate(s, 0);
}

ulong
scrollhit(Scroll *s, Event *e)
{
	Rectangle	r, q, t, inside;
	ulong		buttons;
	long		x, y;
	int		inscroll;
	int		fwdbut, bkwdbut;

	if (e->mouse.buttons)
	{
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
		buttons = e->mouse.buttons;
		inside = inset(s->r, 1);
		t = _getthumb(s);
		r = t;
		q = r;
		if (rectclip(&q, inside))
				bitblt(s->b, q.min, s->b, q, ~D);
		inscroll = 1;
		while (e->mouse.buttons == buttons)
		{
			x = e->mouse.xy.x;
			y = e->mouse.xy.y;
			if (s->r.min.x <= x && x <= s->r.max.x) {
				if (!inscroll) {
					r = _getthumb(s);
					inscroll = 1;
				} else {
					q = r;
					if (rectclip(&q, inside))
						bitblt(s->b, q.min, s->b, q, ~D);
				}
				if (buttons & BUTTON2)
				{
					if (y < inside.min.y)
						y = inside.min.y;
					if (y > inside.max.y-2)
						y = inside.max.y-2;
					r.min.y = t.min.y+y-t.min.y;
					r.max.y = t.max.y+y-t.min.y;
					if (r.min.y < inside.min.y)
						r.min.y = inside.min.y;
					if (r.max.y > inside.max.y)
						r.max.y = inside.max.y;
				}
				else if (buttons & (BUTTON1 | BUTTON3))
				{
					int		o;
	
						o = (Dy(t)*(y - inside.min.y))/Dy(inside);
					if (buttons & fwdbut)
						r = raddp(t, Pt(0, o));
					else
						r = rsubp(t, Pt(0, o));
				}
				q = r;
				if (rectclip(&q, inside))
					bitblt(s->b, q.min, s->b, q, ~D);
			} else if (inscroll) {
				q = r;
				if (rectclip(&q, inside))
					bitblt(s->b, q.min, s->b, q, ~D);
				inscroll = 0;
			}
			eread(Emouse, e);
		}
		if (inscroll) {
			q = r;
			if (rectclip(&q, inside))
				bitblt(s->b, q.min, s->b, q, ~D);
		} else
			return 0;
		s->buttons = buttons;
		if (y > inside.max.y)
			y = inside.max.y;
		else if (y < inside.min.y)
			y = inside.min.y;
		s->n = y - inside.min.y;
		return 1;
	}
	return 0;
}
