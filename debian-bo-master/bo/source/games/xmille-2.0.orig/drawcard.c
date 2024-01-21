/*
 * drawcard.c
 *
 * display cards on the table
 */

# include	"mille.h"
# include	"ui.h"
# include	"card.h"
# include	"background.h"

struct card_init {
	char	*bits;
	char	*mask;
	int	color;
};

extern char	go_bits[], go_mask_bits[];
extern char	stop_bits[], stop_mask_bits[];
extern char	right_bits[], right_mask_bits[];
extern char	speed_bits[], speed_mask_bits[];
extern char	end_bits[], end_mask_bits[];
extern char	accident_bits[], accident_mask_bits[];
extern char	repairs_bits[], repairs_mask_bits[];
extern char	ace_bits[], ace_mask_bits[];
extern char	flat_bits[], flat_mask_bits[];
extern char	spare_bits[], spare_mask_bits[];
extern char	puncture_bits[], puncture_mask_bits[];
extern char	out_bits[], out_mask_bits[];
extern char	gas_bits[], gas_mask_bits[];
extern char	extra_bits[], extra_mask_bits[];
extern char	miles_mask_bits[];
extern char	_25_bits[], _50_bits[], _75_bits[], _100_bits[], _200_bits[];

extern char	deck_both_bits[];

struct card_init card_inits[NUM_CARDS] = {
{	_25_bits,	miles_mask_bits,	BLUE_COLOR,	},
{	_50_bits,	miles_mask_bits,	BLUE_COLOR,	},
{	_75_bits,	miles_mask_bits,	BLUE_COLOR,	},
{	_100_bits,	miles_mask_bits,	BLUE_COLOR,	},
{	_200_bits,	miles_mask_bits,	BLUE_COLOR,	},
{	out_bits,	out_mask_bits,		RED_COLOR,	},
{	flat_bits,	flat_mask_bits,		RED_COLOR,	},
{	accident_bits,	accident_mask_bits,	RED_COLOR,	},
{	stop_bits,	stop_mask_bits,		RED_COLOR,	},
{	speed_bits,	speed_mask_bits,	RED_COLOR,	},
{	gas_bits,	gas_mask_bits,		GREEN_COLOR,	},
{	spare_bits,	spare_mask_bits,	GREEN_COLOR,	},
{	repairs_bits,	repairs_mask_bits,	GREEN_COLOR,	},
{	go_bits,	go_mask_bits,		GREEN_COLOR,	},
{	end_bits,	end_mask_bits,		GREEN_COLOR,	},
{	extra_bits,	extra_mask_bits,	BLUE_COLOR,	},
{	puncture_bits,	puncture_mask_bits,	BLUE_COLOR,	},
{	ace_bits,	ace_mask_bits,		BLUE_COLOR,	},
{	right_bits,	right_mask_bits,	RED_COLOR,	},
};
extern char	deck_red_bits[], deck_blue_bits[], deck_mask_bits[];

extern char	blank_bits[];

struct card cards[NUM_CARDS];

struct card backside;
struct card eraseCard;


bw_init_cards()
{
	int	i;
	Pixmap	bits, mask;
	long	bits_p, mask_p, fill_p;

	setbuf(stderr, NULL);
	XSetFillStyle(dpy, cheap_gc, FillStippled);
	XSetFunction(dpy, cheap_gc, GXcopy);
	XSetTSOrigin(dpy, cheap_gc, 0, 0);
	
	eraseCard.bits = XCreatePixmap(dpy, xwindow, WIDTH, HEIGHT, 1);
	bits = XCreateBitmapFromData (dpy, xwindow, blank_bits, WIDTH, HEIGHT);
	bits_p = WhitePixel(dpy, screen);
	
	XSetForeground(dpy, cheap_gc, bits_p);
	XSetStipple(dpy, cheap_gc, bits);
	XFillRectangle (dpy, eraseCard.bits, cheap_gc, 0, 0, WIDTH, HEIGHT);

	XFreePixmap(dpy, bits);
	
	fill = XCreateBitmapFromData (dpy, xwindow, fill_bits, WIDTH, HEIGHT);
	fill_p = WhitePixel(dpy, screen);
	mask_p = bits_p = BlackPixel(dpy, screen);

	fprintf(stderr, "Building pixmaps, please wait ");
	
	for (i = 0; i < (NUM_CARDS - 1); i++) {
		mask = XCreateBitmapFromData (dpy, xwindow, card_inits[i].mask, WIDTH, HEIGHT);
		
		bits = XCreateBitmapFromData (dpy, xwindow, card_inits[i].bits, WIDTH, HEIGHT);

		cards[i].bits = XCreatePixmap(dpy, xwindow, WIDTH, HEIGHT, 1);
		
		XCopyArea(dpy, eraseCard.bits, cards[i].bits, cheap_gc,
			  0, 0, WIDTH, HEIGHT, 0, 0);
		
		XSetForeground(dpy, cheap_gc, fill_p);
		XSetStipple(dpy, cheap_gc, fill);
		XFillRectangle (dpy, cards[i].bits, cheap_gc, 0, 0,
				WIDTH, HEIGHT);
		
		XSetForeground(dpy, cheap_gc, bits_p);
		XSetStipple(dpy, cheap_gc, bits);
		XFillRectangle (dpy, cards[i].bits, cheap_gc, 0, 0,
				WIDTH, HEIGHT);
		
		XSetForeground(dpy, cheap_gc, mask_p);
		XSetStipple(dpy, cheap_gc, mask);
		XFillRectangle (dpy, cards[i].bits, cheap_gc, 0, 0,
				WIDTH, HEIGHT);
		XFreePixmap(dpy, mask);
		XFreePixmap(dpy, bits);
		fprintf(stderr, ". ");
	}
	backside.bits = XCreatePixmap(dpy, xwindow, WIDTH, HEIGHT, 1);

	bits = XCreateBitmapFromData (dpy, xwindow, deck_both_bits,
				      WIDTH, HEIGHT);
	bits_p = BlackPixel(dpy, screen);

	XSetForeground(dpy, cheap_gc, fill_p);
	XSetStipple(dpy, cheap_gc, fill);
	XFillRectangle (dpy, backside.bits, cheap_gc, 0, 0, WIDTH, HEIGHT);
		
	XSetForeground(dpy, cheap_gc, bits_p);
	XSetStipple(dpy, cheap_gc, bits);
	XFillRectangle (dpy, backside.bits, cheap_gc, 0, 0, WIDTH, HEIGHT);

	XFreePixmap(dpy, bits);
	fprintf(stderr, ". done\n");
}
init_cards ()
{
	int	i;
	Pixmap	bits, mask, bits1, bits2;
	long	bits_p, mask_p, fill_p, bits1_p, bits2_p;

	setbuf(stderr, NULL);
	XSetFillStyle(dpy, cheap_gc, FillStippled);
	XSetFunction(dpy, cheap_gc, GXcopy);
	XSetTSOrigin(dpy, cheap_gc, 0, 0);
	
	eraseCard.bits = XCreatePixmap(dpy, xwindow, WIDTH, HEIGHT,
				       DefaultDepth(dpy, screen));
	bits = XCreateBitmapFromData (dpy, xwindow, blank_bits, WIDTH, HEIGHT);
	bits_p = colorMap[GREY_COLOR].pixel;
	
	XSetForeground(dpy, cheap_gc, bits_p);
	XSetStipple(dpy, cheap_gc, bits);
	XFillRectangle (dpy, eraseCard.bits, cheap_gc, 0, 0, WIDTH, HEIGHT);

	XFreePixmap(dpy, bits);
	
	fill = XCreateBitmapFromData (dpy, xwindow, fill_bits, WIDTH, HEIGHT);
	fill_p = colorMap[WHITE_COLOR].pixel;

	fprintf(stderr, "Building pixmaps, please wait ");
	
	for (i = 0; i < (NUM_CARDS - 1); i++) {
		mask = XCreateBitmapFromData (dpy, xwindow, card_inits[i].mask, WIDTH, HEIGHT);
		mask_p = colorMap[card_inits[i].color].pixel;
		
		bits = XCreateBitmapFromData (dpy, xwindow, card_inits[i].bits, WIDTH, HEIGHT);
		bits_p = colorMap[BLACK_COLOR].pixel;

		cards[i].bits = XCreatePixmap(dpy, xwindow, WIDTH, HEIGHT,
					      DefaultDepth(dpy, screen));

		XCopyArea(dpy, eraseCard.bits, cards[i].bits, cheap_gc,
			  0, 0, WIDTH, HEIGHT, 0, 0);
		
		XSetForeground(dpy, cheap_gc, fill_p);
		XSetStipple(dpy, cheap_gc, fill);
		XFillRectangle (dpy, cards[i].bits, cheap_gc, 0, 0,
				WIDTH, HEIGHT);
		
		XSetForeground(dpy, cheap_gc, bits_p);
		XSetStipple(dpy, cheap_gc, bits);
		XFillRectangle (dpy, cards[i].bits, cheap_gc, 0, 0,
				WIDTH, HEIGHT);
		
		XSetForeground(dpy, cheap_gc, mask_p);
		XSetStipple(dpy, cheap_gc, mask);
		XFillRectangle (dpy, cards[i].bits, cheap_gc, 0, 0,
				WIDTH, HEIGHT);
		
		XFreePixmap(dpy, mask);
		XFreePixmap(dpy, bits);
		fprintf(stderr, ". ");
	}
	backside.bits = XCreatePixmap(dpy, xwindow, WIDTH, HEIGHT,
				      DefaultDepth(dpy, screen));

	bits = XCreateBitmapFromData (dpy, xwindow, deck_red_bits,
				      WIDTH, HEIGHT);
	bits_p = colorMap[RED_COLOR].pixel;

	bits1 = XCreateBitmapFromData (dpy, xwindow, deck_blue_bits,
				      WIDTH, HEIGHT);
	bits1_p = colorMap[BLUE_COLOR].pixel;

	bits2 = XCreateBitmapFromData (dpy, xwindow, deck_mask_bits,
				      WIDTH, HEIGHT);
	bits2_p = colorMap[BLACK_COLOR].pixel;

	fill_p = colorMap[GREEN_COLOR].pixel;
	
	XCopyArea(dpy, eraseCard.bits, backside.bits, cheap_gc,
		  0, 0, WIDTH, HEIGHT, 0, 0);
		
	XSetForeground(dpy, cheap_gc, fill_p);
	XSetStipple(dpy, cheap_gc, fill);
	XFillRectangle (dpy, backside.bits, cheap_gc, 0, 0, WIDTH, HEIGHT);
		
	XSetForeground(dpy, cheap_gc, bits2_p);
	XSetStipple(dpy, cheap_gc, bits2);
	XFillRectangle (dpy, backside.bits, cheap_gc, 0, 0, WIDTH, HEIGHT);

	XSetForeground(dpy, cheap_gc, bits1_p);
	XSetStipple(dpy, cheap_gc, bits1);
	XFillRectangle (dpy, backside.bits, cheap_gc, 0, 0, WIDTH, HEIGHT);

	XSetForeground(dpy, cheap_gc, bits_p);
	XSetStipple(dpy, cheap_gc, bits);
	XFillRectangle (dpy, backside.bits, cheap_gc, 0, 0, WIDTH, HEIGHT);

	XFreePixmap(dpy, bits);
	XFreePixmap(dpy, bits1);
	XFreePixmap(dpy, bits2);
	fprintf(stderr, ". done\n");
}

displayCard (card, x, y)
int	card;
int	x, y;
{
	if (card < 0 || card >= NUM_CARDS) {
		cardDisplay (&eraseCard, x, y);
	} else {
		cardDisplay (&cards[card], x, y);
	}
}

struct displayed {
	struct displayed	*next;
	struct card		*card;
	int			x, y;
	int			flag;
};

static struct displayed	*onscreen;

cardDisplay (c, x, y)
struct card	*c;
{
	struct displayed	*d, *p;
	char			*malloc ();

	p = 0;
	for (d = onscreen; d; d = d->next) {
		if (d->x == x && d->y == y) {
			if (d->card == c)
				return;
			if (p) {
				p->next = d->next;
				d->next = onscreen;
				onscreen = d;
			}
			goto gotim;
		}
		p = d;
	}
	d = (struct displayed *) malloc (sizeof (struct displayed));
	d->x = x;
	d->y = y;
	if (p)
		p->next = d;
	else
		onscreen = d;
	d->next = 0;
gotim:	;
	d->card = c;
	drawIm (c, x, y);
}

static
drawIm (c, x, y)
struct card	*c;
int		x, y;
{
	XSetFunction(dpy, cheap_gc, GXcopy);
	XCopyArea(dpy, c->bits, xwindow, cheap_gc, 0, 0, WIDTH, HEIGHT,
		  x, y);
}

cardRedisplay (x, y, w, h)
{
	struct displayed	*d;

	for (d = onscreen; d; d = d->next)
		d->flag = 0;
	redisplaybelow (onscreen, x, y, w, h);
}

static
redisplaybelow (d, x, y, w, h)
struct displayed	*d;
{
	int			x2, y2;

	x2 = x + w;
	y2 = y + h;
	for (; d; d = d->next) {
		if ((d->x <= x2 && x <= (d->x + WIDTH)) &&
		    (d->y <= y2 && y <= (d->y + HEIGHT)))
 		{
		    	if (d->flag == 0) {
				drawIm (d->card, d->x, d->y);
				d->flag = 1;
				redisplaybelow (d->next, d->x, d->y, WIDTH, HEIGHT);
			}
		}
	}

}

cardEraseAll ()
{
	struct displayed	*d, *n;

	for (d = onscreen; d; d = n) {
		n = d->next;
		free (d);
	}
	onscreen = 0;
}
