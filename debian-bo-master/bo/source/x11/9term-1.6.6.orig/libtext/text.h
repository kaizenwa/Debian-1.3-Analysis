#ifndef _LIBTEXT_H
#define _LIBTEXT_H

/*
 * Types...
 */

typedef struct Button	Button;
typedef struct Scroll	Scroll;
typedef struct Text	Text;

struct Button {
	Rectangle		r;
	Bitmap			*b;
	char			*label;
	void			*data;
};

struct Scroll {
	Rectangle		r;
	Bitmap			*b;
	ulong			thumb;
	ulong			extent;
	ulong			max;
	ulong			buttons;
	ulong			n;
	Rectangle		_q;
	void			*data;
};

struct Text {
	Rectangle		r;
	Bitmap			*b;
	Frame			f;
	Rune			*text;
	ulong			alloced;
	ulong			length;
	ulong			base;
	ulong			end;
	ulong			p0;
	ulong			p1;
	ulong			pout;
	ulong			pmark;
	uchar			modified;
	uchar			scrolling;
	Scroll			*scroll;
	Rune			*snarfed;
	ulong			snarflen;
	void			*data;
};

/*
 * Things...
 */

#define	BUTTON1			1
#define	BUTTON2			2
#define	BUTTON3			4

/*
 * Functions...
 */

void			scrollupdate(Scroll *, int);
Scroll			*scrollalloc(Bitmap *, Rectangle);
void			scrollfree(Scroll *);
void			scrollset(Scroll *, ulong, ulong, ulong);
ulong			scrollhit(Scroll *, Event *);

Text			*textalloc(Bitmap *, Rectangle, Font *);
void			textfree(Text *);
void			texthit(Text *, Event *, ulong);
void			textinsert(Text *, Rune *, Rune *, ulong);
void			textinsertchar(Text *, uchar *, uchar *, ulong);
void			textselect(Text *, Mouse *);
void			texthighlight(Text *, ulong, ulong, Fcode);
void			textdelete(Text *, ulong, ulong);
void			textset(Text *, ulong);
void			textfill(Text *);
void			textshow(Text *, ulong, ulong);
void			textjump(Text *, int);
void			textsetrects(Text *, Rectangle, Bitmap *);
void			textscroll(Text *, int);
void			textsnarf(Text *, ulong, ulong);
void			textpaste(Text *, ulong, ulong);

void			doubleclick(Text *, ulong);
ulong			_backnl(Text *, long, ulong);

ulong			texttoutf(char *, Rune *, Rune *);
ulong			utftotext(Rune *, char *, char *);

#endif
