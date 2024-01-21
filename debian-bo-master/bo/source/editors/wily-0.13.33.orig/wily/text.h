/*
 * The Text is the non-visible counterpart to the View.  It maintains
 * the internal data structure of the whole file, but has no knowledge
 * about how to display things.  The Text is also responsible for
 * maintaining Undo information.
 *
 * There may be one or more Views of a particular piece of text.
 */
struct Text {
	Rune	*text;		/* where we store stuff */
	ulong	alloced,length;
	Range	gap;			/* buffer gap */
	Data		*data;		/* 0 for wilytag or columntag */
	Bool		isbody;
	View		*v;			/* list of views of this text */
	Bool		needsbackup;	/* this text can become dirty */

	ulong	pos;			/* position for regexp search engine */

	Undo	*did,*undone, *mark;
	enum {NoUndo, StartUndo, MoreUndo} undoing;
};
#define TEXT_ISTAG(t) (	!(t)->isbody	)
#define TEXT_ISWINTAG(t) ((t)->v && (t)->v->win && TEXT_ISTAG(t))
#define TCHAR(t,p) ((p)<(t)->gap.p0) ? (t)->text[(p)] : (t)->text[(p) + RLEN((t)->gap)]


/* Get next rune, going forwards/backwards */
#define	Tgetc(t)  ( ((t)->pos<(t)->gap.p0) ? ((t)->text[(t)->pos++] ) : (((t)->pos < (t)->length) ? (t)->text[(t)->pos++ + RLEN((t)->gap)] : -1))

#define Tbgetc(tp) ( ((tp)->pos > (tp)->gap.p0) ? \
	(tp)->text[--(tp)->pos + RLEN((tp)->gap)] : \
	( (tp)->pos ? (tp)->text[--(tp)->pos] : -1) )

#define Tbgetcset(t,p) ( (t)->pos = (p))
#define Tgetcset(t,p) ((t)->pos = (p))
