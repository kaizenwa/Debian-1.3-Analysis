/*
 * Prototypes for run-time functions.
 */

/*
 * Prototypes common to the compiler and interpreter.
 */

int	interp	Params((int fsig,dptr cargp));
novalue	EVInit	Params((noargs));
word	add	Params((word a,word b));
word	mul	Params((word a,word b));
word	sub	Params((word a,word b));
int	dp_pnmcmp	Params((struct pstrnm *pne,dptr dp));
int	getvar	Params((char *s,dptr vp));
word	neg	Params((word a));
int	pstrnmcmp	Params((struct pstrnm *a,struct pstrnm *b));
novalue	xmfree	Params((noargs));
novalue	icon_init	Params((char *name, int *argcp, char *argv[]));
novalue	inttrap	Params((noargs));
char	*alcstr			Params((char *s,word slen));
char	*findfile		Params((word *ipc));
char	*reserve		Params((int region, word nbytes));
int	activate	Params((dptr val, struct b_coexpr *ncp, dptr result));
int	anycmp			Params((dptr dp1,dptr dp2));
int	bfunc			Params((noargs));
int	co_chng	Params((struct b_coexpr *ncp, struct descrip *valloc, struct descrip *rsltloc, int swtch_typ, int first));
int	cnv_c_dbl		Params((dptr s, double *d));
int	cnv_c_int		Params((dptr s, C_integer *d));
int	cnv_c_str		Params((dptr s, dptr d));
int	cnv_cset		Params((dptr s, dptr d));
int	cnv_ec_int		Params((dptr s, C_integer *d));
int	cnv_eint		Params((dptr s, dptr d));
int	cnv_int			Params((dptr s, dptr d));
int	cnv_real		Params((dptr s, dptr d));
int	cnv_str			Params((dptr s, dptr d));
int	cnv_tcset		Params((struct b_cset *cbuf, dptr s, dptr d));
int	cnv_tstr		Params((char *sbuf, dptr s, dptr d));
int	coswitch		Params((word *old, word *new, int first));
int	cplist			Params((dptr dp1,dptr dp2,word i,word j));
novalue	cpslots			Params((dptr dp1,dptr slotptr,word i, word j));
int	cpset			Params((dptr dp1,dptr dp2,word size));
int	csetcmp			Params((unsigned int *cs1,unsigned int *cs2));
int	cssize			Params((dptr dp));
int	def_c_dbl		Params((dptr s, double df, double * d));
int	def_c_int		Params((dptr s, C_integer df, C_integer * d));
int	def_c_str		Params((dptr s, char * df, dptr d));
int	def_cset		Params((dptr s, struct b_cset * df, dptr d));
int	def_ec_int		Params((dptr s, C_integer df, C_integer * d));
int	def_eint		Params((dptr s, C_integer df, dptr d));
int	def_int			Params((dptr s, C_integer df, dptr d));
int	def_real		Params((dptr s, double df, dptr d));
int	def_str			Params((dptr s, dptr df, dptr d));
int	def_tcset Params((struct b_cset *cbuf,dptr s,struct b_cset *df,dptr d));
int	def_tstr		Params((char *sbuf, dptr s, dptr df, dptr d));
int	doasgn			Params((dptr dp1,dptr dp2));
int	doimage			Params((int c,int q));
int	equiv			Params((dptr dp1,dptr dp2));
int	err			Params((noargs));
int	findipc			Params((int line));
int	findline		Params((word *ipc));
int	getstrg			Params((char *buf,int maxi,FILE *fd));
int	lexcmp			Params((dptr dp1,dptr dp2));
int	numcmp			Params((dptr dp1,dptr dp2,dptr dp3));
int	pushact		Params((struct b_coexpr *ce, struct b_coexpr *actvtr));
int	putstr			Params((FILE *f,dptr d));
int	qtos	Params((dptr dp,char *sbuf));
int     radix	Params((int sign, register int r, register char *s,
                       register char *end_s, union numeric *result));
novalue	addmem 	Params((struct b_set *ps,struct b_selem *pe, union block **pl));
novalue	c_exit			Params((int i));
novalue	co_init			Params((struct b_coexpr *sblkp));
novalue	coacttrace	Params((struct b_coexpr *ccp,struct b_coexpr *ncp));
novalue	cofailtrace	Params((struct b_coexpr *ccp,struct b_coexpr *ncp));
novalue	corettrace	Params((struct b_coexpr *ccp,struct b_coexpr *ncp));
novalue	datainit		Params((noargs));
novalue	deallocate		Params((union block *bp));
novalue	drunerr			Params((int n, double v));
novalue	env_int	Params((char *name,word *variable,int non_neg, uword limit));
novalue	error			Params((char *s1, char *s2));
novalue	err_msg			Params((int n, dptr v));
novalue	fatalerr		Params((int n,dptr v));
novalue	fpetrap			Params((noargs));
novalue	iconhost		Params((char *hostname));
novalue	irunerr			Params((int n, C_integer v));
novalue	mksubs	Params((dptr var,dptr val,word i,word j, dptr result));
novalue	outimage		Params((FILE *f,dptr dp,int restrict));
#ifdef MultiThread
novalue	resolve			Params((struct progstate *pstate));
#else					/* MultiThread */
novalue	resolve			Params((noargs));
#endif					/* MultiThread */
novalue retderef		Params((dptr valp, word *low, word *high));
novalue	segvtrap		Params((noargs));
novalue	stkdump			Params((int));
novalue	syserr			Params((char *s));
struct	astkblk *alcactiv	Params((noargs));
struct	b_coexpr *popact	Params((struct b_coexpr *ce));
struct	b_coexpr *topact	Params((struct b_coexpr *ce));
struct	b_cset *alccset		Params((noargs));
struct	b_file *alcfile		Params((FILE *fd,int status,dptr name));
struct	b_lelem *alclstb	Params((uword nslots,uword first,uword nused));
struct	b_list *alclist		Params((uword size));
struct	b_real *alcreal		Params((double val));
struct	b_selem *alcselem	Params((dptr mbr,uword hn));
struct	b_slots *alcsegment	Params((word nslots));
struct	b_telem *alctelem	Params((noargs));
struct	b_tvtbl *alctvtbl	Params((dptr tbl,dptr ref,uword hashnum));
struct	b_proc	*bi_strprc	Params((dptr s, C_integer arity));
union block *alchash		Params((int tcode));
union block *hgfirst	Params((union block *bp, struct hgstate *state));
union block *hgnext	Params((union block*b,struct hgstate*s,union block *e));
union block *hmake	Params((int tcode,word nslots,word nelem));
union block **memb	Params((union block *pb,dptr x,uword hn, int *res));
union block **hchain		Params((union block *pb,uword hn));
uword	hash			Params((dptr dp));
word	cvpos			Params((long pos,long len));
word	longread		Params((char *s,int width,long len,FILE *fname));
word	prescan			Params((dptr d));

int	c_get		Params((struct b_list *hp, struct descrip *res));
void	c_put		Params((struct descrip *l, struct descrip *val));


#if !HIGHC_386
int	dup2		Params((int h1, int h2));
#endif					/* !HIGHC_386 */

char *qsearch	Params((char *key,char *base,int nel,int width, int (*cmp)()));

/*
 * Temporary fix
 */

#if ARM
FILE *popen
#endif					/* ARM */

#if ATARI_ST
char	*sbrk			Params((int incr));
#endif                                  /* ATARI_ST */

#if HIGHC_386
int	brk		Params((char *p));
#endif					/* HIGHC_386 */

#if MACINTOSH
#if MPW
char	*brk			Params((char *addr));
char	*sbrk			Params((int incr));
novalue	free			Params((char* addr));
#endif					/* MPW */
#endif					/* MACINTOSH */

#if MVS || VM
#if SASC
#define brk(x) sbrk(((char *)(x))-sbrk(0))
char    *sbrk                   Params((int incr));
#endif					/* SASC */
novalue free                    Params((void* addr));
#endif                                  /* MVS || VM */

#if UNIX || VMS
/*
 * We use old-style declarations instead of prototypes here to avoid conflicts
 *  with system definitions.  For example, some <stdio.h> files don't declare
 *  popen; some declare it with char * args; and some specify const char *.
 */
int	chdir	();
FILE	*popen	();
#ifdef ExecImages
char	*sbrk	();
#endif					/* ExecImages */
#endif                                  /* UNIX || VMS */

#ifdef Coexpr
novalue new_context		Params((int fsig, dptr cargp));
#endif					/* Coexpr */


#ifdef Graphics
/*
 * portable graphics routines in rwindow.r and rwinrsc.r
 */
wcp	alc_context		Params((wbp w));
wbp	alc_wbinding		Params((noargs));
wsp	alc_winstate		Params((noargs));
int	atobool			Params((char *s));
int	docircles		Params((wbp w, int argc, dptr argv, int fill));
novalue	drawCurve		Params((wbp w, XPoint *p, int n));
char	*evquesub		Params((wbp w, int i));
wsp	getactivewindow		Params((void));
novalue	genCurve		Params((wbp w, XPoint *p, int n, void (*h)()));
int	getpattern		Params((wbp w, char *answer));
novalue	mystrncpy		Params((char *dest,char *src,int n));
struct	palentry *palsetup	Params((int p));
int	palnum			Params((dptr d));
int	parsecolor		Params((wbp w, char *s, long *r, long *g, long *b));
int	parsefont		Params((char *s, char *fam, int *sty, int *sz));
int	parsegeometry		Params((char *buf, SHORT *x, SHORT *y, SHORT *width, SHORT *height));
int	parsepattern		Params((char *s, int len, int *width, int *nbits, C_integer *bits));
novalue	qevent			Params((wsp ws, dptr e, int x, int y, uword t, long f));
int	readGIF			Params((char *fname, int p, struct imgdata *d));
int	rectargs		Params((wbp w, int argc, dptr argv, int i,
				   word *px, word *py, word *pw, word *ph));
char	*rgbkey			Params((int p, double r, double g, double b));
int	writeGIF		Params((wbp w, char *filename,
				  int x, int y, int width, int height));
int	setsize			Params((wbp w, char *s));
char	*si_i2s			Params((siptr sip, int i));
int	si_s2i			Params((siptr sip, char *s));
char	*strnchr		Params((char *s, int ci, long len));
int	ulcmp			Params((pointer p1, pointer p2));
int	wattrib			Params((wbp w, char *s, long len, dptr answer, char *abuf));
int	wgetche			Params((wbp w, dptr res));
int	wgetchne		Params((wbp w, dptr res));
int	wgetevent		Params((wbp w, dptr res));
int	wgetstrg		Params((char *s, long maxlen, FILE *f));
novalue	wgoto			Params((wbp w, int row, int col));
int	wlongread		Params((char *s, int elsize, int nelem, FILE *f));
novalue	wputstr			Params((wbp w, char *s, int len));
int	xyrowcol		Params((dptr dx));

/*
 * graphics implementation routines supplied for each platform
 * (excluding those defined as macros for X-windows)
 */
int	SetPattern		Params((wbp w, char *name, int len));
int	SetPatternBits		Params((wbp w, int width, C_integer *bits, int nbits));
int	allowresize		Params((wbp w, int on));
int	blimage			Params((wbp w, int x, int y, int width, int hgt,
				  int ch, unsigned char *s, word len));
wcp	clone_context		Params((wbp w));
int	copyArea		Params((wbp w, wbp w2, int x, int y, int width, int height, int x2, int y2));
int	do_config		Params((wbp w, int status));
int	dumpimage		Params((wbp w, char *filename, unsigned int x, unsigned int y, unsigned int width, unsigned int height));
novalue	eraseArea		Params((wbp w, int x, int y, int width, int height));
novalue	fillrectangles		Params((wbp w, XRectangle *recs, int nrecs));
novalue	free_binding		Params((wbp w));
novalue	free_context		Params((wcp wc));
novalue	free_mutable		Params((wbp w, int mute_index));
int	free_window		Params((wsp ws));
novalue	freecolor		Params((wbp w, char *s));
char	*get_mutable_name	Params((wbp w, int mute_index));
novalue	getbg			Params((wbp w, char *answer));
novalue	getcanvas		Params((wbp w, char *s));
int	getdefault		Params((wbp w, char *prog, char *opt, char *answer));
novalue	getdisplay		Params((wbp w, char *answer));
novalue	getdrawop		Params((wbp w, char *answer));
novalue	getfg			Params((wbp w, char *answer));
novalue	getfntnam		Params((wbp w, char *answer));
novalue	geticonic		Params((wbp w, char *answer));
int	geticonpos		Params((wbp w, char *s));
int	getimstr		Params((wbp w, int x, int y, int width, int hgt,
				  struct palentry *ptbl, unsigned char *data));
novalue	getlinestyle		Params((wbp w, char *answer));
int	getpixel_init		Params((wbp w, int x, int y, int width, int height));
int	getpixel		Params((wbp w, int x, int y, long *rv, char *s));
novalue	getpointername		Params((wbp w, char *answer));
int	getpos			Params((wbp w));
int	getvisual		Params((wbp w, char *answer));
int	isetfg			Params((wbp w, int fg));
int	isetbg			Params((wbp w, int bg));
int	lowerWindow		Params((wbp w));
int	mutable_color		Params((wbp w, dptr argv, int ac, int *retval));
int	nativecolor		Params((wbp w, char *s, long *r, long *g, long *b));

#ifndef PresentationManager
/* Exclude those functions defined as macros */
int	pollevent		Params((noargs));
novalue wflush			Params((wbp w));
#endif					/* PresentationManager */

int	query_pointer		Params((wbp w, XPoint *pp));
int	query_rootpointer	Params((XPoint *pp));
int	raiseWindow		Params((wbp w));
int	readimage		Params((wbp w, char *filename, int x, int y, int *status));
int	rebind			Params((wbp w, wbp w2));
int	set_mutable		Params((wbp w, int i, char *s));
int	setbg			Params((wbp w, char *s));
int	setcanvas		Params((wbp w, char *s));
novalue	setclip			Params((wbp w));
int	setcursor		Params((wbp w, int on));
int	setdisplay		Params((wbp w, char *s));
int	setdrawop		Params((wbp w, char *val));
int	setfg			Params((wbp w, char *s));
int	setfillstyle		Params((wbp w, char *s));
int	setfont			Params((wbp w, char **s));
int	setgamma		Params((wbp w, double gamma));
int	setgeometry		Params((wbp w, char *geo));
int	setheight		Params((wbp w, SHORT new_height));
int	seticonicstate		Params((wbp w, char *s));
int	seticonlabel		Params((wbp w, char *val));
int	seticonpos		Params((wbp w, char *s));
int	setimage		Params((wbp w, char *val));
int	setleading		Params((wbp w, int i));
int	setlinestyle		Params((wbp w, char *s));
int	setlinewidth		Params((wbp w, LONG linewid));
int	setpointer		Params((wbp w, char *val));
int	setwidth		Params((wbp w, SHORT new_width));
int	setwindowlabel		Params((wbp w, char *val));
int	strimage		Params((wbp w, int x, int y, int width,
				  int height, struct palentry *e,
				  unsigned char *s, word len, int on_icon));
novalue	toggle_fgbg		Params((wbp w));
int	walert			Params((wbp w, int volume));
novalue	warpPointer		Params((wbp w, int x, int y));
int	wclose			Params((wbp w));
novalue	wflush			Params((wbp w));
int	wgetq			Params((wbp w, dptr res));
FILE	*wopen			Params((char *windowname, struct b_list *hp, dptr attr, int n, int *err_index));
int	wputc			Params((int ci, wbp w));
novalue	wsync			Params((wbp w));
novalue	xdis			Params((wbp w, char *s, int n));
#ifdef ConsoleWindow
FILE* OpenConsole		Params((void));
int   Consolefprintf		(FILE *file, char *format, ...);
int   Consoleputc		(int c, FILE *file);
int   Consolefflush		(FILE *file);
#endif					/* ConsoleWindow */

#ifdef MacGraph
/*
 * Implementation routines specific to Macintosh
 */
void hidecrsr (wsp ws);
void showcrsr (wsp ws);
void UpdateCursorPos(wsp ws, wcp wc);
void GetEvents (void);
void DoEvent (EventRecord *eventPtr);
void DoMouseUp (EventRecord *eventPtr);
void DoMouseDown (EventRecord *eventPtr);
void DoGrowWindow (EventRecord *eventPtr, WindowPtr whichWindow);
void GetLocUpdateRgn (WindowPtr whichWindow, RgnHandle localRgn);
void DoKey (EventRecord *eventPtr, WindowPtr whichWindow);
void HandleMenuChoice (long menuChoice);
void HandleAppleChoice (short item);
void HandleFileChoice (short item);
void HandleOptionsChoice (short item);
void DoUpdate (EventRecord *eventPtr);
void DoActivate (WindowPtr whichWindow, Boolean becomingActive);
void RedrawWindow (WindowPtr whichWindow);
/*void setfile(char *filename, OSType type, OSType creator);*/
const int ParseCmdLineStr (char *s, char *t, char **argv);
pascal OSErr SetDialogDefaultItem (DialogPtr theDialog, short newItem) = { 0x303C, 0x0304, 0xAA68 };
pascal OSErr SetDialogCancelItem (DialogPtr theDialog, short newItem) = { 0x303C, 0x0305, 0xAA68 };
pascal OSErr SetDialogTracksCursor (DialogPtr theDialog, Boolean tracks) = { 0x303C, 0x0306, 0xAA68 };
#endif					/* MacGraph */

#ifdef XWindows
/*
 * Implementation routines specific to X-Windows
 */
novalue	unsetclip		Params((wbp w));
novalue	moveWindow		Params((wbp w, int x, int y));
int	moveResizeWindow	Params((wbp w, int x, int y, int width, int height));
int	resetfg			Params((wbp w));
int	setfgrgb		Params((wbp w, int r, int g, int b));
int	setbgrgb		Params((wbp w, int r, int g, int b));

XColor	xcolor			Params((wbp w, LinearColor clr));
LinearColor	lcolor		Params((wbp w, XColor color));
int	pixmap_open		Params((wbp w, dptr attribs, int argc));
int	pixmap_init		Params((wbp w));
int	remap			Params((wbp w, int x, int y));
int	seticonimage		Params((wbp w, dptr dp));
novalue	makeIcon		Params((wbp w, int x, int y));
int	translate_key_event	Params((XKeyEvent *k1, char *s, KeySym *k2));
int	handle_misc		Params((wdp display, wbp w));
wdp	alc_display		Params((char *s));
novalue	free_display		Params((wdp wd));
wfp	alc_font		Params((wbp w, char **s));
wfp	tryfont			Params((wbp w, char *s));
wclrp	alc_rgb			Params((wbp w, char *s, unsigned int r, unsigned int g, unsigned int b, int is_iconcolor));
int	alc_centry		Params((wdp wd));
wclrp	alc_color		Params((wbp w, char *s));
novalue	copy_colors		Params((wbp w1, wbp w2));
novalue	free_xcolor		Params((wbp w, unsigned long c));
novalue	free_xcolors		Params((wbp w, int extent));
int	go_virtual		Params((wbp w));
int	resizePixmap		Params((wbp w, int width, int height));
#endif					/* XWindows */

#ifdef MSWindows
/*
 * Implementation routines specific to MS Windows
 */
int mswinsystem		Params((char *s));
void UpdateCursorPos	Params((wsp ws, wcp wc));
LRESULT_CALLBACK WndProc	Params((HWND, UINT, WPARAM, LPARAM));
HDC CreateWinDC		Params((wbp));
HDC CreatePixDC		Params((wbp, HDC));
HBITMAP loadimage	Params((wbp wb, char *filename, unsigned int *width,
			unsigned int *height, int atorigin, int *status));
void wfreersc();
int getdepth(wbp w);
HBITMAP CreateBitmapFromData(char *data);
int resizePixmap(wbp w, int width, int height);
int textWidth(wbp w, char *s, int n);
int	seticonimage		Params((wbp w, dptr dp));
int devicecaps(wbp w, int i);
void fillarcs(wbp wb, XArc *arcs, int narcs);
void drawarcs(wbp wb, XArc *arcs, int narcs);
void drawlines(wbinding *wb, XPoint *points, int npoints);
void drawpoints(wbinding *wb, XPoint *points, int npoints);
void drawrectangles(wbp wb, XRectangle *recs, int nrecs);
void fillpolygon(wbp w, XPoint *pts, int npts);
void drawsegments(wbinding *wb, XSegment *segs, int nsegs);
void drawstrng(wbinding *wb, int x, int y, char *s, int slen);
novalue unsetclip(wbp w);

#endif					/* MSWindows */

#ifdef PresentationManager
/*
 * Implementation routines specific to OS/2 Presentation Manager
 */
wsp ObtainEvents(wsp ws, SHORT blockflag, ULONG messg, QMSG *msg);
void InterpThreadStartup(void *args);
void InterpThreadShutdown(void);
void DestroyWindow(wsp ws);
void LoadDefAttrs(wbinding *wb, wsp ws, wcp wc);
void ResizeBackingBitmap(wsp ws, SHORT x, SHORT y);
int  moveResizeWindow(wbp w,int x, int y, int width, int height);
novalue moveWindow(wbp w, int x, int y);
int  resizeWindow(wbp w,int width,int height);
int SetNewBitPattern(wcp wc, PBYTE bits);
int LoadFont(wbp wb, char *family, LONG attr, ULONG fontsize);
void FreeIdTable(void);
void FreeLocalID(LONG id);

/* -- not needed because of macro definitions
void SetCharContext(wbp wb, wsp ws, wcp wc);
void SetAreaContext(wbp wb, wsp ws, wcp wc);
void SetLineContext(wbp wb, wsp ws, wcp wc);
void SetImageContext(wbp wb, wsp ws, wcp wc);
   -- */

void SetClipContext(wbp wb, wsp ws, wcp wc);
void UnsetContext(wcp wc, void (*f)(wcp, wsp));
void UCharContext(wcp wc, wsp ws);
void ULineContext(wcp wc, wsp ws);
void UAreaContext(wcp wc, wsp ws);
void UImageContext(wcp wc, wsp ws);
void UClipContext(wcp wc, wsp ws);
void UAllContext(wcp wc, wsp ws);
void drawpoints(wbp wb, XPoint *pts, int npts);
void drawsegments(wbp wb, XSegment *segs, int nsegs);
void drawstrng(wbp wb, int x, int y, char *str, int slen);
void drawarcs(wbp w, XArc *arcs, int narcs);
void drawlines(wbp wb, XPoint *pts, int npts);
void drawrectangles(wbp wb, XRectangle *recs, int nrecs);
int dumpimage(wbp wb, char *filename, int x, int y, int width, int height);
void fillpolygon(wbp wb, XPoint *pts, int npts);
HBITMAP loadimage(wbp wb, char *filename, int *width, int *height);
void InitializeIdTable(void);
void InitializeColorTable(void);
void FreeColorTable(void);
LONG GetColorIndex(char *buf, double gamma);
void AddLocalIdToWindow(wsp ws, LONG id);
void ReleaseLocalId(LONG id);
void ReleaseColor(LONG indx);
void ColorInitPS(wbp wb);
void GetColorName(LONG indx, char *buf, int len);
void EnsureColorAvailable(LONG indx);
int GetTextWidth(wbp wb, char *text, int len);
int AddWindowDep(wsp ws, wcp wc);
int AddContextDep(wsp ws, wcp wc);
FILE *PMOpenConsole(void);
void UpdateCursorConfig(wsp ws, wcp wc);
void UpdateCursorPos(wsp ws, wcp wc);

#endif					/* PresentationManager */
#endif					/* Graphics */


#if UNIX
#ifdef KeyboardFncs
int getch(), getche(), kbhit();
#endif					/* KeyboardFncs */
#endif					/* UNIX */

#ifdef LargeInts
struct	b_bignum *alcbignum	Params((word n));
word	bigradix		Params((int sign, int r, char *s, char *x, union numeric *result));
double	bigtoreal		Params((dptr da));
int	realtobig		Params((dptr da, dptr dx));
int	bigtos			Params((dptr da, dptr dx));
novalue	bigprint		Params((FILE *f, dptr da));
int	cpbignum		Params((dptr da, dptr db));
int	bigadd			Params((dptr da, dptr db, dptr dx));
int	bigsub			Params((dptr da, dptr db, dptr dx));
int	bigmul			Params((dptr da, dptr db, dptr dx));
int	bigdiv			Params((dptr da, dptr db, dptr dx));
int	bigmod			Params((dptr da, dptr db, dptr dx));
int	bigneg			Params((dptr da, dptr dx));
int	bigpow			Params((dptr da, dptr db, dptr dx));
int	bigand			Params((dptr da, dptr db, dptr dx));
int	bigor			Params((dptr da, dptr db, dptr dx));
int	bigxor			Params((dptr da, dptr db, dptr dx));
int	bigshift		Params((dptr da, dptr db, dptr dx));
word	bigcmp			Params((dptr da, dptr db));
int	bigrand			Params((dptr da, dptr dx));
#endif					/* LargeInts */


/*
 * Prototypes for the run-time system.
 */

C_integer	iipow	Params((C_integer n1, C_integer n2));
int	bfunc	Params((noargs));
int	collect	Params((int region));
int	cvcset	Params((dptr dp,int * *cs,int *csbuf));
int	cvnum	Params((dptr dp,union numeric *result));
int	cvreal	Params((dptr dp,double *r));
int	eq	Params((dptr dp1,dptr dp2));
int	fixtrap	Params((noargs));
int	getimage	Params((dptr dp1, dptr dp2));
int	get_name	Params((dptr dp1, dptr dp2));
int	getstrg	Params((char *buf,int maxi,FILE *fd));
int	mkreal	Params((double r,dptr dp));
int	nthcmp	Params((dptr d1,dptr d2));
void	nxttab	Params((C_integer *col, dptr *tablst, dptr endlst, C_integer *last, C_integer *interval));
int	order	Params((dptr dp));
int	printable	Params((int c));
int	ripow	Params((double r, C_integer n, dptr rslt));
int	sig_rsm	Params((noargs));
int	subs_asgn	Params((dptr dest, const dptr src));
int	trcmp3	Params((struct dpair *dp1,struct dpair *dp2));
int	trefcmp	Params((dptr d1,dptr d2));
int	tvalcmp	Params((dptr d1,dptr d2));
int	tvcmp4	Params((struct dpair *dp1,struct dpair *dp2));
long	ckadd	Params((long i, long j));
long	ckmul	Params((long i, long j));
long	cksub	Params((long i, long j));
novalue	cmd_line	Params((int argc, char **argv, dptr rslt));
novalue	cotrace	Params((struct b_coexpr *ccp, struct b_coexpr *ncp, int swtch_typ, dptr valloc));
novalue	deref	Params((dptr dp1, dptr dp2));
novalue	envset	Params((noargs));
novalue	hgrow	Params((union block *bp));
novalue	hshrink	Params((union block *bp));
novalue	init	Params((char *name, int *argcp, char *argv[], int trc_init));
novalue	rtos			Params((double n,dptr dp,char *s));
int	tvtbl_asgn		Params((dptr dest, const dptr src));
novalue	varargs	Params((dptr argp, int nargs, dptr rslt));
#ifdef MultiThread
struct	b_coexpr *alccoexp	Params((long icodesize, long stacksize));
#else					/* MultiThread */
struct	b_coexpr *alccoexp	Params((noargs));
#endif					/* MultiThread */
struct	b_coexpr *create   Params((continuation fnc, struct b_proc *p, int ntemps, int wrk_size));
struct	b_external *alcextrnl	Params((int n));
struct	b_record *alcrecd	Params((int nflds,union block *recptr));
struct	b_proc *strprc	Params((dptr s, C_integer arity));
struct	b_tvsubs *alcsubs	Params((word len,word pos,dptr var));

#ifdef EventMon
novalue	MMAlc	Params((word len, int type));
novalue	MMStr	Params((word slen));
novalue EVVal	Params((word value, int event));
novalue EVValD	Params((dptr dp, int event));
novalue EVAsgn	Params((dptr dx));
#endif					/* EventMon */

#ifdef MultiThread
struct b_coexpr *loadicode Params((char *name, struct b_file *theInput,
				   struct b_file *theOutput,
				   struct b_file *theError,
				   C_integer bs, C_integer ss, C_integer stk));
novalue actparent Params((int eventcode));
int mt_activate   Params((dptr tvalp, dptr rslt, struct b_coexpr *ncp));
#endif					/* MultiThread */

#if COMPILER
novalue	tracebk	Params((struct p_frame *lcl_pfp,dptr argp));
int	invoke	Params((int nargs,dptr args, dptr rslt, continuation cont));
int	xdisp	Params((struct p_frame *fp,dptr dp,int count, FILE *f));
novalue	atrace	Params((noargs));
novalue	ctrace	Params((noargs));
novalue	failtrace	Params((noargs));
novalue	rtrace	Params((noargs));
novalue	strace	Params((noargs));
novalue	initalloc	Params((noargs));
struct	b_refresh *alcrefresh	Params((int na,int nl, int nt, int wk_sz));
#else					/* COMPILER */

#define Fargs dptr cargp

novalue	tracebk	Params((struct pf_marker *lcl_pfp,dptr argp));
int	invoke	Params((int nargs, dptr *cargs, int *n));
int	xdisp	Params((struct pf_marker *fp,dptr dp,int count, FILE *f));

int	Obscan			Params((int nargs,Fargs));
int	Ocreate			Params((word *entryp,Fargs));
int	Oescan			Params((int nargs,Fargs));
int	Ofield			Params((int nargs,Fargs));
int	Olimit			Params((int nargs,Fargs));
int	Ollist			Params((int nargs,Fargs));
int	Omkrec			Params((int nargs,Fargs));
novalue	atrace	Params((dptr dp));
novalue	ctrace	Params((dptr dp, int nargs, dptr arg));
novalue	failtrace	Params((dptr dp));
novalue	rtrace	Params((dptr dp, dptr rval));
novalue	strace	Params((dptr dp, dptr rval));
#ifdef MultiThread
novalue	initalloc	Params((word codesize, struct progstate *p));
#else					/* MultiThread */
novalue	initalloc	Params((word codesize));
#endif					/* MultiThread */
struct	b_refresh *alcrefresh	Params((word *e, int nl, int nt));

#ifdef ExternalFunctions
dptr	extcall			Params((dptr x, int nargs, int *signal));
#endif					/* ExternalFunctions */

#endif					/* COMPILER */
