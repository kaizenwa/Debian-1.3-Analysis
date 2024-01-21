
#include "global.h"

#include <signal.h>
#ifdef PINUP_DEFAULT
#	include <sys/stat.h>
#	include <pwd.h>
#endif
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/cursorfont.h>

#include <X11/extensions/shape.h>

#include <X11/Xos.h>
#ifndef X_GETTIMEOFDAY
	/* define X_GETTIMEOFDAY macro, a portable gettimeofday() */
	/* copied from Xos.h for pre-X11R6 releases               */
#	if defined(SVR4) || defined(VMS) || defined(WIN32)
#		define X_GETTIMEOFDAY(t) gettimeofday(t)
#	else
#		define X_GETTIMEOFDAY(t) gettimeofday(t, (struct timezone*)0)
#	endif
#endif
#ifndef FDS_TYPE
//
// The problem occured, that on HP-UX, the type used in the masks of the
// select() system call are not the usual 'fd_set'. Instead they are just
// integer pointers, which leads to an compile-error, when not casted.
// Therefore it actually will get casted by the following Macro FDS_TYPE,
// which should usually be defined to an empty string.
//
#  ifdef __hpux
#     define   FDS_TYPE (int*)
#  else
#     define   FDS_TYPE
#  endif
#endif

#ifndef _objects_h
#	include "objects.H"
#endif

#include "gifx_image.H"
#include "color_mapper.H"
#include "imgbuff.H"
#include "puzzle.H"

#include "cursor.h"

#define	_DEBUG
#include "global.h"

#ifndef JIG_DEFAULT
#	define JIG_DEFAULT	"default.gif"
#endif

Display	*dpy;
int		scr;
Window	win;
GC			gc;

int texture_mode=0;				// mode for texture mapping depending on depth

Cursor	normal_cursor, move_cursor, pull_cursor, idle_cursor, no_cursor;

int		zoom_factor=20;
int		win_size_x=100;
int		win_size_y=100;

int		offx=0;					// half tilesize as offset to frames
int		offy=0;
int		width =0;				// height of image
int		height=0;				// width of image
int		dx = 0;					// number of tiles in x-direction
int		dy = 0;					// number of tiles in y-direction
int		tile_size=0;			// average tile size
int		shuffle=3;				// shuffle tile as default
int		side_lock=-1;			// which side (of TwinPixmap) as default
#ifdef __hpux
int		shared=0;				// dont use extension as default
#else
int		shared=1;				// use MIT-SHM extension as default
#endif
int		shapes=0;

int		quit=0;					// global flag to initiate quitting
int		random_seed=0;
int		distortion=20;			// factor to control distortion of the tiles
double	maxang=45;				// maximum rotation angle (off from 90degrees)
int		shadow_size=1;			// pixels in shadow frame
int		straight_setup=-1;	// offset for straight debugging setup
int		no_flip=0;				// suppress automatic flip of landscape images
int		autocrop=1;				// try to cut the edges away

double	fliptimebase=0.07;	// base time for flipping
double	fliptimedelta=0.02;	// added to base for each tile
int		maxfliptiles=8;		// max. number of tiles for automated flip
int		minadjustcount=4;		// number of tiles to start 90 degrees autoadjust
double	flipsave=0.2;			// dont let the tile come close to a vertical
                              // position during the flip ...

double	turntimebase=0.15;	// base time for 90 degree rotation
double	turntimedelta=0.02;	// added to base for each additional tile
int		maxturntiles=5;		// max. number of tiles for rotation animation

int		maxsnapretries=1;		// max. no. of tiles that could recursively snap together

int		warp_center;
int		warp_lock_x=WARP_NO_LOCK;
int		warp_lock_y=WARP_NO_LOCK;

char		*dispname="";
char		*filename=JIG_DEFAULT;
int		verbose=0;
int		rotate=0;				// rotation demo for debugging
int		angle=0;					// preset angles for debugging

class Puzzle		*p;			// Collection of all puzzle pieces
class GifPixmap	*pm;			// Original pixmap for the puzzle tiles
class Port			*port;		// Port (Display synonym) for color mapping
class ObjectStack	*stk;			// administrator object for all viewable objects
class ImageBuffer	*img_buf;	//	memory for rotating image (probably shared)

// ===========================================================================

static unsigned char b[2];

static void my_srand( unsigned seed ) {
	b[0]=seed&0xff;
	b[1]=seed>>8;
}

int my_rand(void) {
	unsigned long	s1=b[0]<<8|b[1];
	s1+=12345;
	s1*=s1;
	b[0]=(unsigned char)((s1>>8)&0xff);
	b[1]=(unsigned char)((s1>>16)&0xff);
	return (s1>>8)&0xffff;
}

// ===========================================================================

static void local_usleep( long time )
{
struct timeval  timeout;
int    nfound;

   timeout.tv_sec  = (long)0;
   timeout.tv_usec = (long)time;

   nfound=select(0,0,0,0,&timeout);
}

/*static*/ void do_sound( char *str ) {
XKeyboardState		old_keyboard_values;
XKeyboardControl	values;
int		pitch, percent,duration,pause;
char		*str_p=str;

	XGetKeyboardControl(dpy,&old_keyboard_values);
	while( sscanf(str_p,"%03d%02d%02d%02d;",&pitch,&percent,&duration,&pause)==4 ) {
		values.bell_pitch=pitch;
		values.bell_percent=percent;
		values.bell_duration=duration;
		XChangeKeyboardControl(dpy,KBBellPercent|KBBellPitch|KBBellDuration,&values);
		XBell(dpy,values.bell_percent);
		XFlush(dpy);
		local_usleep(pause*10000);
		str_p+=10;
	}
#ifdef __hpux
	values.bell_pitch    = old_keyboard_values.bell_pitch;
	values.bell_percent  = old_keyboard_values.bell_percent;
	values.bell_duration = old_keyboard_values.bell_duration;
#else
	values.bell_pitch    = 440;
	values.bell_percent  =  50;
	values.bell_duration = 100;
#endif
	XChangeKeyboardControl(dpy,KBBellPercent|KBBellPitch|KBBellDuration,&values);
	XFlush(dpy);
}

// ===========================================================================
//
// some help routines for easy drawing ...
//


void DrawLine( const Real& x1, const Real& y1, const Real& x2, const Real& y2 ) {
int   px1 = XPix(x1);
int   py1 = YPix(y1);
int   px2 = XPix(x2);
int   py2 = YPix(y2);
	XDrawLine( dpy, win, gc, px1, py1, px2, py2 );
}
inline void DrawLine( const Vec2 &p1, const Vec2 &p2 ) {
   DrawLine( p1.X(), p1.Y(), p2.X(), p2.Y() );
}

// ===========================================================================

static unsigned long start_seconds;

static void InitTime() {
struct timeval start;

	X_GETTIMEOFDAY( &start );
	start_seconds = start.tv_sec;
}

double GetCurrentTime(int busy) {
struct timeval current;

	 X_GETTIMEOFDAY( &current );
#ifdef PINUP_DEFAULT
	 	static unsigned long last=0;
	 	static unsigned long idle_start=0;

	 	unsigned long	val=(current.tv_sec%86400uL);
	 	if (busy)	idle_start=val+15;
	 	if (last!=val/5&&val<idle_start) {
	 		last=val/5;
	 		unsigned long	minute=val/60+120;
	 		if (minute<540||(minute>=570&&minute<765)||(minute>=810&&minute<1020)) {
				do_sound( "440995005;370995005;440995005;" );
			}
	 	}
#else
	(void)busy;
#endif
	 return( ((double)(current.tv_sec-start_seconds))+(current.tv_usec/1000000.0) );
}

// ===========================================================================

class BackDrop : public Object {
	public:
		BackDrop();
		virtual ~BackDrop();

		virtual void ExposeRegion( int x, int y, int width, int height );
		virtual void ZoomView( int midx, int midy, int chg );

	protected:
		void InitFillStyle();

		unsigned long	toppixel;
		unsigned long	bgpixel;
		unsigned long	botpixel;
		Pixmap	tilemap;
		GC			gc_tile;
};

BackDrop::BackDrop() {
	bgpixel =port->AllocNamedColor( "grey50" );
	toppixel=port->AllocNamedColor( "grey60" );
	botpixel=port->AllocNamedColor( "grey40" );

	gc_tile=XCreateGC(dpy,RootWindow(dpy,scr),0,0);
	InitFillStyle();
}

BackDrop::~BackDrop() {
	XFreeGC(dpy,gc_tile);
	XFreePixmap(dpy,tilemap);
}

void BackDrop::InitFillStyle() {
	tilemap=XCreatePixmap(dpy,RootWindow(dpy,scr),
								zoom_factor,zoom_factor,DefaultDepth(dpy,scr));
	XSetForeground(dpy,DefaultGC(dpy,scr),bgpixel);
	XFillRectangle(dpy,tilemap,DefaultGC(dpy,scr),0,0,zoom_factor,zoom_factor);
	XSetForeground(dpy,DefaultGC(dpy,scr),toppixel);
	XDrawLine(dpy,tilemap,DefaultGC(dpy,scr),0,0,zoom_factor-2,0);
	XDrawLine(dpy,tilemap,DefaultGC(dpy,scr),0,1,0,zoom_factor-2);
	XSetForeground(dpy,DefaultGC(dpy,scr),botpixel);
	XDrawLine(dpy,tilemap,DefaultGC(dpy,scr),zoom_factor-1,0,zoom_factor-1,zoom_factor-1);
	XDrawLine(dpy,tilemap,DefaultGC(dpy,scr),0,zoom_factor-1,zoom_factor-2,zoom_factor-1);
	XSetTile(dpy,gc_tile,tilemap);
	XSetFillStyle(dpy,gc_tile,FillTiled);
}

void BackDrop::ExposeRegion( int x, int y, int width, int height ) {
	XSetTSOrigin(dpy,gc_tile,-x,-y);
	XFillRectangle(dpy,mystack->dbmap,gc_tile,0,0,width,height);
}

void BackDrop::ZoomView( int /*midx*/, int /*midy*/, int chg ) {
	zoom_factor+=chg;
	XFreePixmap(dpy,tilemap);
	InitFillStyle();
	zoom_factor-=chg;
}

// ===========================================================================

void usage() {
	printf( "usage  : xjig [<options>]\n" );
	printf( "\n" );
	printf( "options: -file <file>: use gif-image in <file>\n" );
	printf( "         -w  <n>     : number of tiles in x direction\n" );
	printf( "         -h  <n>     : number of tiles in y direction\n" );
	printf( "         -ts <n>     : set average tile size\n" );
	printf( "         -ww <n>     : width of image\n" );
	printf( "         -wh <n>     : height of image\n" );
	printf( "         -side <n>   : lock side <n> only\n" );
	printf( "         -no_flip    : no automatic flip of landscape images\n" );
	printf( "         -no_crop    : do not crop images\n" );
#ifdef USE_MIT_SHM
	printf( "         -shm        : use MIT-SHM %s\n", (shared)?"(default)":"" );
	printf( "         -no_shm     : don't use MIT-SHM extension %s\n", (shared)?"":"(default)" );
#endif
	printf( "         -shapes     : use shape extension to draw on desktop\n" );
	printf( "         -no_anim    : don't animate rotation and flipping of tiles\n" );
	if (verbose) {
	printf( "additional options for debugging:\n" );
	printf( "         -a  <n>    : startup angle\n" );
	printf( "         -s         : shuffle tiles\n" );
	printf( "         -sf        : full shuffle\n" );
	printf( "         -sa        : shuffle angles\n" );
	printf( "         -sp        : shuffle positions\n" );
	printf( "         -r         : rotation demo\n" );
	printf( "         -8 -16 -32 : manually select optimized texture mapping routine\n" );
	printf( "         -dist <n>  : distortion percentage\n" );
	printf( "         -maxang <n>: maximum rotation angle at startup\n" );
	printf( "         -rand <n>  : seed for random generator\n" );
	printf( "         -shadow <n>: pixels of shadowed border\n" );
	printf( "         -setup <n> : straight setup with offset <n>\n" );
	printf( "         -ftb <t>   : set flip time base                        (%g)\n", fliptimebase );
	printf( "         -ftd <t>   : set flip time delta (for aditional tiles) (%g)\n", fliptimedelta );
	printf( "         -mft <t>   : maximun number of flip tiles              (%d)\n", maxfliptiles );
	printf( "         -fs <t>    : set value, when to stop the flip          (%g)\n", flipsave );
	printf( "         -ttb <t>   : set turn time base                        (%g)\n", turntimebase );
	printf( "         -ttd <t>   : set turn time delta (for aditional tiles) (%g)\n", turntimedelta );
	printf( "         -mtt <t>   : maximun number of flip tiles              (%d)\n", maxturntiles );
	printf( "         -msr <t>   : maximun number snap retries               (%d)\n", maxsnapretries );
	printf( "         -mac <t>   : minimum number of tiles for adjustment    (%d)\n", minadjustcount );
	}
	printf( "\ncontrols:\n" );
	printf( "\n" );
	printf( "click left:              rotate 90 degrees left\n" );
	printf( "click right:             rotate 90 degrees right\n" );
	printf( "click middle:            flip tile to backside on double sided puzzle\n" );
	printf( "drag left:               drag with automatic rotation\n" );
	printf( "drag middle:             straight drag\n" );
	printf( "drag left+middle:        pause rotator drag for a straight drag\n" );
	printf( "drag middle+left:        pause straight drag for a static rotation\n" );
	printf( "drag middle+click left:  rotate 90 degrees left during straight drag\n" );
	printf( "drag middle+click right: rotate 90 degrees right during straight drag\n" );
	printf( "CTRL + click left:       (same as click middle)\n" );

	printf( "\n" );
	printf( "author : Helmut Hoenig, July-24-96 (V2.4)   (Helmut.Hoenig@hub.de)\n" );
	printf( "\n" );
	exit(0);
}

void scan_args( int argc, char **argv ) {
	for (int i=1;i<argc;i++) {
		if (!strcmp(argv[i],"-display"))			dispname=argv[++i];
		else if (!strcmp(argv[i],"-ww"))	width =atoi(argv[++i]);
		else if (!strcmp(argv[i],"-wh")||!strcmp(argv[i],"-hh"))	height=atoi(argv[++i]);
		else if (!strcmp(argv[i],"-w"))	dx=atoi(argv[++i]);
		else if (!strcmp(argv[i],"-h")&&i<argc-1)	dy=atoi(argv[++i]);
		else if (!strcmp(argv[i],"-ts"))			tile_size=atoi(argv[++i]);
		else if (!strcmp(argv[i],"-r"))			rotate=1;
		else if (!strcmp(argv[i],"-a")) {
				angle=atoi(argv[++i]);
				shuffle&=~1;
		}
		else if (!strcmp(argv[i],"-s"))			shuffle=0;

		else if (!strcmp(argv[i],"-sf"))			shuffle|=4;
		else if (!strcmp(argv[i],"-sa"))			shuffle=1;
		else if (!strcmp(argv[i],"-sp"))			shuffle=2;
		else if (!strcmp(argv[i],"-no_flip"))  no_flip=1;
		else if (!strcmp(argv[i],"-no_crop"))  autocrop=0;
		else if (!strcmp(argv[i],"-side"))		side_lock=atoi(argv[++i]);
		else if (!strcmp(argv[i],"-rand"))		random_seed=atoi(argv[++i]);
		else if (!strcmp(argv[i],"-dist"))		distortion=atoi(argv[++i]);
		else if (!strcmp(argv[i],"-maxang"))	maxang=atof(argv[++i]);
		else if (!strcmp(argv[i],"-shadow"))	shadow_size=atoi(argv[++i]);
		else if (!strcmp(argv[i],"-setup")) {
				straight_setup=atoi(argv[++i]);
				shuffle=0;
		}
		else if (!strcmp(argv[i],"-file"))			filename=argv[++i];
#ifdef PINUP_DEFAULT
		else if (!strcmp(argv[i],"-pinup"))			filename=PINUP_DEFAULT;
#endif
		else if (!strcmp(argv[i],"-8"))				texture_mode=1;
		else if (!strcmp(argv[i],"-16"))				texture_mode=2;
		else if (!strcmp(argv[i],"-32"))				texture_mode=3;
		else if (!strcmp(argv[i],"-shm"))			shared=1;
		else if (!strcmp(argv[i],"-no_shm"))		shared=0;
		else if (!strcmp(argv[i],"-shapes"))		shapes=1;
		else if (!strncmp(argv[i],"-verbose",2))	verbose=1;
		else if (!strcmp(argv[i],"-ftb"))			fliptimebase=atof(argv[++i]);
		else if (!strcmp(argv[i],"-ftd"))			fliptimedelta=atof(argv[++i]);
		else if (!strcmp(argv[i],"-mft"))			maxfliptiles=atoi(argv[++i]);
		else if (!strcmp(argv[i],"-fs"))				flipsave=atof(argv[++i]);
		else if (!strcmp(argv[i],"-ttb"))			turntimebase=atof(argv[++i]);
		else if (!strcmp(argv[i],"-ttd"))			turntimedelta=atof(argv[++i]);
		else if (!strcmp(argv[i],"-mtt"))			maxturntiles=atoi(argv[++i]);
		else if (!strcmp(argv[i],"-msr"))			maxsnapretries=atoi(argv[++i]);
		else if (!strcmp(argv[i],"-mac"))			minadjustcount=atoi(argv[++i]);
		else if (!strcmp(argv[i],"-no_anim")) {
									maxturntiles = maxfliptiles = 0;
		}

		else usage();
	}
}

void (*old_sighandler)(int);
void sig_quit( int ) {
	// handler that raises the quit flag on interrupts
	quit=1;
}

static XErrorHandler old_handler;
int error_handler( Display * dpy_in, XErrorEvent *xerror ) {
	if (xerror->error_code==BadWindow) {
		// just ignore BadWindow failures, since they easily might occur
		// due to the destruction of windows.
		return 0;
	}
	else return old_handler( dpy_in, xerror );
}

int main(int argc, char **argv)
{
static char *def_argv[]=
 {	"xpuzzle", "-file", JIG_DEFAULT, 0 };
XEvent	event;
unsigned long next_sec=0;
const		char *options;

	if (argc<2) {
		argv=def_argv;
		argc=(sizeof(def_argv)/sizeof(char*))-1;
	}

	scan_args( argc, argv );

	quit=0;
	old_sighandler=signal( SIGINT, sig_quit );

	//
	// open the display and a port object as a color manager
	//
	dpy = XOpenDisplay(dispname);
	if (!dpy) {
		fprintf(stderr,"can't open display '%s'\n", dispname);
		exit(-1);
	}
	scr = DefaultScreen(dpy);
	port = new Port(dpy);

	printf( "\n" );
	printf( "xjig V2.4, by Helmut Hoenig, July-24-96\n" );
	printf( "\n" );

	if (!texture_mode) {
		//
		// check screen depth to select function for texture mappings
		//
		switch(DefaultDepth(dpy,scr)) {
		case 8:		texture_mode=1;	break;
		case 16:		texture_mode=2;	break;
		case 24:
		case 32:		texture_mode=3;	break;
		}
	}
	if (!texture_mode) {
		fprintf( stderr, "*** Unable to select texture mode for Depth %d\n", DefaultDepth(dpy,scr) );
		fprintf( stderr, "    You can manually select one by trying either -8, -16 or -32\n" );
		fprintf( stderr, "    Good Luck.\n" );
		exit(0);
	}

	if (verbose) {
		switch( texture_mode ) {
		case 1: printf( "texture mode 1: 1 byte\n" ); break;
		case 2: printf( "texture mode 2: 2 byte\n" ); break;
		case 3: printf( "texture mode 3: 4 byte\n" ); break;
		}
	}

	old_handler=XSetErrorHandler( error_handler );

	// check for shapes
	if (shapes) {
		int major, minor;
		shapes=XShapeQueryVersion(dpy,&major,&minor);
		if (shapes&verbose)
			printf( "--- using shape extension V%d.%d\n", major, minor );
	}

#ifdef PINUP_DEFAULT
	{
	struct stat buf;
	struct passwd *pwd;
	char	name[256];

		stat( filename, &buf );
		pwd=getpwuid( buf.st_uid );
		strcpy( name, pwd->pw_gecos );
		if (strchr(name,','))		*strchr(name,',')='\0';
		if ( !strstr( filename, "default" ) ) {
			if (!strcmp( name, "Gast-Kennung"))		strcpy(name,"Norbert Klaus");
			printf( "loading: '%s', supplied by %s.\n\n", filename, name );
		}
	}
#endif

	//
	// load image an scale it according to the input options
	// or set original image size, when no size selected.
	//
	pm=new GifPixmap(port,filename);
	options=pm->GetExtensionData( OPTION_EXTENSION );
	if (options) {
		char	opt_buffer[256];
		int	argc_opt=0;
		char	*argv_opt[20];
		char	*cptr=opt_buffer;
		strcpy(opt_buffer,options);

		argv_opt[argc_opt++]=argv[0];
		argv_opt[argc_opt++]=cptr;
		while( *cptr ) {
			if ( *cptr==' ' ) {
				*cptr++=0;
				argv_opt[argc_opt++]=cptr;
			}
			else cptr++;
		}
		scan_args( argc_opt, argv_opt );
	}
	const char *comment=pm->GetExtensionData( COMMENT_EXTENSION );
	if (comment)	{ printf( comment ); putchar( '\n' ); }

	if (autocrop)		pm->CropImage();
	if (pm->Width()>pm->Height()&&!no_flip)		pm->Rotate90();
	if (verbose) 
		printf("original image size: %d %d\n", pm->Width(), pm->Height() );

	if (!width&&!height) {
		width = pm->Width();
		height= pm->Height();
	}
	else {
		// scale to desired size, (keep aspect when only one param selected)
		if (!width)		width=height*pm->Width()/pm->Height();
		if (!height)	height=width*pm->Height()/pm->Width();
	}
	pm->CreateData( width, height );

	if (!dx||!dy) {
		if (dy)			dx=width*dy/height;
		else if (dx)	dy=height*dx/width;
		else if (tile_size) {
							dx=width/tile_size;
							dy=height/tile_size;
		}
	}
	if (dx<=0||dy<=0)	 { dx=4; dy=6; }

	if (verbose)
		printf( "number of tiles: %d\n", dx * dy );

	offy = height/dy/2;
	if ( (height+(dy+1)*offy) > DisplayHeight(dpy,scr)-20 ) {
		offy=(DisplayHeight(dpy,scr)-height-20)/(dy+1);
	}
	offx = width /dx/2;
	win_size_x=2*(width+dx*offx)+offx;
	if ( win_size_x > DisplayWidth(dpy,scr)-8 ) {
		win_size_x=DisplayWidth(dpy,scr)-8;
		if ( 2*width+dx*offx+offx > win_size_x )	offx=(win_size_x-2*width)/(dx+1);
	}
	tile_size=width/dx;

	if (shapes) {
		win_size_x=DisplayWidth(dpy,scr);
		win_size_y=DisplayHeight(dpy,scr);
		win=RootWindow(dpy,scr);
	}
	else {
		win_size_y=height+(dy+1)*offy;

		win=XCreateSimpleWindow(dpy,RootWindow(dpy,scr),
				0,0,win_size_x,win_size_y,2,WhitePixel(dpy,scr),
						port->AllocNamedColor( "grey50" ) );
		XStoreName(dpy,win,argv[0]);
	}
	gc =XCreateGC(dpy,win,0,0);
	XSetForeground(dpy,gc,WhitePixel(dpy,scr));

	// prepare some cursors
XColor	white_col, black_col;
Pixmap	pixmap;

	XParseColor(dpy,DefaultColormap(dpy,scr), "white", &white_col );
	XParseColor(dpy,DefaultColormap(dpy,scr), "black", &black_col );

	normal_cursor = XCreateFontCursor( dpy, XC_top_left_arrow );
	move_cursor   = XCreateFontCursor( dpy, XC_fleur );
	pull_cursor   = XCreateFontCursor( dpy, XC_hand2 );
	idle_cursor   = XCreateFontCursor( dpy, XC_watch );

	pixmap = XCreateBitmapFromData(dpy,RootWindow(dpy,scr),
					cursor_bits, cursor_width, cursor_height );
	no_cursor = XCreatePixmapCursor( dpy, pixmap, pixmap, 
					&white_col, &black_col, cursor_x_hot, cursor_y_hot );
	XFreePixmap( dpy, pixmap );



	InitTime();

	// create Object-Stack with Background
	if (shapes) {
		stk = new WindowObjectStack();
	}
	else {
		stk = new DBObjectStack();
		stk->Append( new BackDrop() );
	}

	// create buffer for faster image rotation
	img_buf = new ImageBuffer();

	// initialize puzzle game
	my_srand( random_seed );
	p = new Puzzle();
	p->Init(width,height,dx,dy, pm->GetExtensionData( FLATTILE_EXTENSION ) );

	// check for hidden pieces
	options=pm->GetExtensionData( REMOVETILE_EXTENSION );
	if (options) {
		const char	*cptr=options;
		while(*cptr) {
			int x,y;
			sscanf(cptr,"%02x%02x",&x,&y);
			cptr+=4;
			p->DropTile(x,y);
		}
	}

	if (shapes) {
		XSelectInput(dpy,win,KeyPressMask);
	}
	else {
		XSelectInput(dpy,win, ExposureMask|StructureNotifyMask|KeyPressMask|
				ButtonPressMask|ButtonReleaseMask|PointerMotionMask);
		XMapRaised(dpy,win);
	}

	while(!quit) {

		if (!XPending(dpy)) {
			XSync(dpy,0);
			while (!XPending(dpy)) {
				struct timeval timeout;
				struct fd_set readfds;
				int		nfds;
	
				FD_ZERO( &readfds );
				FD_SET(  ConnectionNumber(dpy), &readfds  );
				nfds = ConnectionNumber(dpy)+1;
	
				timeout.tv_sec  = 0;
				double current_time=GetCurrentTime();
				timeout.tv_usec = (long)(1000000 * (1.0-(current_time-floor(current_time))));
				select( nfds, FDS_TYPE &readfds, 0, 0, &timeout );
	
				current_time = GetCurrentTime();
				//	printf( "%g\n", current_time );
				if ((unsigned long)current_time>next_sec) {
					char	 buffer[20];
					next_sec = (unsigned long)current_time;
					if (!p->Finished()) {
						sprintf( buffer, "xpuzzle:  %02ld:%02ld",
							next_sec / 60, next_sec % 60 );
						if (!shapes)	XStoreName(dpy,win,buffer);
					}
					else {
						sprintf( buffer, "no more tiles left at:  %02ld:%02ld",
							next_sec / 60, next_sec % 60 );
						if (!shapes)	XStoreName(dpy,win,buffer);
						XBell(dpy,100);
						XFlush(dpy);
						next_sec=1000000;
					}
				}
			}
		}


		XNextEvent(dpy,&event);
		switch(event.type) {
		case KeyPress: {
			char				buffer=0;
			XComposeStatus	compose;
			KeySym			keysym;
			int				mult=(event.xkey.state&ShiftMask)?2:1;

			XDefineCursor(dpy,win,idle_cursor);
			XFlush(dpy);
			XLookupString( (XKeyEvent*)&event, &buffer, 1, &keysym, &compose );
			switch( keysym ) {
			case XK_plus:
			case XK_KP_Add:
			case XK_Page_Up:
			case XK_KP_Page_Up:
					stk->ZoomView(win_size_x/2,win_size_y/2,2*mult);
					stk->ExposeRegion(0,0,win_size_x,win_size_y);
					break;
			case XK_minus:
			case XK_KP_Subtract:
			case XK_Page_Down:
			case XK_KP_Page_Down:
					if (zoom_factor>5)	stk->ZoomView(win_size_x/2,win_size_y/2,-2*mult);
					stk->ExposeRegion(0,0,win_size_x,win_size_y);
					break;
			case XK_Right:
			case XK_KP_Right:
					stk->PanView( 10*mult, 0 );
					stk->ExposeRegion(0,0,win_size_x,win_size_y);
					break;
			case XK_Down:
			case XK_KP_Down:
					stk->PanView( 0, 10*mult );
					stk->ExposeRegion(0,0,win_size_x,win_size_y);
					break;
			case XK_Left:
			case XK_KP_Left:
					stk->PanView( -10*mult, 0 );
					stk->ExposeRegion(0,0,win_size_x,win_size_y);
					break;
			case XK_Up:
			case XK_KP_Up:
					stk->PanView( 0, -10*mult );
					stk->ExposeRegion(0,0,win_size_x,win_size_y);
					break;
			case XK_End:
			case XK_KP_End: {
					int x1,y1,x2,y2;
					stk->GetExtent(&x1,&y1,&x2,&y2);
					stk->PanView( x1-win_size_x/2+(x2-x1)/2, y1-win_size_y/2+(y2-y1)/2 );
					int	zf1=(int)(win_size_x*zoom_factor/(x2-x1));
					int	zf2=(int)(win_size_y*zoom_factor/(y2-y1));
					if (zf2<zf1)	zf1=zf2;
					if (zf1>3) {
						stk->ZoomView(win_size_x/2,win_size_y/2,zf1-zoom_factor);
						stk->ExposeRegion(0,0,win_size_x,win_size_y);
					}
					break;
			}
			case XK_Home:
			case XK_KP_Home: {
					int x1,y1,x2,y2;
					stk->GetExtent(&x1,&y1,&x2,&y2);
					stk->PanView( x1-10, y1-win_size_y/2+(y2-y1)/2 );
					stk->ZoomView(10,win_size_y/2,20-zoom_factor);
					stk->ExposeRegion(0,0,win_size_x,win_size_y);
					break;
			}
			case XK_Escape:
			case XK_Q:
			case XK_q:
					quit=1;
					break;
			default:
					break;
			}
			XDefineCursor(dpy,win,normal_cursor);
			XSync(dpy,0);
			warp_lock_x=WARP_NO_LOCK;	// just as a way tp unlock
			warp_lock_y=WARP_NO_LOCK;
			break;
		}

		case ButtonPress:
			GetCurrentTime(1);	// Reset idle start
			stk->DispatchPress( &event.xbutton );
			break;
		case ButtonRelease:
			stk->DispatchRelease( &event.xbutton );
			break;
		case MotionNotify:
			if (warp_lock_x!=WARP_NO_LOCK) {
				do {
					if ( (!shapes&&warp_lock_x==event.xmotion.x&&warp_lock_y==event.xmotion.y)
						||((shapes)&&warp_lock_x==event.xmotion.x_root&&warp_lock_y==event.xmotion.y_root) ) {
						warp_lock_x=WARP_NO_LOCK;
						warp_lock_y=WARP_NO_LOCK;
						break;
					}
					printf( "#### motion event skipped due to warp lock.\n" );
				}
				while( XCheckMaskEvent(dpy,PointerMotionMask,&event) );
			}
			else {
				while( XCheckMaskEvent(dpy,PointerMotionMask,&event) );
				if (event.xmotion.state)	stk->DispatchMotion( &event.xmotion );
			}
			break;

		case EnterNotify:
			XSetInputFocus( dpy, event.xcrossing.window, (int)None, CurrentTime );
			break;

		case Expose:
			if (shapes) {
				stk->ExposeWindowRegion( event.xexpose.window, event.xexpose.x,
					event.xexpose.y, event.xexpose.width, event.xexpose.height );
			}
			else {
				stk->ExposeRegion(event.xexpose.x, event.xexpose.y,
						event.xexpose.width, event.xexpose.height );
			}
				if (rotate) {
					XSync(dpy,0);
					double	t1=GetCurrentTime();
					p->Rotation();
					double	t2=GetCurrentTime();
					printf( "Rotation Time: %.4g secs.\n", t2 - t1 );
					rotate=0;
				}
			break;

		case ConfigureNotify:
			if (win_size_x!=event.xconfigure.width||win_size_y!=event.xconfigure.height) {
				win_size_x=event.xconfigure.width;
				win_size_y=event.xconfigure.height;
			}
			break;
		}
	}

	delete img_buf;

	printf( "terminated\n" );
	return 0;
}
