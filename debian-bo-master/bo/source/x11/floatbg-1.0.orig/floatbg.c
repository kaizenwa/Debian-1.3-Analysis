/*
 *	floatbg.c
 *	author: Jan Rekers
 *	purpose: This program slowly changes the color of the root window of X.
 */

#include <X11/Xlib.h>
#include <stdio.h>

#define MaxColor 	0xffff
#define MaxTime  	0x7fff
#define pi		3.1415926535
#define default_sf_in_hf	.25
#define default_s_mid	.375
#define default_s_var	.125
#define default_v_val	.87

char *program_name;
Display *dpy;
int screen;
Window root;

struct hsv { float h,s,v; };
struct rgb { float r,g,b; };

void hsv2rgb(), ticks2hsv();
float sf_in_hf = default_sf_in_hf;
float s_mid = default_s_mid;
float s_var = default_s_var;
float v_val = default_v_val;

unsigned long GetMutableColor();
void SetColor();
long time(), random();
double atof();

usage()
{
    fprintf(stderr, "usage: %s [options]\n", program_name);
    fprintf(stderr, "  where options are:\n");
    fprintf(stderr, "  -display <display>   or   -d <display>\n");
    fprintf(stderr, "  -help\n");
    fprintf(stderr, "  -value <float>   (default %1.2f)\n", default_v_val);
    fprintf(stderr, "  -satmid <float>  (default %1.3f)\n", default_s_mid);
    fprintf(stderr, "  -satvar <float>  (default %1.3f)\n", default_s_var);
    fprintf(stderr, "  -fase <float>    (default %1.2f)\n", default_sf_in_hf);
    fprintf(stderr, "  value, (satmid-satvar) and (satmid+satvar) must be between 0 and 1\n");
    exit(1);
}

main(argc, argv) 
    int argc;
    char **argv;
{
    char *display_name = NULL;
    unsigned long cmapentry;
    register int ticks;
    int i;
    struct hsv hsv;
    struct rgb rgb;
    extern char *optarg;

    program_name=argv[0];
    for (i = 1; i < argc; i++) {
	if (!strcmp ("-display", argv[i]) || !strcmp ("-d", argv[i])) {
	    if (++i>=argc) usage ();
	    display_name = argv[i];
	    continue;
	}
	if (!strcmp("-help", argv[i])) {
	    usage();
	}
	if (!strcmp("-value", argv[i])) {
	    if (++i>=argc) usage();
	    v_val = atof(argv[i]);
	    continue;
	}
	if (!strcmp("-satmid", argv[i])) {
	    if (++i>=argc) usage();
	    s_mid = atof(argv[i]);
	    continue;
	}
	if (!strcmp("-satvar", argv[i])) {
	    if (++i>=argc) usage();
	    s_var = atof(argv[i]);
	    continue;
	}
	if (!strcmp("-fase", argv[i])) {
	    if (++i>=argc) usage();
	    sf_in_hf = atof(argv[i]);
	    continue;
	}
	usage();
    } 

    if ( (v_val < 0) || (v_val > 1) ) usage();
    if ( ((s_mid - s_var) < 0) || ((s_mid + s_var) > 1) ) usage();

    dpy = XOpenDisplay(display_name);
    if (!dpy) {
	fprintf(stderr, "%s:  unable to open display '%s'\n",
		program_name, XDisplayName (display_name));
	exit(1);
    }
    screen = DefaultScreen(dpy);
    root = RootWindow(dpy, screen);

    srandom( (int)time(0L) );
    ticks = (int)random() % MaxTime;

    cmapentry = GetMutableColor();
    XSetWindowBackground(dpy, root, cmapentry);
    XClearWindow(dpy, root);
    while ( 1 ) {
	ticks2hsv(ticks, &hsv);
	hsv2rgb(&hsv, &rgb);
	SetColor(cmapentry, &rgb);
	XFlush(dpy);
	ticks = ticks++ % MaxTime;
	sleep(10);
    }
}

unsigned long GetMutableColor ()
{
    XColor color;
    if (XAllocColorCells (dpy, DefaultColormap(dpy,screen),
			  0, 0, 0, &color.pixel, 1) == NULL) {
	fprintf(stderr, "%s:  unable to allocate colorcells\n", program_name);
	exit(1); }
    return(color.pixel);
}

void SetColor (pixel, rgb)
    unsigned long pixel;
    struct rgb *rgb;
{
    XColor color;
    int red = (int) MaxColor * rgb->r;
    int green = (int) MaxColor * rgb->g;
    int blue = (int) MaxColor * rgb->b;

    color.red = red; color.green = green; color.blue = blue;
    color.pixel = pixel;
    color.flags = DoRed|DoGreen|DoBlue;
    XStoreColor(dpy, DefaultColormap(dpy,screen), &color);
}

void ticks2hsv (ticks, hsv)
    int ticks;
    struct hsv *hsv;
{
    float s_fase, sin();

    hsv->h = ticks % 360;
    s_fase = sf_in_hf * (pi / 180) * ticks;
    hsv->s = s_mid - (s_var * sin(s_fase));
    hsv->v = v_val;
}

void hsv2rgb (hsv, rgb)
    struct hsv *hsv;
    struct rgb *rgb;
{
    int i;
    float f, p, q, r, h, v;

    v = hsv->v;
    if (hsv->s == 0) {
	rgb->r = v; rgb->g = v; rgb->b = v; }
    else {
	h = hsv->h / 60;
	i = (int) h;
	f = h - i;
	p = hsv->v * (1 - hsv->s);
	q = hsv->v * (1 - (hsv->s * f));
	r = hsv->v * (1 - (hsv->s * (1 - f)));
	switch (i) {
	    case 0: rgb->r = v; rgb->g = r; rgb->b = p; break;
	    case 1: rgb->r = q; rgb->g = v; rgb->b = p; break;
	    case 2: rgb->r = p; rgb->g = v; rgb->b = r; break;
	    case 3: rgb->r = p; rgb->g = q; rgb->b = v; break;
	    case 4: rgb->r = r; rgb->g = p; rgb->b = v; break;
	    case 5: rgb->r = v; rgb->g = p; rgb->b = q; break;
	}
    }
}
