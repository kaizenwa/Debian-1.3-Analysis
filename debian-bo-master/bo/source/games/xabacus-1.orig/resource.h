/* resource.h: 
 * definitions for the resource & command-line parser for xabaxus.
 * Make sure main.h is also modified to reflect changes to resources made in
 * this file.
 */

struct _resources {
	String display;
	int beadwidth; 
	int beadheight;
	int framewidth; 
	int ncols; 
	String colors[4];
	String demo;
	String demofont;
	Boolean script;
} app_resources;

#define offset(field)   XtOffset (struct _resources *, field)

static XtResource resources[] = {
	{ "display","Display", XtRString, sizeof(String),
	offset(display), XtRString, "                 "},
	{ "beadwidth","Beadwidth",XtRInt, sizeof (int),
	offset(beadwidth),XtRString, "30"},
	{ "beadheight","Beadheight",XtRInt, sizeof (int),
	offset(beadheight),XtRString, "20"},
	{ "framewidth","Framewidth",XtRInt, sizeof (int),
	offset(framewidth),XtRString, "10"},
	{ "ncols","Ncols",XtRInt, sizeof (int),
	offset(ncols),XtRString, "13"},
	{ "framecolor","Framecolor", XtRString, sizeof(String),
	offset(colors[0]), XtRString, "brown"},
	{ "bg","Bg", XtRString, sizeof(String),
	offset(colors[1]), XtRString, "white"},
	{ "beadcolor","Beadcolor", XtRString, sizeof(String),
	offset(colors[2]), XtRString, "green4"},
	{ "railcolor","Railcolor", XtRString, sizeof(String),
	offset(colors[3]), XtRString, "black"},
	{ "demo","Demo",XtRString, sizeof(String),
	offset(demo), XtRString, "                                  " },
	{ "demofont","Demofont",XtRString, sizeof(String),
	offset(demofont), XtRString, "-*-times-*-r-*-*-*-180-*-*-*-*" },
	{ "script","Script",XtRBoolean, sizeof(Boolean),
	offset(script), XtRString, "False" },

};

static XrmOptionDescRec options[] = {
	{ "-display","display",XrmoptionSepArg,	NULL},
	{ "-beadwidth","beadwidth",XrmoptionSepArg,	NULL},
	{ "-beadheight","beadheight",XrmoptionSepArg,	NULL},
	{ "-framewidth","framewidth",XrmoptionSepArg,	NULL},
	{ "-ncols","ncols",XrmoptionSepArg,	NULL},
	{ "-framecolor","framecolor",XrmoptionSepArg,	NULL},
	{ "-bg","bg",XrmoptionSepArg,	NULL},
	{ "-beadcolor","beadcolor",XrmoptionSepArg,	NULL},
	{ "-railcolor","railcolor",XrmoptionSepArg,	NULL},
	{ "-demo","demo",XrmoptionSepArg, NULL},
	{ "-demofont","demofont", XrmoptionSepArg, NULL},
	{ "-fn","demofont", XrmoptionSepArg, NULL},
	{ "-script","script",XrmoptionNoArg, (caddr_t)"TRUE"},
};

