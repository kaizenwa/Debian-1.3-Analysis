/*
 * xover public data and routines
 *
 * @(#)$Id: xover.h,v 1.2 1994/11/01 05:57:31 sob Exp sob $
 */

struct xobj {
	FILE *fp;
	int num;
	int open_tried;
	FILE *(*open)();
	void (*fake)();
	char *name;
	char *cmd;
};

extern struct xobj over;
extern struct xobj rover;

int over_is_cheap();
int over_header();
char *over_grab_header();
int xfind();
