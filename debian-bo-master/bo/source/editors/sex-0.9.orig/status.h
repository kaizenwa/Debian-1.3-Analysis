/*
 * status.h - declarations for status messages
 * Lars Wirzenius
 */

#ifndef status_h_included
#define status_h_included

#define STATUS_MAX	128

struct status {
	struct win *win;
	long row, col;
	long erow, ecol;
	int columnar;
	long total_rows;
	long total_chars;
	int dirty;
	char msg[STATUS_MAX];
};

struct win;

void status_init(struct status *, struct win *);
int status_get(struct status *, char **);

#endif
