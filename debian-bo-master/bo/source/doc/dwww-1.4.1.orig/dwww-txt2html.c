/*
 * dwww-man2html.c
 *
 * A very simple converter from formatted manual pages to HTML. Handles
  * backspace characters. Converts `<', `>', and `&' properly. Does _NOT_ add
 * <head> and <body> tags -- caller must do that. _Does_ add <pre>
 * and </pre> tags. Converts manual page references to anchors.
 *
 * Bug: because of the static line length limit, anchor generation
 * can fail if the manual page reference happens to fall on buffer
 * limit. This is rather unlikely, though, if we make the buffer big
 * enough. (I'm lazy.)
 *
 * Bug: if the manual page reference is divided on two lines, only
 * the part on the second line is recognized.
 *
 * Part of dwww.
 * Lars Wirzenius.
 */

#include <assert.h>
#include <limits.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <publib.h>

#define UNDERLINE	(0x01 << CHAR_BIT)
#define BOLD		(0x02 << CHAR_BIT)

#define FLAGMASK	((~0) << CHAR_BIT)
#define CHARMASK	(~FLAGMASK)

#define BUF_SIZE 256

#define ispagename(c)	((c) == '_' || (c) == '-' || isalnum(c))

static int manual_page;	/* are we doing a manual page? */

static int find_manual_pages(int *men, int maxmen, int *buf, int n) {
	int c, i, j, m, ok, left_par;

	left_par = -1;
	m = 0;
	for (i = 0; i < n; ++i) {
		c = buf[i] & CHARMASK;
		if (c == '(')
			left_par = i;
		else if (c == ')') {
			if (left_par > 0 && left_par < i-1 &&
			    isdigit(buf[left_par+1] & CHARMASK)) {
				j = left_par + 2;
				ok = (j == i);
				for (; !ok && j < i; ++j)
					ok = isalpha(buf[j] & CHARMASK);
				if (ok) {
					j = left_par-1;
					if (ispagename(buf[j] & CHARMASK)) {
						while (j > 0 && ispagename(buf[j-1] & CHARMASK))
							--j;
						if (m+1 < maxmen) {
							men[m++] = j;
							men[m++] = i;
						}
					}
				}
			}
			left_par = -1;
		}
	}
	return m;
}

static void output_man_anchor(int *buf, int start, int max) {
	int c;

	(void) printf("<a href=\"/cgi-bin/dwww?type=runman&location=");
	while (start < max) {
		c = buf[start] & CHARMASK;
		if (c == '(')
			break;
		(void) printf("%c", c);
		++start;
	}
	(void) printf("/");
	++start;
	while (start < max) {
		c = buf[start] & CHARMASK;
		if (c == ')')
			break;
		(void) printf("%c", c);
		++start;
	}
	(void) printf("\">");
}

static int flush(int *buf, int *i, int *n) {
	int c, j, m, this, prev, nmen;
	int men[128];

	if (manual_page)
		nmen = find_manual_pages(men, sizeof(men)/sizeof(*men), buf, *n);
	else
		nmen = 0;

	prev = 0;
	m = 0;
	for (j = 0; j < *n; ++j) {
		c = buf[j] & CHARMASK;
		if (c == '\n')
			this = 0;
		else
			this = buf[j] & FLAGMASK;
		if (m < nmen && (m % 2) == 0 && j == men[m]) {
			output_man_anchor(buf, men[m], *n);
			++m;
		}
		if (this != prev) {
			if ((this & UNDERLINE) != 0 && (this & BOLD) != 0) {
				if (prev == UNDERLINE)
					this = BOLD;
				else
					this = UNDERLINE;
			}
			if (prev == UNDERLINE)
				(void) printf("</em>");
			else if (prev == BOLD)
				(void) printf("</strong>");
			if (this == UNDERLINE)
				(void) printf("<em>");
			else if (this == BOLD)
				(void) printf("<strong>");
		}
		prev = this;
		switch (c) {
		case '<':
			(void) printf("&lt;"); break;
		case '>':
			(void) printf("&gt;"); break;
		case '&':
			(void) printf("&amp;"); break;
		case '\n':
			(void) printf("\n"); break;
		default:
			(void) printf("%c", c); break;
		}
		if (m < nmen && (m % 2) == 1 && j == men[m]) {
			(void) printf("</a>");
			++m;
		}
	}
	*i = *n = 0;
	if (ferror(stdout))
		return -1;
	return 0;
}

static int add(int *buf, int *i, int *n, int c) {
	if (*i == *n && *n == BUF_SIZE)
		if (flush(buf, i, n) == -1)
			return -1;
	assert(*i < *n || *n < BUF_SIZE);
	if (*i < *n) {
		if (c == '_')
			buf[*i] |= UNDERLINE;
		else if (buf[*i] == '_') {
			buf[*i] |= UNDERLINE;
			buf[*i] = (buf[*i] & FLAGMASK) | (c & CHARMASK);
		} else if (c == buf[*i])
			buf[*i] |= BOLD;
		else
			buf[*i] = c & CHARMASK;
	} else {
		buf[*i] = c & CHARMASK;
		++*n;
	}
	++*i;
	return 0;
}

static int txt2html(FILE *f, char *filename, void *dummy) {
	int buf[BUF_SIZE];
	int c, i, j, n;

	(void) printf("<pre>");

	i = n = 0;
	while ((c = getc(f)) != EOF) {
		switch (c) {
		case '\n':
			if (add(buf, &i, &n, '\n') == -1 ||
			    flush(buf, &i, &n) == -1)
				return -1;
			break;
		case '\b':
			if (i > 0)
				--i;
			break;
		case '\t':
			for (j = 8-(i%8); j > 0; --j)
				if (add(buf, &i, &n, ' ') == -1)
					return -1;
			break;
		default:
			if (add(buf, &i, &n, c) == -1)
				return -1;
			break;
		}
	}

	(void) printf("</pre>");

	if (ferror(stdout))
		return -1;
	return 0;
}

int main(int argc, char **argv) {
	if (argc == 1 || strcmp(argv[1], "--man") != 0)
		manual_page = 0;
	else {
		manual_page = 1;
		--argc;
		++argv;
	}
	if (main_filter(argc-1, argv+1, txt2html, NULL) == -1)
		return EXIT_FAILURE;
	return 0;
}
