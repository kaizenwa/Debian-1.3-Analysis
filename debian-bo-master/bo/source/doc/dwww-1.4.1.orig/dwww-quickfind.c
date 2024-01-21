/*
 * File:	dwww-quickfind.c
 * Purpose:	Find quickly which package a program belongs to.
 * Author:	Lars Wirzenius <liw@iki.fi>
 * Version:	"@(#)dwww:$Id: dwww-quickfind.c,v 1.1.1.1 1996/12/12 06:41:08 jim Exp $"
 * Description:	Builds a database (--build):
 *			line pairs
 *			first is filename (reversed: /bin/ls -> sl/nib/)
 *			second is package name
 */


#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <publib.h>

#include <sys/stat.h>
#include <unistd.h>


struct file {
	char *package;
	char *file;
};


static int file_cmp(const void *a, const void *b) {
	const struct file *aa = a;
	const struct file *bb = b;
	return strcmp(aa->file, bb->file);
}


static void write_db(struct dynarr *files, char *dbfile) {
	struct file *list;
	FILE *f;
	int i;

	qsort(files->data, files->used, sizeof(struct file), file_cmp);
	f = fopen(dbfile, "w");
	if (f == NULL)
		errormsg(1, -1, "couldn't create %s", dbfile);
	list = files->data;
	for (i = 0; i < files->used; ++i)
		fprintf(f, "%s\n%s\n", list[i].file, list[i].package);
	if (ferror(f))
		errormsg(1, -1, "error writing to %s", dbfile);
	fclose(f);
}


static void add_file(struct dynarr *files, char *package, char *file) {
	struct file *f;

	if (dynarr_resize(files, files->used + 1) == -1)
		errormsg(1, -1, "out of memory");

	f = files->data;
	f[files->used].package = package;
	f[files->used].file = file;
	++files->used;
}


static void read_db(struct dynarr *files, char *dbfile) {
	FILE *f;
	char *file, *pkg;

	f = fopen(dbfile, "r");
	if (f == NULL)
		errormsg(1, -1, "couldn't open %s", dbfile);
	while ((file = getaline(f)) != NULL && (pkg = getaline(f)) != NULL)
		add_file(files, pkg, file);
	if (ferror(f))
		errormsg(1, -1, "error reading %s", dbfile);
	fclose(f);
}


/* kludge */
static int name_is_ok(const char *p) {
	static char *tab[] = {
		"/bin/",
		"/sbin/",
		"/usr/games/",
	};
	static int n = sizeof(tab) / sizeof(*tab);
	int i;
	
	for (i = 0; i < n; ++i)
		if (strstr(p, tab[i]) != NULL)
			return 1;
	return 0;
}


static void build(char *dbfile) {
	struct dynarr files;
	char *p, *line;
	struct stat st;
	
	dynarr_init(&files, sizeof(struct file));
	while ((line = getaline(stdin)) != NULL) {
		p = strchr(line, ':');
		if (p == NULL)
#if 0
			errormsg(1, 0, "syntax error in input: no colon");
#else
			continue;
#endif
		*p++ = '\0';
		strtrim(line);
		strtrim(p);
		if (name_is_ok(p) && stat(p, &st) != -1) {
			if (S_ISREG(st.st_mode) && (st.st_mode & 0111) != 0) {
				strrev(p);
				add_file(&files, line, p);
			}
		} else
			free(line);
	}
	write_db(&files, dbfile);
}


static int find_cmp(const void *a, const void *b) {
	const struct file *aa = a;
	const struct file *bb = b;
	int c, i, n, alen, blen;
	
	alen = strlen(aa->file);
	blen = strlen(bb->file);
	n = (alen < blen) ? alen : blen;

	i = strncmp(aa->file, bb->file, n);
	if (i != 0)
		return i;
	if (aa->file[n] == '\0' && bb->file[n] == '\0')
		return 0;
	if (aa->file[n] == '/' || bb->file[n] == '/')
		return 0;
	c = aa->file[n];
	if (c == '\0')
		c = '/';
	if (c < bb->file[n])
		return -1;
	assert(c != bb->file[n]);
	return 1;
}


static void find(char *program, char *dbfile) {
	struct dynarr files;
	struct file *p, key;
	
	dynarr_init(&files, sizeof(struct file));
	read_db(&files, dbfile);
	strrev(program);
	key.file = program;
	p = bsearch(&key, files.data, files.used, sizeof(struct file),
		find_cmp);
	if (p != NULL)
		printf("%s\n", p->package);
}


int main(int argc, char **argv) {
	set_progname(argv[0], "dwww-quickfind");

	if (argc != 3)
		errormsg(1, 0, "Error: wrong number of arguments");
		
	if (strcmp(argv[1], "--build") == 0)
		build(argv[2]);
	else
		find(argv[1], argv[2]);
		
	return 0;
}
