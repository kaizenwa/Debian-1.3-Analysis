#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

int highscore[300];
int newscore[300];

#define MAGICH	0x741d	/* magic of xsok version 1 highscore file */

static void portable_to_internal(long *args, unsigned char *p, int num) {
    do {
	int j;
	*args = 0;
	for (j = 0; j < 4; ++j)
	    *args += (long)p[3-j] << (j << 3);
	++args;
	p += 4;
    } while (--num);
}
static void internal_to_portable(unsigned char *p, long *args, int num) {
    do {
	int j;
	memset(p, (*args < 0 ? -1 : 0), 4);
	for (j = 0; j < 4; ++j)
	    p[3-j] = (unsigned char)(*args >> (j << 3));
	++args;
	p += 4;
    } while (--num);
}
static void ReadHighscores(const char *filename) {
    FILE *fp;
    int i;
    memset(highscore, 0, sizeof(highscore));
    for (i = 100; i < 300; ++i)
	    highscore[i] = 0x7fffffff;
    if ((fp = fopen(filename, "rb"))) {
	long p[300];
	int num;
	unsigned char s[1200];
	num = fread(s, 4, 300, fp);
	portable_to_internal(p, s, num);	
	for (i = 0; i < num; ++i)
	    highscore[i] = p[i];
	fclose(fp);
    }
    highscore[0] = MAGICH;
}
static void WriteHighscores(const char *filename) {
    FILE *fp;
    if ((fp = fopen(filename, "wb"))) {
	int i;
	long p[300];
	unsigned char s[1200];
	for (i = 0; i < 300; ++i)
	    p[i] = highscore[i];
	internal_to_portable(s, p, 300);
	fwrite(s, 1, 1200, fp);
	fclose(fp);
    }
}

static void mergescores(const char *filename) {
    const char *base_name;
    int i;
    base_name = strrchr(filename, '/');
    if (!base_name) {
	fprintf(stderr, "need directory components for %s\n", filename);
	exit(1);
    }
    ++base_name;
    ReadHighscores(filename);
    memcpy(newscore, highscore, sizeof(highscore));
    ReadHighscores(base_name);
    for (i = 1; i < 100; ++i)
	if (newscore[i] > highscore[i])
	     highscore[i] = newscore[i];
    for (i = 100; i < 300; ++i)
	if (newscore[i] < highscore[i])
	     highscore[i] = newscore[i];
    WriteHighscores(filename);
}


int main(int argc, char *argv[]) {
    int i;
    for (i = 1; i < argc; ++i)
	mergescores(argv[i]);
    return 0;
}
