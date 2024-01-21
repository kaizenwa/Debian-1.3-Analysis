/*
 * setfont.c - Eugene Crosser & Andries Brouwer
 *
 * Version 0.92
 *
 * Loads the console font, and possibly the corresponding screen map(s).
 * We accept two kind of screen maps, one [-m] giving the correspondence
 * between some arbitrary 8-bit character set currently in use and the
 * font positions, and the second [-u] giving the correspondence between
 * font positions and Unicode values.
 */
#define VERSION "0.92"

#include <stdio.h>
#include <memory.h>
#include <fcntl.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/kd.h>
#include "paths.h"
#include "psf.h"

char *progname;

static void position_codepage(int iunit, FILE *fpi);
static void saveoldfont(int fd, char *ofil);
static void loadnewfont(int fd, char *ifil, int iunit, int no_m, int no_u);
extern void saveoldmap(int fd, char *omfil);
extern void loadnewmap(int fd, char *mfil);
extern void saveunicodemap(int fd, char *oufil);
extern void loadunicodemap(int fd, char *ufil);
extern void activatemap(void);
extern int getfd(void);
extern char *malloc();

int verbose = 0;

/* search for the font in these directories (with trailing /) */
char *fontdirpath[] = { "", DATADIR "/" FONTDIR "/", 0 };
char *fontsuffixes[] = { "", ".psf", ".cp", ".fnt", ".psfu", 0 };

static inline FILE*
findfont(char *fnam) {
    return findfile(fnam, fontdirpath, fontsuffixes);
}

void
usage(void)
{
        fprintf(stderr, "\
Usage:  %s [-o oldfont] [-fontsize] [newfont] [-m consolemap] [-u unicodemap]
If no -o is given, and no newfont, then the font \"default\" is loaded.
Explicitly (with -m or -u) or implicitly (in the fontfile) given mappings will
be loaded and, in the case of consolemaps, activated.
Options:
 -8, -14, -16	Choose the right font from a codepage that contains three fonts
                (or choose default font, e.g. \"default8x16\").
 -o <filename>	Write current font to file before loading a new one.
 -om <filename>	Write current consolemap to file before loading a new one.
 -ou <filename>	Write current unicodemap to file before loading a new one.
 -m none	Suppress loading and activation of a mapping table.
 -u none	Suppress loading of a unicode map.
 -v		Verbose.
 -V		Print version.
", progname);
	exit(1);
}

int
main(int argc, char *argv[])
{
	char *ifil, *mfil, *ufil, *ofil, *omfil, *oufil;
	int fd, i, iunit, no_m, no_u;

	progname = argv[0];

	fd = getfd();

	ifil = mfil = ufil = ofil = omfil = oufil = 0;
	iunit = 0;
	no_m = no_u = 0;

	for (i = 1; i < argc; i++) {
	    if (!strcmp(argv[i], "-V")) {
	        printf("setfont version %s\n", VERSION);
	    } else if (!strcmp(argv[i], "-v")) {
	        verbose = 1;
	    } else if (!strcmp(argv[i], "-o")) {
		if (++i == argc || ofil)
		  usage();
		ofil = argv[i];
	    } else if (!strcmp(argv[i], "-om")) {
		if (++i == argc || omfil)
		  usage();
		omfil = argv[i];
	    } else if (!strcmp(argv[i], "-ou")) {
		if (++i == argc || oufil)
		  usage();
		oufil = argv[i];
	    } else if (!strcmp(argv[i], "-m")) {
		if (++i == argc || mfil)
		  usage();
		if (!strcmp(argv[i], "none"))
		  no_m = 1;
		else
		  mfil = argv[i];
	    } else if (!strcmp(argv[i], "-u")) {
		if (++i == argc || ufil)
		  usage();
		if (!strcmp(argv[i], "none"))
		  no_u = 1;
		else
		  ufil = argv[i];
	    } else if(argv[i][0] == '-') {
		iunit = atoi(argv[i]+1);
		if(iunit <= 0 || iunit > 32)
		  usage();
	    } else {
		if (ifil)
		  usage();
		ifil = argv[i];
	    }
	}

	if (!ifil && !mfil && !ufil && !ofil && !omfil && !oufil)
	  /* reset to some default */
	  ifil = "";

	if (ofil)
	  saveoldfont(fd, ofil);

	if (omfil)
	  saveoldmap(fd, omfil);

	if (oufil)
	  saveunicodemap(fd, oufil);

	if (mfil) {
	    loadnewmap(fd, mfil);
	    activatemap();
	    no_m = 1;
	}

	if (ufil)
	  no_u = 1;

	if (ifil)
	  loadnewfont(fd, ifil, iunit, no_m, no_u);

	if (ufil)
	  loadunicodemap(fd, ufil);

	return 0;
}

void
do_loadfont(int fd, int unit, int fontsize, FILE *fpi) {
	char buf[16384];
	int i;

	memset(buf,0,sizeof(buf));

	if (unit < 1 || unit > 32) {
	    fprintf(stderr, "Bad character size %d\n", unit);
	    exit(1);
	}

	for (i = 0; i < fontsize; i++)
	  if (fread(buf+(32*i), unit, 1, fpi) != 1) {
	      perror("Cannot read font from file");
	      exit(1);
	  }

	if (verbose)
	  printf("Loading 8x%d font from file %s\n", unit, pathname);

#ifdef PIO_FONTX
	{
	    struct consolefontdesc cfd;

	    cfd.charcount = fontsize;
	    cfd.charheight = unit;
	    cfd.chardata = buf;

	    if (ioctl(fd,PIO_FONTX,&cfd) == 0)
	      return;		/* success */
	    perror("PIO_FONTX ioctl error (trying PIO_FONT)");
	}
#endif
	if (ioctl(fd,PIO_FONT,buf)) {
	    perror("PIO_FONT ioctl error");
	    exit(1);
	}
}

void
do_loadtable(int fd, int tailsz, int fontsize, FILE *fpi) {
	struct unimapinit advice;
	struct unimapdesc ud;
	struct unipair *up;
	int ct = 0, maxct;
	int glyph, i;
	u_short unicode;

	maxct = tailsz;		/* more than enough */
	up = (struct unipair *) malloc(maxct * sizeof(struct unipair));
	if (!up) {
	    fprintf(stderr, "Out of memory?\n");
	    exit(1);
	}
	for (glyph = 0; glyph < fontsize; glyph++) {
	    while ((i = fread(&unicode, sizeof(unicode), 1, fpi)) == 1 &&
		   unicode != PSF_SEPARATOR) {
		if (ct >= maxct) { /* impossible */
		    i = 0;
		    break;
		}
		up[ct].unicode = unicode;
		up[ct].fontpos = glyph;
		ct++;
	    }
	    if (i != 1) {
		fprintf(stderr, "Input error reading unicode table\n");
		exit(1);
	    }
	}

	advice.advised_hashsize = 0;
	advice.advised_hashstep = 0;
	advice.advised_hashlevel = 0;
	if(ioctl(fd, PIO_UNIMAPCLR, &advice)) {
#ifdef ENOIOCTLCMD
	    if (errno == ENOIOCTLCMD) {
		fprintf(stderr, "It seems this kernel is older than 1.1.92\n");
		fprintf(stderr, "No Unicode mapping table loaded.\n");
	    } else
#endif
	      perror("PIO_UNIMAPCLR");
	    exit(1);
	}
	if (verbose)
	  printf("Loading Unicode mapping table...\n");
	ud.entry_ct = ct;
	ud.entries = up;
	if(ioctl(fd, PIO_UNIMAP, &ud)) {
#if 0
	    if (errno == ENOMEM) {
		/* change advice parameters */
	    }
#endif
	    perror("PIO_UNIMAP");
	    exit(1);
	}
}

void
loadnewfont(int fd, char *ifil, int iunit, int no_m, int no_u) {
	FILE *fpi;
	char defname[20];
	int size, unit;
	struct stat stbuf;

	if (!*ifil) {
	    /* try to find some default file */

	    if (iunit < 0 || iunit > 32)
	      iunit = 0;
	    if (iunit == 0) {
		if ((fpi = findfont(ifil = "default")) == NULL &&
		    (fpi = findfont(ifil = "default8x16")) == NULL &&
		    (fpi = findfont(ifil = "default8x14")) == NULL &&
		    (fpi = findfont(ifil = "default8x8")) == NULL) {
		    fprintf(stderr, "Cannot find default font\n");
		    exit(1);
		}
	    } else {
		sprintf(defname, "default8x%d", iunit);
		if ((fpi = findfont(ifil = defname)) == NULL) {
		    fprintf(stderr, "Cannot find %s font\n", ifil);
		    exit(1);
		}
	    }
	} else {
	    if ((fpi = findfont(ifil)) == NULL) {
		fprintf(stderr, "Cannot open font file %s\n", ifil);
		exit(1);
	    }
	}

	if (stat(pathname, &stbuf)) {
	    perror(pathname);
	    exit(1);
	}
	size = stbuf.st_size;

	/* test for psf first */
	{
	    struct psf_header psfhdr;
	    int fontsize;
	    int hastable;
	    int head;

	    if (fread(&psfhdr, sizeof(struct psf_header), 1, fpi) != 1) {
		perror("Error reading header input font");
		exit(1);
	    }
	    if (psfhdr.magic != PSF_MAGIC) {
		fseek(fpi,0,SEEK_SET);
		goto no_psf;
	    }
	    if (psfhdr.mode > PSF_MAXMODE) {
		fprintf(stderr, "Unsupported psf file mode\n");
		exit(1);
	    }
	    fontsize = ((psfhdr.mode & PSF_MODE512) ? 512 : 256);
#ifndef PIO_FONTX
	    if (fontsize != 256) {
		fprintf(stderr, "Only fontsize 256 supported\n");
		exit(1);
	    }
#endif
	    hastable = (psfhdr.mode & PSF_MODEHASTAB);
	    unit = psfhdr.charsize;
	    head = sizeof(struct psf_header) + fontsize*unit;
	    if (head > size || (!hastable && head != size)) {
		fprintf(stderr, "Input file: bad length\n");
		exit(1);
	    }
	    do_loadfont(fd, unit, fontsize, fpi);
	    if (hastable && !no_u)
	      do_loadtable(fd, size-head, fontsize, fpi);
	    fclose(fpi);
	    return;
	}
      no_psf:

	/* file with three code pages? */
	if (size == 9780) {
	    position_codepage(iunit, fpi);
	    unit = iunit;
	} else {
	    /* bare font */
	    if (size & 0377) {
		fprintf(stderr, "Bad input file size\n");
		exit(1);
	    }
	    unit = size/256;
	}
	do_loadfont(fd, unit, 256, fpi);
	fclose(fpi);
}

static void
position_codepage(int iunit, FILE *fpi) {
        int offset;

	/* code page: first 40 bytes, then 8x16 font,
	   then 6 bytes, then 8x14 font,
	   then 6 bytes, then 8x8 font */

	if (!iunit) {
	    fprintf(stderr, "\
This file contains 3 fonts: 8x8, 8x14 and 8x16. Please indicate
using an option -8 or -14 or -16 which one you want loaded.\n");
	    exit(1);
	}
	switch (iunit) {
	  case 8:
	    offset = 7732; break;
	  case 14:
	    offset = 4142; break;
	  case 16:
	    offset = 40; break;
	  default:
	    fprintf(stderr, "\
You asked for font size %d, but only 8, 14, 16 are possible here.\n",
		    iunit);
	    exit(1);
	}
	if(fseek(fpi,offset,SEEK_SET)) {
	    perror("seek error on input file");
	    exit(1);
	}
}

static void
saveoldfont(int fd, char *ofil) {
    FILE *fpo;
    int i, unit;
    char buf[8192];

    if((fpo = fopen(ofil, "w")) == NULL) {
	perror(ofil);
	exit(1);
    }
    i = ioctl(fd,GIO_FONT,buf);
    if (i) {
	perror("GIO_FONT ioctl error");
	exit(1);
    }

    /* save as efficiently as possible */
    for (unit = 32; unit > 0; unit--)
      for (i = 0; i < 256; i++)
	if (buf[32*i+unit-1])
	  goto nonzero;
  nonzero:
    if (unit == 0)
      fprintf(stderr, "Found nothing to save\n");
    else {
	for (i = 0; i < 256; i++)
	  if (fwrite(buf+(32*i), unit, 1, fpo) != 1) {
	      perror("Cannot write font file");
	      exit(1);
	  }
	if (verbose)
	  printf("Saved 8x%d font file on %s\n", unit, ofil);
    }
    fclose(fpo);
}

/* Only on the current console? On all allocated consoles? */
/* A newly allocated console has NORM_MAP by default -
   probably it should copy the default from the current console?
   But what if we want a new one because the current one is messed up? */
/* For the moment: only the current console, only the G0 set */
void
activatemap(void) {
    printf("\033(K");
}

void
disactivatemap(void) {
    printf("\033(B");
}
