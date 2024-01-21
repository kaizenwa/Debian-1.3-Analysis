#include <stdlib.h>
#include <stdio.h>
#include <vga.h>


/*
 * Note: Observe that when writing the font to a file, the file to write is
 * opened after vga_init has been called (so that root permissions have been
 * given up). This means that there is no major security hole lurking here.
 */

#define FONT_SIZE 8192

unsigned char font[FONT_SIZE];

void main(int argc, char *argv[])
{
    FILE *f;

    if (argc == 1) {
	printf("Restore corrupted textmode font.\n");
	printf("Syntax: restorefont option filename\n");
	printf("	-r filename	Restore VGA font from file.\n");
	printf("	-w filename	Write current VGA font to file.\n");
	exit(0);
    }
    if (argv[1][0] != '-') {
	printf("Must specify -r or -w.\n");
	exit(1);
    }
    switch (argv[1][1]) {
    case 'r':
    case 'w':
	if (argc != 3) {
	    printf("Must specify filename.\n");
	    exit(1);
	}
	break;
    default:
	printf("Invalid option. Must specify -r or -w.\n");
	exit(1);
    }
    vga_disabledriverreport();
    vga_setchipset(VGA);	/* avoid SVGA detection */
    vga_init();
    vga_setmode(G640x350x16);
    switch (argv[1][1]) {
    case 'r':
	f = fopen(argv[2], "rb");
	if (f == NULL) {
	  error:
	    perror("restorefont");
	  ex_no_errno:
	    vga_setmode(TEXT);
	    exit(1);
	}
	if (1 != fread(font, FONT_SIZE, 1, f)) {
	    if (errno)
		goto error;
	    puts("restorefont: input file corrupted.");
	    goto ex_no_errno;
	}
	fclose(f);
	vga_puttextfont(font);
	break;
    case 'w':			/* this line was missing */
	vga_gettextfont(font);
	f = fopen(argv[2], "wb");
	if (f == NULL)
	    goto error;
	if (1 != fwrite(font, FONT_SIZE, 1, f))
	    goto error;
	if (fclose(f))
	    goto error;
	break;
    }
    vga_setmode(TEXT);
    exit(0);
}
