/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name:     yforth.c
 * Abstract:        Main program
 */

#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include "yforth.h"
#include "defaults.h"
#include "core.h"
#include "block.h"
#include "search.h"
#include "ver.h"
#include "file.h"

jmp_buf warm_start_jump, cold_start_jump;

Char *dp0;                          /* Data-Space base pointer */
Cell dspace_size = DEF_DSPACE_SIZE; /* Data-Space size */
Cell dstack_size = DEF_DSTACK_SIZE, /* Data-Stack size */
     rstack_size = DEF_RSTACK_SIZE, /* Return-stack size */
     fstack_size = DEF_FSTACK_SIZE; /* Floating-stack size */
Cell tib_size    = DEF_TIB_SIZE;    /* TIB size */
Cell in_pnos, pnos_size;            /* Pictured Numeric Output String */
Char *pnos, *p_pnos;                /* Ptrs inside PNOS */
Cell pad_size    = DEF_PAD_SIZE;    /* PAD size */

static char *file_name,             /* Ptr to file name on command line, if present */
            *image_file;            /* Ptr to image file name on cmd line, if present */

static int silent,
    image_file_loaded;

static struct image_header hd;

void print_version() {
    printf("yForth? v%d.%d%s - Written by Luca Padovani (C) 1996.\n\
This software is Freeware, use it at your own risk.\n",
	VER_HI, VER_LO, VER_TEST);
}

void print_help(void) {
	print_version();
	printf("Usage: yForth [options] [file name]\n\
-d<n>	Data-Space size         -s<n>	Data-Stack size\n\
-r<n>	Return-Stack size       -f<n>	Floating-Stack size\n\
-t<n>	TIB size                -p<n>	PAD size\n\
-h,-H	This help               -q   	Quiet\n\
-i<file> Image file\n\
All sizes are expressed in cells.\n");
}

/* do_parameters: processes parameters passed on command line */
void do_parameters(int argc, char *argv[]) {
	int i = 1;
    while (argc-- > 1) {
		if (argv[i][0] == '-')
			switch (argv[i][1]) {
				case 'd': dspace_size = atoi(argv[i] + 2); break;
				case 's': dstack_size = atoi(argv[i] + 2); break;
				case 'r': rstack_size = atoi(argv[i] + 2); break;
				case 'f': fstack_size = atoi(argv[i] + 2); break;
				case 't': tib_size = atoi(argv[i] + 2); break;
				case 'p': pad_size = atoi(argv[i] + 2); break;
				case 'q': silent = 1; break;
				case 'i': image_file = argv[i] + 2; break;
				case 'h':
				case 'H':
					print_help();
					exit(0);
					break;
				default:
                    fprintf(stderr, "%c unknown option, use -h for help.\n");
					exit(0);
					break;
			}
		else {
			file_name = argv[i];
			break;
		}
	}
}

/* default_parameters: adjust environment parameters in case they do not
 * fall into required range
 */
void default_parameters(void) {
	dspace_size = max(MIN_DSPACE_SIZE, dspace_size);
	dstack_size = max(MIN_DSTACK_SIZE, dstack_size);
	rstack_size = max(MIN_RSTACK_SIZE, rstack_size);
	fstack_size = max(MIN_FSTACK_SIZE, fstack_size);
	tib_size = max(MIN_TIB_SIZE, tib_size);
	pad_size = max(MIN_PAD_SIZE, pad_size);
}

/* load_image_file: loads image file named "name" into the dictionary. Loading
 * is divided in two parts: when "header" is set to 1 the file is opened and
 * the header is loaded into the structure "hd". Then some checks are made
 * to adjust parameters in case of a corrupted image.
 * Finally, when "load_image_file" is called with "header" set to 0, the
 * actual loading is performed. Note that pointers inside the dictionary
 * are absolute, so an image file can be loaded only if the allocated
 * memory is placed at the same address when it's been saved. Furthermore,
 * the same image file cannot be loaded thru different version of the
 * executable file "yForth".
 */
int load_image_file(char *name, int header) {
	FILE *f = fopen(name, "rb");
	int res = 1;
	if (f) {
		if (header) {
			if (fread(&hd, sizeof(struct image_header), 1, f)) {
				if (hd.ver_hi != VER_HI || hd.ver_lo != VER_LO)
					if (!silent)
                        fprintf(stderr, "Warning: different image file version (%d.%d).\n",
						hd.ver_hi, hd.ver_lo);
				if (hd.pattern != VERSION_PATTERN)
					if (!silent)
						fprintf(stderr, "Warning: different version pattern (Image: %x).\n",
							hd.pattern); 
				res = 0;
            } else fprintf(stderr, "Error: can't read image file header.\n");
		} else {
			fseek(f, sizeof(struct image_header), SEEK_SET);
			if (hd.base == dp0) {
				struct voc_marker vm;
				if (fread(&vm, sizeof(struct voc_marker), 1, f) < 1 ||
					fread(dp0, sizeof(Cell), hd.dspace_size, f) != hd.dspace_size)
                    fprintf(stderr, "Error: can't read image file.\n");
				else {
					load_vocabulary(&vm);
					res = 0;
				}
            } else fprintf(stderr, "Error: can't load image file with base %u at %u.\n",
				hd.base, dp0);
		}
		fclose(f);
    } else fprintf(stderr, "Can't open image file (%s).\n", name);
	return (res);
}

main(int argc, char *argv[]) {
	do_parameters(argc, argv);
	if (image_file) {
		if (load_image_file(image_file, 1)) exit(-1);
	} else fopen(argv[0], "rb");
	/* !!! WARNING !!! Previous line opens a file even if no image-file
	 * is specified. This is because in some system data space would 
	 * result unaligned in subsequent loadings. I have to find a more
	 * smart trick here...
	 */ 

	default_parameters();
#if BLOCK_DEF
	open_block_file("YFORTH.BLK");
#endif
	init_stacks(dstack_size, rstack_size, fstack_size);
	if (image_file && dspace_size < hd.dspace_size) {
		if (!silent)
            fprintf(stderr, "Warning: can't restrict dictionary to %u cells, now is %u cells.\n",
			dspace_size, hd.dspace_size);
		dspace_size = max(dspace_size, hd.dspace_size);
	}
	init_data_space(dspace_size);
	init_tib(tib_size);
	init_pad(pad_size);
	init_pnos();
	init_signals();

	silent |= setjmp(cold_start_jump);
	if (image_file)
		if (load_image_file(image_file, 0)) exit(-1);

    /* Note that after a cold start the vocabulary is reloaded */

	if (!silent) {
		print_version();
		/*
		printf("Cell: %d  Double-Cell: %d  Char: %d  Real: %d\n",
			sizeof(Cell), sizeof(DCell), sizeof(Char), sizeof(Real));
		*/
	}
	init_forth_environment(!image_file);
	if (!setjmp(warm_start_jump) && file_name) load_file(file_name);
	_quit();
	return 0;
}

