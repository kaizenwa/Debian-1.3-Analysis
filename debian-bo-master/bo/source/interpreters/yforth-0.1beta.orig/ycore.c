/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name:     ycore.c
 * Abstract:        Words defined for this particular implementation of
                    forth. Do not expect to find these words in other
                    implementations.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "ver.h"
#include "yforth.h"
#include "core.h"
#include "file.h"
#include "search.h"

/**************************************************************************/
/* WORDS DEFINITION *******************************************************/
/**************************************************************************/

/* ( --- ) print current version of yForth? */
void _yforth_version() {
	print_version();
}

/* ( c-addr u --- ) save a snapshot of the current dictionary and vocabulary
 * search order
 */
void _save_image() {
	FILE *f = fopen(get_file_name(), "wb");
	struct image_header hd;
	struct voc_marker vm;
	if (f) {
		memset(&hd, 0, sizeof(struct image_header));
		strcpy(hd.header, "yForth? Image File\n");
		hd.ver_hi = VER_HI;
		hd.ver_lo = VER_LO;
		hd.base = dp0;
		hd.dspace_size = dspace_size;
		hd.pattern = VERSION_PATTERN;
		if (fwrite(&hd, sizeof(struct image_header), 1, f) < 1) _error = E_NOFILE;
		else {
			save_vocabulary(&vm);
			if (fwrite(&vm, sizeof(struct voc_marker), 1, f) < 1) _error = E_NOFILE;
			else {
				if (fwrite(dp0, sizeof(Cell), dspace_size, f) < dspace_size)
					_error = E_NOFILE;
			}
		}
		fclose(f);
	} else _error = E_NOFILE;
}

/* ( c-addr u --- n ) execute command pointeb by c-addr via "system", n is 
 * the result of operation as described in the C library manual 
 */
void _system() {
	register UCell len = *sp++;
	register Char *name = (Char *) *sp; 
	extern Char s_tmp_buffer[];
	memcpy(s_tmp_buffer, name, len);
	s_tmp_buffer[len] = '\0';
	*sp = system(s_tmp_buffer);
}

