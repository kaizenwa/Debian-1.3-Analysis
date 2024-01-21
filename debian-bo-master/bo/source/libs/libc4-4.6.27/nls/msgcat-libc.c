/* Some routines for Linux libc (and probably others).
 * Covered by GNU LIBC LICENCE.
 *
 * Mitch D'Souza. (m.dsouza@mrc-apu.cam.ac.uk)
 */

#ifdef __linux__
#include <stdio.h>
#include <locale.h>
#include "nl_types.h"
#define CATALOGUE "libc"
nl_catd _libc_cat = 0;

void libc_nls_init(void) {
    extern nl_catd _libc_cat;

    if (_libc_cat != 0)		/* 
				 * Either a previous catopen() failed
				 * or the libc catalog is already open
				 */
	return;
				/* This is the first time we try. */

	/* Get the environment setting for LC_MESSAGES */
	setlocale(LC_MESSAGES,"");
	_libc_cat=catopen(CATALOGUE,0);
}

#endif
