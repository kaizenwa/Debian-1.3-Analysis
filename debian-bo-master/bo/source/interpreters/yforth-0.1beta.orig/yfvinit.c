/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name:     yfvinit.c
 * Abstract:        Initialize the vocabulary.
 */

#include <string.h>
#include "yforth.h"
#include "core.h"
#include "ycore.h"
#if COREE_DEF
#	include "coree.h"
#endif
#if DOUBLE_DEF
#	include "double.h"
#endif
#if DOUBLEE_DEF
#	include	"doublee.h"
#endif
#if FLOAT_DEF
#	if !COREE_DEF
#		include "coree.h"
#	endif
#	include "float.h"
#endif
#if FLOATE_DEF
#	include	"floate.h"
#endif
#if MEMALL_DEF
#	include "memall.h"
#endif
#if SEARCH_DEF
#	include "search.h"
#endif
#if SEARCHE_DEF
#	include "searche.h"
#endif
#if TOOLS_DEF
#	include "tools.h"
#endif
#if TOOLSE_DEF
#	if !COREE_DEF
#		include "coree.h"
#	endif
#	include "toolse.h"
#endif
#if LOCALS_DEF
#	include "locals.h"
#endif
#if LOCALSE_DEF
#	include "localse.h"
#endif
#if FACILITY_DEF
#	include "facility.h"
#endif
#if FACILITYE_DEF
#	include "facilite.h"
#endif
#if STRING_DEF
#	include "string.h"
#endif
#if FILE_DEF
#	include "file.h"
#endif
#if	FILEE_DEF
#	include "filee.h"
#endif
#if BLOCK_DEF
#	include "block.h"
#endif
#if BLOCKE_DEF
#	include "blocke.h"
#endif
#if EXCEPTION_DEF
#	include "exceptio.h"
#endif
#if EXCEPTIONE_DEF
#	include "excepte.h"
#endif

static struct raw_voc iv[] = {
#define DECLARE_WORDS

#include "core.h"
#include "ycore.h"
#if COREE_DEF
#	include "coree.h"
#endif
#if DOUBLE_DEF
#	include "double.h"
#endif
#if DOUBLEE_DEF
#	include	"doublee.h"
#endif
#if FLOAT_DEF
#	include "float.h"
#endif
#if FLOATE_DEF
#	include	"floate.h"
#endif
#if MEMALL_DEF
#	include "memall.h"
#endif
#if SEARCH_DEF
#	include "search.h"
#endif
#if SEARCHE_DEF
#	include "searche.h"
#endif
#if TOOLS_DEF
#	include "tools.h"
#endif
#if TOOLSE_DEF
#	include "toolse.h"
#endif
#if LOCALS_DEF
#	include "locals.h"
#endif
#if LOCALSE_DEF
#	include "localse.h"
#endif
#if FACILITY_DEF
#	include "facility.h"
#endif
#if FACILITYE_DEF
#	include "facilite.h"
#endif
#ifdef STRING_DEF
#	include "string.h"
#endif
#if FILE_DEF
#	include "file.h"
#endif
#if	FILEE_DEF
#	include "filee.h"
#endif
#if BLOCK_DEF
#	include "block.h"
#endif
#if BLOCKE_DEF
#	include "blocke.h"
#endif
#if EXCEPTION_DEF
#	include "exceptio.h"
#endif
#if EXCEPTIONE_DEF
#	include "excepte.h"
#endif

	{ 0, 0, 0 },
};

#undef DECLARE_WORDS

/* init_vocabulary: loads words into the real dictionary from the table
 * builded by including all the header files after the declaration of
 * DECLARE_WORDS. See the header files such as "core.h" and the macro
 * file "macro.h" for the implementation of this.
 * This function returns the dictionary pointer after loading.
 */
void init_vocabulary(Char **dp) {
	struct word_def *w;
	Char *name;
	int i = 0;
    while (iv[i].name) {                /* Last name is a NULL (see table above) */
        name = *dp;                     /* "name" is a ptr to the name */
        **dp = strlen(iv[i].name);      /* first copy length... */
        strcpy(*dp + 1, iv[i].name);    /* ...and then the actual name */
        *dp = (Char *) WORD_PTR(*dp);   /* advance "dp" */
        w = (struct word_def *) *dp;    /* here begins the structure */
        w->name = name;                 /* adjust pointer... */
        w->class = iv[i].class;         /* ...and the class of the word */
        ins_word(w);                    /* Finally adjust the link field... */
        mark_word(w);                   /* ...accordingly with the hash function
                                           and make the word visible */
        *dp += sizeof(struct word_def); /* advance "dp" */
        switch (iv[i].class & A_WORD) {     /* The last field must be adjusted here */
			case A_PRIMITIVE:
				w->func[0] = iv[i].func;
				break;
			case A_USER:
				w->func[0] = (pfp) iv[i].func;
				break;
		}
		i++;
	}
}

