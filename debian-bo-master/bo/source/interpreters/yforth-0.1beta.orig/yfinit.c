/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name:     yfinit.c
 * Abstract:        Allocate memory for the main structures of the
 *                  environment and initialize the environment itself.
 */

#include <stdlib.h>
#include <malloc.h>
#include <stdio.h>
#include "yforth.h"
#include "core.h"
#include "coree.h"
#include "search.h"

/* init_stacks: allocate memory for the stacks */
void init_stacks(int dstack_size, int rstack_size, int fstack_size) {
	sp_base = (Cell *) malloc(dstack_size * sizeof(Cell));
	rp_base = (Cell *) malloc(rstack_size * sizeof(Cell));
	fp_base = (Real *) malloc(fstack_size * sizeof(Real));
    if (sp_base && rp_base && (fp_base || !fstack_size)) {
		sp = sp_top = sp_base + dstack_size;
		rp = rp_top = rp_base + rstack_size;
		fp = fp_top = fp_base + fstack_size;
	} else {
        fprintf(stderr, "Stack sizes: %d %d %d. Not enough memory.\n", dstack_size,
			rstack_size, fstack_size);
		exit(-1);
	}
}

/* init_data_space: allocate memory for the data-space dictionary */
void init_data_space(int dspace_size) {
	dp0 = _dp = (Char *) malloc(dspace_size * sizeof(Cell));
	if (!_dp) {
		printf("Data Space size: %d. Not enough memory.\n", dspace_size);
		exit(-1);
	}
}

/* init_tib: allocate memory for the TIB */
void init_tib(int size) {
	_tib = (Char *) malloc(size * sizeof(Char));
	if (!_tib) {
        fprintf(stderr, "Tib size: %d. Not enough memory.\n", size);
		exit(-1);
	}
}

/* init_pad: allocate memory for the PAD */
void init_pad(int size) {
	_pad = (Char *) malloc(size * sizeof(Char));
	if (!_pad) {
        fprintf(stderr, "PAD size: %d. Not enough memory.\n", size);
		exit(-1);
	}
}

/* init_pnos: allocate memory for PNOS, note that the size of PNOS is
 * determined by the actual size of a double cell.
 */
void init_pnos() {
    pnos_size = sizeof(DCell) * 8 + 2;      /* plus a space and eventually a '-' */
	pnos = (Char *) malloc(pnos_size * sizeof(Char));
	if (!pnos) {
        fprintf(stderr, "Can't allocate PNOS.\n");
		exit(-1);
	}
}

/* init_forth_environment: perform actual inizialization of the dictionary
 * only if "reload" is true, then initialize the value of variables
 * of the forth environment. This variable must be initialized even in
 * the case of an image file since they're not inside the dictionary,
 * but are simply C variables.
 */
void init_forth_environment(int reload) {
	if (reload) {
		struct vocabulary *v;
		_wordlist();
		list[0] = forth_wid = voc = (struct vocabulary *) *sp++;
		_last = NULL;
		init_vocabulary(&_dp);
	}
	_base = 10;
	_env_slash_counted_string = (1 << (8 * sizeof(Char))) - 1;
	_env_slash_hold = 		pnos_size;
	_env_slash_pad = 		pad_size;
	_env_address_unit_bits = 8 * sizeof(Char);
	_env_core = 			FFLAG(1);
	_env_core_ext =     	FFLAG(COREE_DEF);
	_env_floored = 			FFLAG(FLOORED_DIVISION);
	_env_max_char =			_env_slash_counted_string;
	_env_max_d =			MAX_D;
	_env_max_n =			MAX_N;
	_env_max_u =			MAX_U;
	_env_max_ud =			MAX_UD;
	_env_return_stack_cells = rstack_size;
	_env_stack_cells =		dstack_size;
	_env_double =			FFLAG(DOUBLE_DEF);
	_env_double_ext =		FFLAG(DOUBLEE_DEF);
	_env_floating =			FFLAG(FLOAT_DEF);
	_env_floating_stack =	fstack_size;
	_env_max_float =		MAX_F;
	_env_floating_ext = 	FFLAG(FLOATE_DEF);
	_env_memory_alloc = 	FFLAG(MEMALL_DEF);
	_env_memory_alloc_ext = FFLAG(MEMALLE_DEF);
	_env_search_order =		FFLAG(SEARCH_DEF);
	_env_search_order_ext = FFLAG(SEARCHE_DEF);
	_env_wordlists =		WORD_LISTS;
	_env_tools =			FFLAG(TOOLS_DEF);
	_env_tools_ext =		FFLAG(TOOLSE_DEF);
	_env_number_locals = 	MAX_LOCALS;
	_env_locals =			FFLAG(LOCALS_DEF);
	_env_locals_ext =		FFLAG(LOCALSE_DEF);
	_env_facility =			FFLAG(FACILITY_DEF);
	_env_facility_ext =		FFLAG(FACILITYE_DEF);
	_env_block = 			FFLAG(BLOCK_DEF);
	_env_block_ext =		FFLAG(BLOCKE_DEF);
	_env_exception = 		FFLAG(EXCEPTION_DEF);
	_env_exception_ext =	FFLAG(EXCEPTIONE_DEF);
	_env_file =				FFLAG(FILE_DEF);
	_env_file_ext =			FFLAG(FILEE_DEF);
	_env_string =			FFLAG(STRING_DEF);
	_env_string_ext =		FFLAG(STRINGE_DEF);
}

