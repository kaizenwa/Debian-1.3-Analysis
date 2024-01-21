/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name:     block.c
 * Abstract:        Block word set implementation
 */

#include <stdio.h>
#include <malloc.h>
#include "yforth.h"
#include "core.h"
#include "block.h"

/**************************************************************************/
/* VARIABLES **************************************************************/
/**************************************************************************/

UCell _b_l_k;
UCell current_block;

FILE *block_file;                   /* FILE used to implement blocks */

struct _block_data *block_data;
struct _block_buffer *block_buffer;

static int block_clock;             /* Used to select the next block to
                                       deallocate. Based on the "clock
                                       algorithm
                                    */

/**************************************************************************/
/* WORDS ******************************************************************/
/**************************************************************************/

void _block() {
	register UCell u = (UCell) *sp;
	register int b = search_block(u);
	if (b < 0) b = allocate_block(u, 1);
	current_block = b;
	sp[0] = (Cell) &block_buffer[b].buffer;
}

void _buffer() {
	register UCell u = (UCell) *sp;
	register int b = search_block(u);
	if (b < 0) b = allocate_block(u, 0);
	current_block = b;
	sp[0] = (Cell) &block_buffer[b].buffer;
}

void _flush() {
	register int i;
	_save_buffers();
	for (i = 0; i < NUM_BLOCKS; i++) block_data[i].block_no = 0;
}

void _load() {
	register UCell block_no = (UCell) *sp;
	save_input_specification();
	_block();
	_input_buffer = (Char *) *sp++;
	_in_input_buffer = BLOCK_SIZE;
	_to_in = 0;
	_b_l_k = block_no;
	_interpret();
	restore_input_specification();
}

void _save_buffers() {
	register int i;
	for (i = 0; i < NUM_BLOCKS; i++) if (block_data[i].dirty) save_block(i);
}

void _update() {
	block_data[current_block].dirty = 1;
}

/**************************************************************************/
/* AUXILIARY FUNCTIONS ****************************************************/
/**************************************************************************/

int search_block(UCell block_no) {
	register int i;
	for (i = 0; i < NUM_BLOCKS && block_data[i].block_no != block_no; i++) ;
	return (i < NUM_BLOCKS ? i : -1);
}

int allocate_block(UCell block_no, int load) {
	register int i;
	register int b = search_block(0);
	if (b < 0) {
        if (block_data[block_clock].dirty) save_block(block_clock);
        b = block_clock;
        block_clock = (block_clock + 1) % NUM_BLOCKS;
	}
	if (load) load_block(block_no, b);
    return (b);
}

void load_block(UCell block_no, int b) {
	block_data[b].block_no = block_no;
	block_data[b].dirty = 0;
	fseek(block_file, ((long) (block_no - 1)) * BLOCK_SIZE, SEEK_SET);
	fread(&block_buffer[b].buffer, BLOCK_SIZE, 1, block_file);
}

void save_block(int b) {
	fseek(block_file, ((long) (block_data[b].block_no - 1)) * BLOCK_SIZE, SEEK_SET);
	fwrite(&block_buffer[b].buffer, BLOCK_SIZE, 1, block_file);
	block_data[b].dirty = 0;
}

int open_block_file(char *name) {
	block_file = fopen(name, "r+b");
	if (!block_file) block_file = fopen(name, "r+b");
	if (block_file) {
		block_data = (struct _block_data *) malloc(NUM_BLOCKS * sizeof(struct _block_data));
		block_buffer = (struct _block_buffer *) malloc(NUM_BLOCKS * sizeof(struct _block_buffer));
		if (block_data && block_buffer) {
			int i;
			for (i = 0; i < NUM_BLOCKS; i++) block_data[i].block_no = 0;
		} else block_file = NULL;
	}
	return (block_file ? 0 : -1);
}

void close_block_file() {
	if (block_file) {
		_save_buffers();
		fclose(block_file);
	}
}
