/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name:     block.h
 * Abstract:        Block word set header file
 */

#ifdef DECLARE_WORDS
#	ifdef PROTOTYPES
#		undef PROTOTYPES
#	endif
#	undef __BLOCK_H__
#else
#	ifndef PROTOTYPES
#		define PROTOTYPES
#	endif
#endif

#ifndef __BLOCK_H__
#define __BLOCK_H__

#include "yforth.h"
#include "macro.h"

/**************************************************************************/
/* VARIABLES **************************************************************/
/**************************************************************************/

#ifdef PROTOTYPES

struct _block_data {                /* Entry in the table of blocks */
    UCell block_no;                 /* Block number */
    Cell dirty;                     /* Block updated */
};

struct _block_buffer {              /* Simply an array of Char */
	Char buffer[BLOCK_SIZE];
};

extern FILE *block_file;

extern struct _block_data *block_data;
extern struct _block_buffer *block_buffer;

extern UCell current_block;

#endif

variable(UCell, b_l_k,						"blk")

/**************************************************************************/
/* PROTOTYPES *************************************************************/
/**************************************************************************/

code(block,					"block",					0)
code(buffer,				"buffer",					0)
code(flush,					"flush",					0)
code(load,					"load",						0)
code(save_buffers,			"save-buffers",				0)
code(update,				"update",					0)

#ifdef PROTOTYPES

/**************************************************************************/
/* AUXILIARY FUNCTIONS PROTOTYPES *****************************************/
/**************************************************************************/

int search_block(UCell block_no);
int allocate_block(UCell block_no, int load);
void load_block(UCell block_no, int b);
void save_block(int b);
int open_block_file(char *name);
void close_block_file(void);

#endif

#endif

