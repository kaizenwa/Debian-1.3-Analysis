/* Copyright (C) 1993, 1994 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* isstate.h */
/* State structure for Ghostscript save/restore machinery */
/* Requires isave.h */

/* Saved state of allocator and other things as needed. */
/*typedef struct alloc_save_s alloc_save_t;*/	/* in isave.h */
struct alloc_save_s {
	gs_ref_memory_t state;		/* must be first for subclassing */
	gs_dual_memory_t *dmem;
	uint name_cnt;
	bool is_current;
	ulong id;
	void *client_data;
};
#define private_st_alloc_save()	/* in isave.c */\
  gs_private_st_suffix_add1(st_alloc_save, alloc_save_t, "alloc_save",\
    save_enum_ptrs, save_reloc_ptrs, st_ref_memory, client_data)
