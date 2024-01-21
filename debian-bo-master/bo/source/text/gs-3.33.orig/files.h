/* Copyright (C) 1989, 1995 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* files.h */
/* Common declarations for zfile.c and zfileio.c */
/* Requires stream.h */

/*
 * File objects store a pointer to a stream in value.pfile.
 * A file object is valid if its "size" matches the read_id or write_id
 * (as appropriate) in the stream it points to.  This arrangement
 * allows us to detect closed files reliably, while allowing us to
 * reuse closed streams for new files.
 */
#define fptr(pref) (pref)->value.pfile
#define make_file(pref,a,id,s)\
  make_tasv(pref,t_file,a,id,pfile,s)

/* The stdxxx files.  We have to access them through procedures, */
/* because they might have to be opened when referenced. */
int zget_stdin(P1(stream **));
int zget_stdout(P1(stream **));
int zget_stderr(P1(stream **));
extern bool gs_stdin_is_interactive;
/* An invalid (closed) file. */
extern stream *invalid_file_entry;

/* Macros for checking file validity. */
#define file_is_valid(svar,op)\
  (svar = fptr(op), (svar->read_id | svar->write_id) == r_size(op))
#define check_file(svar,op)\
   {	check_type(*(op), t_file);\
	if ( !file_is_valid(svar, op) ) return_error(e_invalidaccess);\
   }

/*
 * If a file is open for both reading and writing, its read_id, write_id,
 * and stream procedures and modes reflect the current mode of use;
 * an id check failure will switch it to the other mode.
 */
int file_switch_to_read(P1(ref *));
#define check_read_file(svar,op)\
   {	check_read_type(*(op), t_file);\
	check_read_known_file(svar, op, return, svar = invalid_file_entry);\
   }
#define check_read_known_file(svar,op,error_return,invalid_action)\
   {	svar = fptr(op);\
	if ( svar->read_id != r_size(op) )\
	{	if ( svar->read_id == 0 && svar->write_id == r_size(op) )\
		{	int fcode = file_switch_to_read(op);\
			if ( fcode < 0 ) error_return(fcode);\
		}\
		else invalid_action;	/* closed or reopened file */\
	}\
   }
int file_switch_to_write(P1(ref *));
#define check_write_file(svar,op)\
   {	check_write_type(*(op), t_file);\
	check_write_known_file(svar, op, return);\
   }
#define check_write_known_file(svar,op,error_return)\
   {	svar = fptr(op);\
	if ( svar->write_id != r_size(op) )\
	{	int fcode = file_switch_to_write(op);\
		if ( fcode < 0 ) error_return(fcode);\
	}\
   }

/* Data exported by zfile.c. */
	/* for zfilter.c and ziodev.c */
extern const uint file_default_buffer_size;

/* Procedures exported by zfile.c. */
	/* for gs.c */
FILE *lib_fopen(P1(const char *));
	/* for gsmain.c */
int lib_file_open(P6(const char *, uint, byte *, uint, uint *, ref *));
	/* for gsmain.c and iccinit.c */
int file_read_string(P4(const byte *, uint, stream *, ref *));
	/* for os_open in ziodev.c */
#ifdef iodev_proc_fopen			/* in gxiodev.h */
int file_open_stream(P6(const char *, uint, const char *, uint,
  stream **, iodev_proc_fopen_t));
#endif
	/* for zfilter.c */
int filter_open(P7(const char *, uint, ref *, const stream_procs _ds *,
  const stream_template *, const stream_state *, stream **));
	/* for zfileio.c */
void make_stream_file(P3(ref *, stream *, const char *));
	/* for ziodev.c */
int file_close_finish(P1(stream *));
int file_close_disable(P1(stream *));
int file_close_file(P1(stream *));
	/* for gsmain.c, interp.c */
int file_close(P1(ref *));
	/* for ziodev.c */
stream *file_alloc_stream(P2(gs_memory_t *, client_name_t));
	/* for isave.c */
void file_save(P0());
/*void file_restore(P1(const alloc_save_t *));*/

/* Procedures exported by zfileio.c. */
	/* for ziodev.c */
int zreadline_from(P5(stream *, byte *, uint, uint *, bool *));
