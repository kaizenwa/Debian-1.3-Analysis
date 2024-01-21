/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name: file.c
 * Abstract:    File word set
 */

#include <stdio.h>
#include <errno.h>
#include <malloc.h>
#include <string.h>
#include "yforth.h"
#include "core.h"
#include "block.h"
#include "file.h"

/**************************************************************************/
/* VARIABLES **************************************************************/
/**************************************************************************/

static char *file_mode[] = {
	"r",				/* FILE_R_O 			*/
	"rb",				/* FILE_R_O | FILE_BIN 	*/
	"w",				/* FILE_W_O 			*/
	"wb",				/* FILE_W_O | FILE_BIN	*/
	"w+",				/* FILE_R_W				*/
	"w+b",				/* FILE_R_W | FILE_BIN	*/
	};

Char file_name[FILE_NAME_SIZE];

/**************************************************************************/
/* WORDS ******************************************************************/
/**************************************************************************/

void _bin() {
	sp[0] |= FILE_BIN;
}

void _close_file() {
	if (fclose((FILE *) sp[0])) sp[0] = (Cell) errno;
	else sp[0] = 0;
}

void _create_file() {
	register Cell fam = *sp++;
	register FILE *f;
	get_file_name();
	if (fam & (~FILE_BIN) == FILE_R_O) fam = FILE_R_W | (fam & FILE_BIN);
	f = fopen(file_name, file_mode[fam]);
	*--sp = (Cell) f;
	*--sp = (Cell) f ? 0 : errno;
}

void _delete_file() {
	get_file_name();
	if (remove(file_name)) *--sp = (Cell) errno;
	else *--sp = 0;
}

void _file_position() {
	register FILE *f = (FILE *) sp[0];
	register DCell ud = ftell(f);
	sp -= 2;
	if (ud == -1L) sp[0] = (Cell) errno;
	else {
		PUT_DCELL(sp + 1, ud);
		sp[0] = 0;
	}
}

void _file_size() {
	register FILE *f = (FILE *) sp[0];
	register DCell o_pos = ftell(f);
	if (o_pos != -1L) {
		fseek(f, 0, SEEK_END);
		_file_position();
		fseek(f, o_pos, SEEK_SET);
	} else {
		sp -= 2;
		sp[0] = (Cell) errno;
	}
}

void _include_file() {
	register FILE *f = (FILE *) *sp++;
	save_input_specification();
	_source_id = (Cell) f;
	_input_buffer = malloc(FILE_BUFFER_SIZE);
	_in_input_buffer = 0;
	_b_l_k = 0;
	if (_input_buffer) {
		while (!feof(f) && !ferror(f) && !_error) {
			if (fgets(_input_buffer, FILE_BUFFER_SIZE - 1, f)) {
				_to_in = 0;
				_in_input_buffer = strlen(_input_buffer); 
				if (_in_input_buffer && _input_buffer[_in_input_buffer - 1] == '\n')
					_in_input_buffer--;
				_interpret();
			}
		}
		fclose(f);
		free(_input_buffer);
	}
	restore_input_specification();
}

void _included() {
	_r_o();
	_open_file();
	if ((_error = *sp++) == 0) _include_file();
	else sp++;
}

void _open_file() {
	register Cell fam = *sp++;
	register FILE *f;
	get_file_name();
	f = fopen(file_name, file_mode[fam]);
	*--sp = (Cell) f;
	*--sp = (Cell) (f ? 0 : E_FILENOTFOUND);
}

void _r_o() {
	*--sp = FILE_R_O;
}

void _r_w() {
	*--sp = FILE_R_W;
}

void _read_file() {
	register FILE *f = (FILE *) *sp++;
	register UCell u1 = (UCell) *sp++;
	register Char *buffer = (Char *) *sp++;
	size_t rd = fread(buffer, 1, (size_t) u1, f);
	*--sp = (Cell) rd;
	*--sp = (Cell) ferror(f) ? errno : 0;
}

void _read_line() {
	register FILE *f = (FILE *) *sp++;
	register UCell u1 = (UCell) *sp++;
	register Char *buffer = (Char *) *sp++;
	if (fgets(buffer, u1 + 1, f)) {
		int len = strlen(buffer);
		if (len && buffer[len - 1] == '\n') len--;
		*--sp = 0;
		*--sp = FFLAG(1);
		*--sp = len;
	} else {
		*--sp = (Cell) errno;
		*--sp = FFLAG(0);
		*--sp = 0;
	}
}

void _reposition_file() {
	register FILE *f = (FILE *) *sp++;
	register UDCell ud = GET_DCELL(sp);
	sp++;
	if (fseek(f, ud, SEEK_SET)) sp[0] = errno;
	else sp[0] = 0;
}

void _resize_file() {
	register FILE *f = (FILE *) sp[0];
	register UDCell ud = GET_DCELL(sp + 1), ud1;
	register Cell ior;
	_file_size();
	ior = *sp++;
	if (!ior) {
		ud1 = GET_DCELL(sp);
		if (ud < ud1) ior = truncate_file(f, ud1, ud);
		else if (ud > ud1) ior = expand_file(f, ud1, ud);
	}
	sp += 3;
	sp[0] = ior;
}

void _w_o() {
	*--sp = FILE_W_O;
}

void _write_file() {
	register FILE *f = (FILE *) *sp++;
	register UCell u = (UCell) *sp++;
	register Char *buffer = (Char *) *sp;
	if (fwrite(buffer, 1, (size_t) u, f) < u) sp[0] = errno;
	else sp[0] = 0;
}

void _write_line() {
	register FILE *f = (FILE *) *sp++;
	register UCell u = (UCell) *sp++;
	register Char *buffer = (Char *) *sp;
	while (u--) if (fputc(*buffer++, f) == EOF) break;
	if (!ferror(f)) fputc('\n', f);
	if (ferror(f)) sp[0] = errno;
	else sp[0] = 0;
}

/**************************************************************************/
/* AUXILIARY FUNCTIONS ****************************************************/
/**************************************************************************/

Cell truncate_file(FILE *f, UDCell cur, UDCell ud) {
	if (cur == cur && fseek(f, ud, SEEK_SET)) return (errno);
	else return (0);
}

Cell expand_file(FILE *f, UDCell cur, UDCell ud) {
	fseek(f, 0, SEEK_END);
	while (cur < ud && !ferror(f)) {
		fputc(' ', f);
		cur++;
	}
	if (ferror(f)) return (errno);
	else return (0);
}

Char *get_file_name() {
	register UCell u = (UCell) *sp++;
	register Char *buffer = (Char *) *sp++;
	memcpy(file_name, buffer, u);
	file_name[u] = '\0';
	return (file_name);
}

void load_file(Char *name) {
	*--sp = (Cell) name;
	*--sp = strlen(name);
	_included();
}
