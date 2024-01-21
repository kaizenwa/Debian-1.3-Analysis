/*
** Automatically generated from `io.m' by the
** Mercury compiler, version 0.6.  Do not edit.
*/
/*
INIT mercury__io__init
ENDINIT
*/

#include "imp.h"


#include "init.h"
#include "wrapper.h"
#include "type_info.h"

#include <stdlib.h>
#include <string.h>
#include <errno.h>

/*
** Mercury files are not quite the same as C stdio FILEs,
** because we keep track of a little bit more information.
*/

typedef struct mercury_file {
	FILE *file;
	int line_number;
} MercuryFile;

extern MercuryFile mercury_stdin;
extern MercuryFile mercury_stdout;
extern MercuryFile mercury_stderr;
extern MercuryFile *mercury_current_text_input;
extern MercuryFile *mercury_current_text_output;
extern MercuryFile *mercury_current_binary_input;
extern MercuryFile *mercury_current_binary_output;

#define initial_external_state()	0	/* some random number */
#define update_io(r_src, r_dest)	((r_dest) = (r_src))
#define final_io_state(r)		((void)0)

void 		mercury_init_io(void);
MercuryFile*	mercury_open(const char *filename, const char *type);
int		mercury_output_error(MercuryFile* mf);
void		mercury_print_string(MercuryFile* mf, const char *s);
void		mercury_print_binary_string(MercuryFile* mf, const char *s);
int		mercury_getc(MercuryFile* mf);
void		mercury_close(MercuryFile* mf);

#include "init.h"
#include "prof.h"

Declare_static(mercury____Index___io_io__read_result_1__ua10000_2_0);
Declare_label(mercury____Index___io_io__read_result_1__ua10000_2_0_i4);
Declare_label(mercury____Index___io_io__read_result_1__ua10000_2_0_i5);
Declare_static(mercury____Index___io_io__result_1__ua10000_2_0);
Declare_label(mercury____Index___io_io__result_1__ua10000_2_0_i4);
Declare_label(mercury____Index___io_io__result_1__ua10000_2_0_i5);
Declare_static(mercury____Index___io_io__res_1__ua10000_2_0);
Declare_label(mercury____Index___io_io__res_1__ua10000_2_0_i3);
Declare_static(mercury____Index___io_io__state_0__ua10000_2_0);
Define_extern_entry(mercury__io__read_char_3_0);
Declare_label(mercury__io__read_char_3_0_i2);
Define_extern_entry(mercury__io__read_word_3_0);
Declare_label(mercury__io__read_word_3_0_i2);
Define_extern_entry(mercury__io__read_line_3_0);
Declare_label(mercury__io__read_line_3_0_i2);
Define_extern_entry(mercury__io__putback_char_3_0);
Declare_label(mercury__io__putback_char_3_0_i2);
Define_extern_entry(mercury__io__read_char_4_0);
Declare_label(mercury__io__read_char_4_0_i2);
Declare_label(mercury__io__read_char_4_0_i3);
Declare_label(mercury__io__read_char_4_0_i8);
Declare_label(mercury__io__read_char_4_0_i7);
Define_extern_entry(mercury__io__read_word_4_0);
Declare_label(mercury__io__read_word_4_0_i2);
Declare_label(mercury__io__read_word_4_0_i6);
Declare_label(mercury__io__read_word_4_0_i5);
Define_extern_entry(mercury__io__read_line_4_0);
Declare_label(mercury__io__read_line_4_0_i2);
Declare_label(mercury__io__read_line_4_0_i5);
Declare_label(mercury__io__read_line_4_0_i7);
Declare_label(mercury__io__read_line_4_0_i10);
Declare_label(mercury__io__read_line_4_0_i13);
Declare_label(mercury__io__read_line_4_0_i3);
Define_extern_entry(mercury__io__putback_char_4_0);
Define_extern_entry(mercury__io__read_anything_3_0);
Declare_label(mercury__io__read_anything_3_0_i2);
Declare_label(mercury__io__read_anything_3_0_i5);
Declare_label(mercury__io__read_anything_3_0_i6);
Declare_label(mercury__io__read_anything_3_0_i9);
Declare_label(mercury__io__read_anything_3_0_i8);
Define_extern_entry(mercury__io__read_anything_4_0);
Declare_label(mercury__io__read_anything_4_0_i2);
Declare_label(mercury__io__read_anything_4_0_i3);
Declare_label(mercury__io__read_anything_4_0_i4);
Define_extern_entry(mercury__io__ignore_whitespace_3_0);
Declare_label(mercury__io__ignore_whitespace_3_0_i2);
Define_extern_entry(mercury__io__ignore_whitespace_4_0);
Declare_label(mercury__io__ignore_whitespace_4_0_i2);
Declare_label(mercury__io__ignore_whitespace_4_0_i5);
Declare_label(mercury__io__ignore_whitespace_4_0_i9);
Declare_label(mercury__io__ignore_whitespace_4_0_i8);
Declare_label(mercury__io__ignore_whitespace_4_0_i12);
Declare_label(mercury__io__ignore_whitespace_4_0_i6);
Define_extern_entry(mercury__io__write_string_3_0);
Define_extern_entry(mercury__io__write_string_4_0);
Define_extern_entry(mercury__io__write_strings_3_0);
Declare_label(mercury__io__write_strings_3_0_i2);
Define_extern_entry(mercury__io__write_strings_4_0);
Declare_label(mercury__io__write_strings_4_0_i4);
Declare_label(mercury__io__write_strings_4_0_i1002);
Define_extern_entry(mercury__io__write_char_3_0);
Define_extern_entry(mercury__io__write_char_4_0);
Define_extern_entry(mercury__io__write_int_3_0);
Define_extern_entry(mercury__io__write_int_4_0);
Define_extern_entry(mercury__io__write_float_3_0);
Define_extern_entry(mercury__io__write_float_4_0);
Define_extern_entry(mercury__io__write_many_3_0);
Declare_label(mercury__io__write_many_3_0_i2);
Define_extern_entry(mercury__io__write_many_4_0);
Declare_label(mercury__io__write_many_4_0_i7);
Declare_label(mercury__io__write_many_4_0_i6);
Declare_label(mercury__io__write_many_4_0_i9);
Declare_label(mercury__io__write_many_4_0_i12);
Declare_label(mercury__io__write_many_4_0_i1000);
Define_extern_entry(mercury__io__write_3_0);
Define_extern_entry(mercury__io__write_4_0);
Declare_label(mercury__io__write_4_0_i2);
Declare_label(mercury__io__write_4_0_i3);
Declare_label(mercury__io__write_4_0_i4);
Define_extern_entry(mercury__io__flush_output_2_0);
Define_extern_entry(mercury__io__flush_output_3_0);
Define_extern_entry(mercury__io__see_4_0);
Declare_label(mercury__io__see_4_0_i2);
Declare_label(mercury__io__see_4_0_i4);
Declare_label(mercury__io__see_4_0_i5);
Define_extern_entry(mercury__io__seen_2_0);
Declare_label(mercury__io__seen_2_0_i2);
Declare_label(mercury__io__seen_2_0_i3);
Define_extern_entry(mercury__io__open_input_4_0);
Declare_label(mercury__io__open_input_4_0_i2);
Declare_label(mercury__io__open_input_4_0_i8);
Declare_label(mercury__io__open_input_4_0_i3);
Define_extern_entry(mercury__io__close_input_3_0);
Define_extern_entry(mercury__io__input_stream_3_0);
Define_extern_entry(mercury__io__set_input_stream_4_0);
Define_extern_entry(mercury__io__stdin_stream_3_0);
Define_extern_entry(mercury__io__input_stream_name_3_0);
Declare_label(mercury__io__input_stream_name_3_0_i2);
Define_extern_entry(mercury__io__input_stream_name_4_0);
Define_extern_entry(mercury__io__get_line_number_3_0);
Define_extern_entry(mercury__io__get_line_number_4_0);
Define_extern_entry(mercury__io__set_line_number_3_0);
Define_extern_entry(mercury__io__set_line_number_4_0);
Define_extern_entry(mercury__io__tell_4_0);
Declare_label(mercury__io__tell_4_0_i2);
Declare_label(mercury__io__tell_4_0_i6);
Declare_label(mercury__io__tell_4_0_i3);
Define_extern_entry(mercury__io__told_2_0);
Declare_label(mercury__io__told_2_0_i2);
Declare_label(mercury__io__told_2_0_i3);
Define_extern_entry(mercury__io__open_output_4_0);
Declare_label(mercury__io__open_output_4_0_i2);
Declare_label(mercury__io__open_output_4_0_i8);
Declare_label(mercury__io__open_output_4_0_i3);
Define_extern_entry(mercury__io__open_append_4_0);
Declare_label(mercury__io__open_append_4_0_i2);
Declare_label(mercury__io__open_append_4_0_i8);
Declare_label(mercury__io__open_append_4_0_i3);
Define_extern_entry(mercury__io__close_output_3_0);
Define_extern_entry(mercury__io__output_stream_3_0);
Define_extern_entry(mercury__io__set_output_stream_4_0);
Define_extern_entry(mercury__io__stdout_stream_3_0);
Define_extern_entry(mercury__io__stderr_stream_3_0);
Define_extern_entry(mercury__io__output_stream_name_3_0);
Declare_label(mercury__io__output_stream_name_3_0_i2);
Define_extern_entry(mercury__io__output_stream_name_4_0);
Define_extern_entry(mercury__io__read_byte_3_0);
Declare_label(mercury__io__read_byte_3_0_i2);
Define_extern_entry(mercury__io__read_byte_4_0);
Declare_label(mercury__io__read_byte_4_0_i2);
Declare_label(mercury__io__read_byte_4_0_i3);
Declare_label(mercury__io__read_byte_4_0_i6);
Define_extern_entry(mercury__io__putback_byte_3_0);
Declare_label(mercury__io__putback_byte_3_0_i2);
Define_extern_entry(mercury__io__putback_byte_4_0);
Define_extern_entry(mercury__io__write_byte_3_0);
Define_extern_entry(mercury__io__write_byte_4_0);
Define_extern_entry(mercury__io__write_bytes_3_0);
Define_extern_entry(mercury__io__write_bytes_4_0);
Define_extern_entry(mercury__io__flush_binary_output_2_0);
Define_extern_entry(mercury__io__flush_binary_output_3_0);
Define_extern_entry(mercury__io__see_binary_4_0);
Declare_label(mercury__io__see_binary_4_0_i2);
Declare_label(mercury__io__see_binary_4_0_i4);
Declare_label(mercury__io__see_binary_4_0_i5);
Define_extern_entry(mercury__io__seen_binary_2_0);
Declare_label(mercury__io__seen_binary_2_0_i2);
Declare_label(mercury__io__seen_binary_2_0_i3);
Define_extern_entry(mercury__io__open_binary_input_4_0);
Declare_label(mercury__io__open_binary_input_4_0_i2);
Declare_label(mercury__io__open_binary_input_4_0_i8);
Declare_label(mercury__io__open_binary_input_4_0_i3);
Define_extern_entry(mercury__io__close_binary_input_3_0);
Define_extern_entry(mercury__io__binary_input_stream_3_0);
Define_extern_entry(mercury__io__set_binary_input_stream_4_0);
Define_extern_entry(mercury__io__stdin_binary_stream_3_0);
Define_extern_entry(mercury__io__binary_input_stream_name_3_0);
Declare_label(mercury__io__binary_input_stream_name_3_0_i2);
Define_extern_entry(mercury__io__binary_input_stream_name_4_0);
Define_extern_entry(mercury__io__tell_binary_4_0);
Declare_label(mercury__io__tell_binary_4_0_i2);
Declare_label(mercury__io__tell_binary_4_0_i6);
Declare_label(mercury__io__tell_binary_4_0_i3);
Define_extern_entry(mercury__io__told_binary_2_0);
Declare_label(mercury__io__told_binary_2_0_i2);
Declare_label(mercury__io__told_binary_2_0_i3);
Define_extern_entry(mercury__io__open_binary_output_4_0);
Declare_label(mercury__io__open_binary_output_4_0_i2);
Declare_label(mercury__io__open_binary_output_4_0_i8);
Declare_label(mercury__io__open_binary_output_4_0_i3);
Define_extern_entry(mercury__io__open_binary_append_4_0);
Declare_label(mercury__io__open_binary_append_4_0_i2);
Declare_label(mercury__io__open_binary_append_4_0_i8);
Declare_label(mercury__io__open_binary_append_4_0_i3);
Define_extern_entry(mercury__io__close_binary_output_3_0);
Define_extern_entry(mercury__io__binary_output_stream_3_0);
Define_extern_entry(mercury__io__stdout_binary_stream_3_0);
Define_extern_entry(mercury__io__set_binary_output_stream_4_0);
Define_extern_entry(mercury__io__binary_output_stream_name_3_0);
Declare_label(mercury__io__binary_output_stream_name_3_0_i2);
Define_extern_entry(mercury__io__binary_output_stream_name_4_0);
Define_extern_entry(mercury__io__progname_4_0);
Define_extern_entry(mercury__io__progname_base_4_0);
Declare_label(mercury__io__progname_base_4_0_i2);
Declare_label(mercury__io__progname_base_4_0_i3);
Define_extern_entry(mercury__io__command_line_arguments_3_0);
Define_extern_entry(mercury__io__get_exit_status_3_0);
Define_extern_entry(mercury__io__set_exit_status_3_0);
Define_extern_entry(mercury__io__get_globals_3_0);
Declare_label(mercury__io__get_globals_3_0_i2);
Define_extern_entry(mercury__io__set_globals_3_0);
Define_extern_entry(mercury__io__get_environment_var_4_0);
Declare_label(mercury__io__get_environment_var_4_0_i4);
Declare_label(mercury__io__get_environment_var_4_0_i3);
Define_extern_entry(mercury__io__set_environment_var_4_0);
Declare_label(mercury__io__set_environment_var_4_0_i2);
Declare_label(mercury__io__set_environment_var_4_0_i5);
Declare_label(mercury__io__set_environment_var_4_0_i4);
Declare_label(mercury__io__set_environment_var_4_0_i7);
Define_extern_entry(mercury__io__report_stats_2_0);
Declare_label(mercury__io__report_stats_2_0_i2);
Define_extern_entry(mercury__io__preallocate_heap_space_3_0);
Define_extern_entry(mercury__io__call_system_4_0);
Declare_label(mercury__io__call_system_4_0_i2);
Declare_label(mercury__io__call_system_4_0_i3);
Define_extern_entry(mercury__io__error_message_2_0);
Define_extern_entry(mercury__io__get_op_table_3_0);
Declare_label(mercury__io__get_op_table_3_0_i2);
Define_extern_entry(mercury__io__set_op_table_3_0);
Define_extern_entry(mercury__io__init_state_2_0);
Declare_label(mercury__io__init_state_2_0_i2);
Declare_label(mercury__io__init_state_2_0_i3);
Declare_label(mercury__io__init_state_2_0_i4);
Declare_label(mercury__io__init_state_2_0_i5);
Declare_label(mercury__io__init_state_2_0_i6);
Declare_label(mercury__io__init_state_2_0_i7);
Declare_label(mercury__io__init_state_2_0_i8);
Declare_label(mercury__io__init_state_2_0_i9);
Declare_label(mercury__io__init_state_2_0_i10);
Declare_static(mercury__io__read_char_code_4_0);
Declare_static(mercury__io__call_system_code_4_0);
Declare_static(mercury__io__do_open_6_0);
Declare_static(mercury__io__getenv_2_0);
Declare_label(mercury__io__getenv_2_0_i1);
Declare_static(mercury__io__putenv_1_0);
Declare_label(mercury__io__putenv_1_0_i1);
Declare_static(mercury__io__read_word_2_4_0);
Declare_label(mercury__io__read_word_2_4_0_i2);
Declare_label(mercury__io__read_word_2_4_0_i5);
Declare_label(mercury__io__read_word_2_4_0_i9);
Declare_label(mercury__io__read_word_2_4_0_i11);
Declare_label(mercury__io__read_word_2_4_0_i8);
Declare_label(mercury__io__read_word_2_4_0_i12);
Declare_label(mercury__io__read_word_2_4_0_i15);
Declare_label(mercury__io__read_word_2_4_0_i3);
Declare_static(mercury__io__stream_name_4_0);
Declare_label(mercury__io__stream_name_4_0_i2);
Declare_label(mercury__io__stream_name_4_0_i5);
Declare_label(mercury__io__stream_name_4_0_i4);
Declare_static(mercury__io__insert_stream_name_4_0);
Declare_label(mercury__io__insert_stream_name_4_0_i2);
Declare_label(mercury__io__insert_stream_name_4_0_i3);
Declare_label(mercury__io__insert_stream_name_4_0_i4);
Define_extern_entry(mercury____Unify___io__state_0_0);
Declare_label(mercury____Unify___io__state_0_0_i2);
Declare_label(mercury____Unify___io__state_0_0_i4);
Declare_label(mercury____Unify___io__state_0_0_i6);
Declare_label(mercury____Unify___io__state_0_0_i8);
Declare_label(mercury____Unify___io__state_0_0_i1);
Define_extern_entry(mercury____Index___io__state_0_0);
Define_extern_entry(mercury____Compare___io__state_0_0);
Declare_label(mercury____Compare___io__state_0_0_i4);
Declare_label(mercury____Compare___io__state_0_0_i5);
Declare_label(mercury____Compare___io__state_0_0_i3);
Declare_label(mercury____Compare___io__state_0_0_i10);
Declare_label(mercury____Compare___io__state_0_0_i16);
Declare_label(mercury____Compare___io__state_0_0_i22);
Define_extern_entry(mercury____Unify___io__input_stream_0_0);
Define_extern_entry(mercury____Index___io__input_stream_0_0);
Define_extern_entry(mercury____Compare___io__input_stream_0_0);
Define_extern_entry(mercury____Unify___io__output_stream_0_0);
Define_extern_entry(mercury____Index___io__output_stream_0_0);
Define_extern_entry(mercury____Compare___io__output_stream_0_0);
Define_extern_entry(mercury____Unify___io__binary_input_stream_0_0);
Define_extern_entry(mercury____Index___io__binary_input_stream_0_0);
Define_extern_entry(mercury____Compare___io__binary_input_stream_0_0);
Define_extern_entry(mercury____Unify___io__binary_output_stream_0_0);
Define_extern_entry(mercury____Index___io__binary_output_stream_0_0);
Define_extern_entry(mercury____Compare___io__binary_output_stream_0_0);
Define_extern_entry(mercury____Unify___io__res_0_0);
Declare_label(mercury____Unify___io__res_0_0_i3);
Declare_label(mercury____Unify___io__res_0_0_i2);
Declare_label(mercury____Unify___io__res_0_0_i1);
Define_extern_entry(mercury____Index___io__res_0_0);
Declare_label(mercury____Index___io__res_0_0_i3);
Define_extern_entry(mercury____Compare___io__res_0_0);
Declare_label(mercury____Compare___io__res_0_0_i2);
Declare_label(mercury____Compare___io__res_0_0_i3);
Declare_label(mercury____Compare___io__res_0_0_i4);
Declare_label(mercury____Compare___io__res_0_0_i6);
Declare_label(mercury____Compare___io__res_0_0_i11);
Declare_label(mercury____Compare___io__res_0_0_i9);
Define_extern_entry(mercury____Unify___io__res_1_0);
Declare_label(mercury____Unify___io__res_1_0_i1008);
Declare_label(mercury____Unify___io__res_1_0_i1005);
Declare_label(mercury____Unify___io__res_1_0_i1);
Declare_label(mercury____Unify___io__res_1_0_i1007);
Define_extern_entry(mercury____Index___io__res_1_0);
Define_extern_entry(mercury____Compare___io__res_1_0);
Declare_label(mercury____Compare___io__res_1_0_i2);
Declare_label(mercury____Compare___io__res_1_0_i3);
Declare_label(mercury____Compare___io__res_1_0_i4);
Declare_label(mercury____Compare___io__res_1_0_i6);
Declare_label(mercury____Compare___io__res_1_0_i11);
Declare_label(mercury____Compare___io__res_1_0_i9);
Define_extern_entry(mercury____Unify___io__result_0_0);
Declare_label(mercury____Unify___io__result_0_0_i5);
Declare_label(mercury____Unify___io__result_0_0_i4);
Declare_label(mercury____Unify___io__result_0_0_i2);
Declare_label(mercury____Unify___io__result_0_0_i1);
Define_extern_entry(mercury____Index___io__result_0_0);
Declare_label(mercury____Index___io__result_0_0_i5);
Declare_label(mercury____Index___io__result_0_0_i4);
Define_extern_entry(mercury____Compare___io__result_0_0);
Declare_label(mercury____Compare___io__result_0_0_i2);
Declare_label(mercury____Compare___io__result_0_0_i3);
Declare_label(mercury____Compare___io__result_0_0_i4);
Declare_label(mercury____Compare___io__result_0_0_i6);
Declare_label(mercury____Compare___io__result_0_0_i13);
Declare_label(mercury____Compare___io__result_0_0_i12);
Declare_label(mercury____Compare___io__result_0_0_i9);
Declare_label(mercury____Compare___io__result_0_0_i1000);
Define_extern_entry(mercury____Unify___io__result_1_0);
Declare_label(mercury____Unify___io__result_1_0_i1010);
Declare_label(mercury____Unify___io__result_1_0_i6);
Declare_label(mercury____Unify___io__result_1_0_i1007);
Declare_label(mercury____Unify___io__result_1_0_i1);
Declare_label(mercury____Unify___io__result_1_0_i1009);
Define_extern_entry(mercury____Index___io__result_1_0);
Define_extern_entry(mercury____Compare___io__result_1_0);
Declare_label(mercury____Compare___io__result_1_0_i2);
Declare_label(mercury____Compare___io__result_1_0_i3);
Declare_label(mercury____Compare___io__result_1_0_i4);
Declare_label(mercury____Compare___io__result_1_0_i6);
Declare_label(mercury____Compare___io__result_1_0_i12);
Declare_label(mercury____Compare___io__result_1_0_i14);
Declare_label(mercury____Compare___io__result_1_0_i9);
Declare_label(mercury____Compare___io__result_1_0_i1000);
Define_extern_entry(mercury____Unify___io__read_result_1_0);
Declare_label(mercury____Unify___io__read_result_1_0_i1011);
Declare_label(mercury____Unify___io__read_result_1_0_i6);
Declare_label(mercury____Unify___io__read_result_1_0_i1008);
Declare_label(mercury____Unify___io__read_result_1_0_i1);
Declare_label(mercury____Unify___io__read_result_1_0_i1010);
Define_extern_entry(mercury____Index___io__read_result_1_0);
Define_extern_entry(mercury____Compare___io__read_result_1_0);
Declare_label(mercury____Compare___io__read_result_1_0_i2);
Declare_label(mercury____Compare___io__read_result_1_0_i3);
Declare_label(mercury____Compare___io__read_result_1_0_i4);
Declare_label(mercury____Compare___io__read_result_1_0_i6);
Declare_label(mercury____Compare___io__read_result_1_0_i12);
Declare_label(mercury____Compare___io__read_result_1_0_i14);
Declare_label(mercury____Compare___io__read_result_1_0_i20);
Declare_label(mercury____Compare___io__read_result_1_0_i19);
Declare_label(mercury____Compare___io__read_result_1_0_i9);
Define_extern_entry(mercury____Unify___io__error_0_0);
Declare_label(mercury____Unify___io__error_0_0_i1);
Define_extern_entry(mercury____Index___io__error_0_0);
Define_extern_entry(mercury____Compare___io__error_0_0);
Define_extern_entry(mercury____Unify___io__poly_type_0_0);
Define_extern_entry(mercury____Index___io__poly_type_0_0);
Define_extern_entry(mercury____Compare___io__poly_type_0_0);


	/* Prototypes */

void mercury_print_const(Word data_value, Word entry_value);
void mercury_print_enum(Word data_value, Word entry_value);
void mercury_print_simple(Word data_value, Word entry_value, Word * type_info);
void mercury_print_builtin(Word data_value, Word entry_value);
void mercury_print_complicated(Word data_value, Word entry_value, 
		Word * type_info);

void mercury_print_type(Word *type_info, Word data_value);
Word * make_type_info(Word *term_type_info, Word *arg_pseudo_type_info,
	int *allocated);


	/* 
	 * Given a type_info (term_type_info) which contains a
	 * base_type_info pointer and possibly other type_infos
	 * giving the values of the type parameters of this type,
	 * and a pseudo-type_info (arg_pseudo_type_info), which contains a
	 * base_type_info pointer and possibly other type_infos
	 * giving EITHER
	 * 	- the values of the type parameters of this type,
	 * or	- an indication of the type parameter of the
	 * 	  term_type_info that should be substituted here
	 *
	 * This returns a fully instantiated type_info, a version of the
	 * arg_pseudo_type_info with all the type variables filled in.
	 * If there are no type variables to fill in, we return the
	 * arg_pseudo_type_info, unchanged. Otherwise, we allocate
	 * memory using malloc().  If memory is allocated, the integer
	 * argument (passed by reference) is set to 1, otherwise it is
	 * set to 0.  It is the caller's responsibility to check whether 
	 * the call to make_type_info allocated memory, and if so, free
	 * it.
	 *
	 * This code could be tighter. In general, we want to
	 * handle our own allocations rather than using malloc().
	 * Also, we might be able to do only one traversal.
	 */

Word * make_type_info(Word *term_type_info, Word *arg_pseudo_type_info,
	int *allocated) 
{
	int arity, i;
	Word base_type_info;
	Word *type_info;

	*allocated = 0;

		/* The arg_pseudo_type_info might be a polymorphic variable */

	if ((Word) arg_pseudo_type_info < TYPELAYOUT_MAX_VARINT) {
		return (Word *) term_type_info[(Word) arg_pseudo_type_info];
	}


	base_type_info = arg_pseudo_type_info[0];

		/* no arguments - optimise common case */
	if (base_type_info == 0) {
		return arg_pseudo_type_info;
	} else {
		arity = ((Word *) base_type_info)[0];
	}

	for (i = arity; i > 0; i--) {
		if (arg_pseudo_type_info[i] < TYPELAYOUT_MAX_VARINT) {
			break;
		}
	}

		/* 
		 * See if any of the arguments were polymorphic.
		 * If so, substitute.
		 */
	if (i > 0) {
		type_info = checked_malloc(arity * sizeof(Word));
		*allocated = 1;
		for (i = 0; i <= arity; i++) {
			if (arg_pseudo_type_info[i] < TYPELAYOUT_MAX_VARINT) {
				type_info[i] = term_type_info[arg_pseudo_type_info[i]];
				if (type_info[i] < TYPELAYOUT_MAX_VARINT) {
					fatal_error("Error! Can't instantiate type variable.");
				}
			} else {
				type_info[i] = arg_pseudo_type_info[i];
			}
		}
		return type_info;
	} else {
		return arg_pseudo_type_info;
	}

}

/*
 * Print a constant value
 */

void
mercury_print_const(Word data_value, Word entry_value) 
{

#ifdef DEBUG_IO__WRITE
	printf("This is a constant functor, %ld of %ld with this tag
",
            data_value + 1, ((Word *) entry_value)[1]); 
#endif

	/* the functors are stored after the enum_indicator and
	 * the number of functors
	 */
	printf("%s", (char *) ((Word *) entry_value)[data_value + 2]);	
}

void
mercury_print_enum(Word data_value, Word entry_value) 
{

#ifdef DEBUG_IO__WRITE
	printf("This is a constant functor, %ld of %ld in this enum
",
            data_value + 1, entry_value[1]); 
#endif

	/* the functors are stored after the enum_indicator and
	 * the number of functors
	 */

	printf("%s", (char *) ((Word *) entry_value)[data_value + 2]);	
}


/* 
 * Simple tags - type_layout points to an array containing
 * the arity, then a pseudo-typeinfo for each argument.
 *
 * Data word points to an array of argument data.
 */
void
mercury_print_simple(Word data_value, Word entry_value, Word * type_info) 
{
	int num_args, i;
	int allocated = 0;

	num_args = field(0, (Word *) entry_value, 0);

#ifdef DEBUG_IO__WRITE
	printf("This functor has %d arguments.
", num_args); 
#endif
	
	printf("%s(", (char *) ((Word *) entry_value)[num_args + 1]);

	for (i = 0; i < num_args ; i++) {
		Word * arg_pseudo_type_info;
		Word * arg_type_info;

		if (i != 0) {
			printf(", ");
		}

#ifdef DEBUG_IO__WRITE
		printf("Argument %d of %d is:
", i+1, num_args);
#endif

		arg_pseudo_type_info = (Word *) ((Word *) entry_value)[i + 1];

#ifdef DEBUG_IO__WRITE
		printf("Entry %ld Data %ld ", (Word) entry_type_info,
			((Word *) data_value)[i]); 
#endif
		arg_type_info = make_type_info(type_info, (Word *) 
			arg_pseudo_type_info, &allocated);

#ifdef DEBUG_IO__WRITE
		printf("Typeinfo %ld
", (Word) entry_type_info);
#endif
		mercury_print_type(arg_type_info, ((Word *) data_value)[i]);

		if (allocated) {
			free(arg_type_info);
		}

	}

	printf(")");
}

/*
 * Complicated tags - entry_value points to a vector containing: 
 *	The number of sharers of this tag
 *	A pointer to a simple tag structure (see mercury_print_simple)
 *	for each sharer.
 *
 *	The data_value points to the actual sharer of this tag, 
 *	which should be used as an index into the vector of pointers
 *	into simple tag structures. The next n words the data_value
 *	points to are the arguments of the functor.
 */

void
mercury_print_complicated(Word data_value, Word entry_value, Word * type_info) 
{
	Word new_data_value, new_entry_value, new_entry_body,
		new_entry_tag, secondary_tag;

	secondary_tag = ((Word *) data_value)[0];

#ifdef DEBUG_IO__WRITE
	printf("This is %ld of %ld functors sharing this tag
",
		secondary_tag + 1, ((Word *) entry_value)[0]); 
#endif

	new_entry_value = ((Word *) entry_value)[secondary_tag + 1];
	new_entry_tag = tag(new_entry_value);
	new_entry_body = body(new_entry_value, new_entry_tag);
	new_data_value = (Word) ((Word *) data_value + 1);

	mercury_print_simple(new_data_value, new_entry_body, type_info);
}

void
mercury_print_builtin(Word data_value, Word entry_value) 
{

	switch ((int) entry_value) {
	
	case TYPELAYOUT_UNASSIGNED_VALUE:
		fatal_error("Attempt to use an UNASSIGNED tag in io__write.");
		break;

	case TYPELAYOUT_UNUSED_VALUE:
		fatal_error("Attempt to use an UNUSED tag in io__write.");
		break;

	case TYPELAYOUT_STRING_VALUE:
		if (fprintf(mercury_current_text_output->file, 
			"%c%s%c", '"', (char *) data_value, '"') < 0) {
			mercury_output_error(mercury_current_text_output);
		}
		break;

	case TYPELAYOUT_FLOAT_VALUE:
	{
		Float f;
		f = word_to_float(data_value);
		if (fprintf(mercury_current_text_output->file, 
			"%#.15g", f) < 0) {
			mercury_output_error(mercury_current_text_output);
		}
	}
	break;

	case TYPELAYOUT_INT_VALUE:
		if (fprintf(mercury_current_text_output->file, "%ld", 
			(long) data_value) < 0) {
			mercury_output_error(mercury_current_text_output);
		}
	break;

	case TYPELAYOUT_CHARACTER_VALUE:
		if (fprintf(mercury_current_text_output->file, "'%c'",
			(char) data_value) < 0) {
			mercury_output_error(mercury_current_text_output);
		}
		if (data_value == '\n') {
			mercury_current_text_output->line_number++;
		}
		break;

	case TYPELAYOUT_UNIV_VALUE:

#ifdef DEBUG_IO__WRITE
	printf("This is a univ, it is really a:
");
#endif
		/* Univ is a two word structure, containing
		 * type_info and data.
		 */
		mercury_print_type((Word *) ((Word *) data_value)[0], 
			((Word *) data_value)[1]);
		break;

	case TYPELAYOUT_PREDICATE_VALUE:
		if (fprintf(mercury_current_text_output->file, 
			"<<predicate>>") < 0) { 
			mercury_output_error(mercury_current_text_output);
		}
		break;

	default:
		fatal_error("Invalid tag value in io__write");
		break;
	}

}

	/*
	 * Print out Mercury data, given its type_info, and the data 
	 * itself.
	 *
	 * Note: The variable entry_value and data_value are used
	 * for a number of purposes, as depending on the type of the
	 * data, they can be represent many things.
	 *
	 *
	 */

void
mercury_print_type(Word *type_info, Word data_word) 
{
	Word *base_type_info, *arg_type_info, *base_type_layout;
	Word data_value, entry_value, base_type_layout_entry;
	int entry_tag, data_tag; 

	base_type_info = (Word *) type_info[0];

		/* 
		 * Find the base_type_info - type_infos for types with no args 
		 * are themselves base_type_infos
		 */

	if(base_type_info == 0) {
		base_type_info = type_info;
	}

		/* Retrieve base_type_layout */
	base_type_layout = (Word *) base_type_info[OFFSET_FOR_BASE_TYPE_LAYOUT];

	data_tag = tag(data_word);
	data_value = body(data_word, data_tag);
	
	base_type_layout_entry = base_type_layout[data_tag];

	entry_tag = tag(base_type_layout_entry);
	entry_value = body(base_type_layout_entry, entry_tag);
	
	switch(entry_tag) {

	case TYPELAYOUT_CONST_TAG: /* case TYPELAYOUT_COMP_CONST_TAG: */

		/* Is it a builtin or a constant/enum? */ 

		if (entry_value > TYPELAYOUT_MAX_VARINT) {

			/* Check enum indicator */

			if (((Word *) entry_value)[0]) {
				mercury_print_enum(data_word, entry_value);
			} else {
				data_value = unmkbody(data_value);
				mercury_print_const(data_value, entry_value);
			}
		} else {
			entry_value = unmkbody(entry_value);
			mercury_print_builtin(data_word, entry_value);
		}
		break;

	case TYPELAYOUT_SIMPLE_TAG:
		mercury_print_simple(data_value, entry_value, type_info);
		break;

	case TYPELAYOUT_COMPLICATED_TAG:
		mercury_print_complicated(data_value, entry_value, type_info);
		break;

	case TYPELAYOUT_EQUIV_TAG: /* case TYPELAYOUT_NO_TAG: */
	{
		int allocated = 0; 

#ifdef DEBUG_IO__WRITE
		printf("Equivalent to:
"); 
#endif

		/* is it equivalent to a type variable? */

		if (entry_value < TYPELAYOUT_MAX_VARINT) {
			arg_type_info = make_type_info(type_info, 
				(Word *) entry_value, &allocated);
			mercury_print_type(arg_type_info, data_word);
			if (allocated) {
				free(arg_type_info);
			}
		}
			/* is it a no_tag type? */
		else if (((Word *) entry_value)[0]) {
			mercury_print_simple((Word) &data_word, entry_value, 
				type_info);
		}
			/* is it an equivalent type */
		else {
			arg_type_info = make_type_info(type_info, 
				(Word *) ((Word *) entry_value)[1], &allocated);
			mercury_print_type(arg_type_info, data_word);
			if (allocated) {
				free(arg_type_info);
			}
		}

	}
	break;

	default:
		/* If this happens, the layout data is corrupt */

		fatal_error("Found unused tag value in io__write");
	}
}





#ifdef  USE_TYPE_LAYOUT

	/* 
	 * We'll just pretend the io__external_state is an
	 * integer. For memory copying application, this is
	 * close enough.
	 */

Word * mercury_data_io__base_type_layout_io__external_state_0[] = {
	make_typelayout_for_all_tags(TYPELAYOUT_CONST_TAG, 
		mkbody(TYPELAYOUT_INT_VALUE))
};

#endif

Define_extern_entry(mercury____Unify___io__external_state_0_0);
Define_extern_entry(mercury____Compare___io__external_state_0_0);
Declare_label(mercury____Compare___io__external_state_0_0_i1);
Define_extern_entry(mercury____Index___io__external_state_0_0);
Define_extern_entry(mercury____Type_To_Term___io__external_state_0_0);
Define_extern_entry(mercury____Term_To_Type___io__external_state_0_0);

BEGIN_MODULE(unify_external_state_module)
	init_entry(mercury____Unify___io__external_state_0_0);
	init_entry(mercury____Compare___io__external_state_0_0);
	init_entry(mercury____Index___io__external_state_0_0);
	init_entry(mercury____Type_To_Term___io__external_state_0_0);
	init_entry(mercury____Term_To_Type___io__external_state_0_0);
BEGIN_CODE

Define_entry(mercury____Unify___io__external_state_0_0);
Define_entry(mercury____Compare___io__external_state_0_0);
Define_entry(mercury____Index___io__external_state_0_0);
Define_entry(mercury____Term_To_Type___io__external_state_0_0);
Define_entry(mercury____Type_To_Term___io__external_state_0_0);
	/* the unique mode system should prevent these */
	fatal_error("cannot unify/compare/index/term_to_type/type_to_term io__external_state");

END_MODULE

/* Ensure that the initialization code for the above module gets run. */
/*
INIT sys_init_unify_external_state_module
*/
	/* suppress gcc -Wmissing-decl warning */
void sys_init_unify_external_state_module(void);
void sys_init_unify_external_state_module(void) {
	extern ModuleFunc unify_external_state_module;
	unify_external_state_module();
}




#ifdef  USE_TYPE_LAYOUT

	/* Rest of word is pointer to 2 cell struct */

extern Word * mercury_data___base_type_info_int_0[];
Word * mercury_data_io__base_type_layout_io__stream_0b[] = {
	(Word *) (Integer) mercury_data___base_type_info_int_0
};

Word * mercury_data_io__base_type_layout_io__stream_0a[] = {
	(Word *) ((Integer) 2),
	(Word *) ((Integer) mercury_data_io__base_type_layout_io__stream_0b),
	(Word *) ((Integer) mercury_data_io__base_type_layout_io__stream_0b),
	(Word *) string_const("io__stream", 10)
};

Word * mercury_data_io__base_type_layout_io__stream_0[] = {
	make_typelayout_for_all_tags(TYPELAYOUT_SIMPLE_TAG, 
		((Integer) mercury_data_io__base_type_layout_io__stream_0a))
};

#endif

Define_extern_entry(mercury____Unify___io__stream_0_0);
Define_extern_entry(mercury____Index___io__stream_0_0);
Define_extern_entry(mercury____Compare___io__stream_0_0);
Define_extern_entry(mercury____Term_To_Type___io__stream_0_0);
Define_extern_entry(mercury____Type_To_Term___io__stream_0_0);

BEGIN_MODULE(io_stream_module)
	init_entry(mercury____Unify___io__stream_0_0);
	init_entry(mercury____Index___io__stream_0_0);
	init_entry(mercury____Compare___io__stream_0_0);
	init_entry(mercury____Term_To_Type___io__stream_0_0);
	init_entry(mercury____Type_To_Term___io__stream_0_0);
BEGIN_CODE

Define_entry(mercury____Unify___io__stream_0_0);
	unify_output =
		((MercuryFile*) unify_input1 == (MercuryFile *) unify_input2);
	proceed();

Define_entry(mercury____Index___io__stream_0_0);
	index_output = -1;
	proceed();

Define_entry(mercury____Compare___io__stream_0_0);
	compare_output = ((compare_input1 < compare_input2) ? COMPARE_LESS :
		          (compare_input1 > compare_input2) ? COMPARE_GREATER :
			  				      COMPARE_EQUAL);
	proceed();

Define_entry(mercury____Term_To_Type___io__stream_0_0);
	/* don't know what to put here. */
	fatal_error("cannot convert term to type io__stream");

Define_entry(mercury____Type_To_Term___io__stream_0_0);
	/* don't know what to put here. */
	fatal_error("cannot covert type io__stream to term");

END_MODULE

/* Ensure that the initialization code for the above module gets run. */
/*
INIT sys_init_io_stream_module
*/
void sys_init_io_stream_module(void); /* suppress gcc -Wmissing-decl warning */
void sys_init_io_stream_module(void) {
	extern ModuleFunc io_stream_module;
	io_stream_module();
}




Declare_entry(mercury__io__init_state_2_0);

/* This code is the program startup point -- it is called by the Mercury
   runtime.

   The handwritten code below is almost equivalent to

	io__run :-
		initial_external_state(IO0),
		program_entry_point(IO0, IO),
		final_io_state(IO).

   except that program_entry_point is a variable, which is by default
   set to the address of main/2.
*/

Define_extern_entry(mercury__io__run_0_0);
Declare_label(mercury__io__run_0_0_i1);
Declare_label(mercury__io__run_0_0_i2);

BEGIN_MODULE(io_run_module)
	init_entry(mercury__io__run_0_0);
	init_label(mercury__io__run_0_0_i1);
	init_label(mercury__io__run_0_0_i2);
BEGIN_CODE
Define_entry(mercury__io__run_0_0);
        mkframe("mercury__io__run_0_0", 0, ENTRY(do_fail));
	r1 = initial_external_state();
	noprof_call(ENTRY(mercury__io__init_state_2_0),
		LABEL(mercury__io__run_0_0_i1));
Define_label(mercury__io__run_0_0_i1);
#ifdef	COMPACT_ARGS
#else
	r1 = r2;
#endif
	if (program_entry_point == NULL) {
		fatal_error("no program entry point supplied");
	}

#ifdef  PROFILE_TIME
	prof_init_time_profile();
#endif

	noprof_call(program_entry_point,
		LABEL(mercury__io__run_0_0_i2));

Define_label(mercury__io__run_0_0_i2);

#ifdef  PROFILE_TIME
	prof_turn_off_time_profiling();
	prof_output_addr_table();
#endif
#ifdef  PROFILE_CALLS
	prof_output_addr_pair_table();
#endif

	final_io_state(r2);
	succeed();
END_MODULE

/* Ensure that the initialization code for the above module gets run. */
/*
INIT sys_init_io_run_module
*/
void sys_init_io_run_module(void); /* suppress gcc -Wmissing-decl warning */
void sys_init_io_run_module(void) {
	extern ModuleFunc io_run_module;
	io_run_module();
}




void
mercury_close(MercuryFile* mf)
{
	if (mf != &mercury_stdin &&
	    mf != &mercury_stdout &&
	    mf != &mercury_stderr)
	{
		if (fclose(mf->file) < 0) {
			fprintf(stderr,
				"Mercury runtime: error closing file: %s\n",
				strerror(errno));
			exit(1);
		}
		oldmem(mf);
	}
}




int
mercury_getc(MercuryFile* mf)
{
	int c = getc(mf->file);
	if (c == '\n') {
		mf->line_number++;
	}
	return c;
}




void
mercury_print_binary_string(MercuryFile* mf, const char *s)
{
	if (fprintf(mf->file, "%s", s) < 0) {
		mercury_output_error(mf);
	}
}




void
mercury_print_string(MercuryFile* mf, const char *s)
{
	if (fprintf(mf->file, "%s", s) < 0) {
		mercury_output_error(mf);
	}
	while (*s) {
		if (*s++ == '\n') {
			mf->line_number++;
		}
	}
}




int
mercury_output_error(MercuryFile* mf)
{
	fprintf(stderr,
		"Mercury runtime: error writing to output file: %s\n",
		strerror(errno));
	exit(1);
}




MercuryFile*
mercury_open(const char *filename, const char *type)
{
	MercuryFile *mf;
	FILE *f;
	
	f = fopen(filename, type);
	if (!f) return NULL;
	mf = make(MercuryFile);
	mf->file = f;
	mf->line_number = 1;
	return mf;
}




MercuryFile mercury_stdin = { NULL, 0 };
MercuryFile mercury_stdout = { NULL, 0 };
MercuryFile mercury_stderr = { NULL, 0 };
MercuryFile *mercury_current_text_input = &mercury_stdin;
MercuryFile *mercury_current_text_output = &mercury_stdout;
MercuryFile *mercury_current_binary_input = &mercury_stdin;
MercuryFile *mercury_current_binary_output = &mercury_stdout;

void
mercury_init_io(void)
{
	mercury_stdin.file = stdin;
	mercury_stdout.file = stdout;
	mercury_stderr.file = stderr;
}



extern Word * mercury_data_io__base_type_layout_io__binary_input_stream_0[];
Word * mercury_data_io__base_type_info_io__binary_input_stream_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury____Unify___io__binary_input_stream_0_0),
	(Word *) (Integer) ENTRY(mercury____Index___io__binary_input_stream_0_0),
	(Word *) (Integer) ENTRY(mercury____Compare___io__binary_input_stream_0_0),
	(Word *) (Integer) mercury_data_io__base_type_layout_io__binary_input_stream_0
};

extern Word * mercury_data_io__base_type_layout_io__binary_output_stream_0[];
Word * mercury_data_io__base_type_info_io__binary_output_stream_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury____Unify___io__binary_output_stream_0_0),
	(Word *) (Integer) ENTRY(mercury____Index___io__binary_output_stream_0_0),
	(Word *) (Integer) ENTRY(mercury____Compare___io__binary_output_stream_0_0),
	(Word *) (Integer) mercury_data_io__base_type_layout_io__binary_output_stream_0
};

extern Word * mercury_data_io__base_type_layout_io__error_0[];
Word * mercury_data_io__base_type_info_io__error_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury____Unify___io__error_0_0),
	(Word *) (Integer) ENTRY(mercury____Index___io__error_0_0),
	(Word *) (Integer) ENTRY(mercury____Compare___io__error_0_0),
	(Word *) (Integer) mercury_data_io__base_type_layout_io__error_0
};

Declare_entry(mercury____Unify___io__external_state_0_0);
Declare_entry(mercury____Index___io__external_state_0_0);
Declare_entry(mercury____Compare___io__external_state_0_0);
extern Word * mercury_data_io__base_type_layout_io__external_state_0[];
Word * mercury_data_io__base_type_info_io__external_state_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury____Unify___io__external_state_0_0),
	(Word *) (Integer) ENTRY(mercury____Index___io__external_state_0_0),
	(Word *) (Integer) ENTRY(mercury____Compare___io__external_state_0_0),
	(Word *) (Integer) mercury_data_io__base_type_layout_io__external_state_0
};

extern Word * mercury_data_io__base_type_layout_io__input_stream_0[];
Word * mercury_data_io__base_type_info_io__input_stream_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury____Unify___io__input_stream_0_0),
	(Word *) (Integer) ENTRY(mercury____Index___io__input_stream_0_0),
	(Word *) (Integer) ENTRY(mercury____Compare___io__input_stream_0_0),
	(Word *) (Integer) mercury_data_io__base_type_layout_io__input_stream_0
};

extern Word * mercury_data_io__base_type_layout_io__output_stream_0[];
Word * mercury_data_io__base_type_info_io__output_stream_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury____Unify___io__output_stream_0_0),
	(Word *) (Integer) ENTRY(mercury____Index___io__output_stream_0_0),
	(Word *) (Integer) ENTRY(mercury____Compare___io__output_stream_0_0),
	(Word *) (Integer) mercury_data_io__base_type_layout_io__output_stream_0
};

extern Word * mercury_data_io__base_type_layout_io__poly_type_0[];
Word * mercury_data_io__base_type_info_io__poly_type_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury____Unify___io__poly_type_0_0),
	(Word *) (Integer) ENTRY(mercury____Index___io__poly_type_0_0),
	(Word *) (Integer) ENTRY(mercury____Compare___io__poly_type_0_0),
	(Word *) (Integer) mercury_data_io__base_type_layout_io__poly_type_0
};

extern Word * mercury_data_io__base_type_layout_io__read_result_1[];
Word * mercury_data_io__base_type_info_io__read_result_1[] = {
	(Word *) ((Integer) 1),
	(Word *) (Integer) ENTRY(mercury____Unify___io__read_result_1_0),
	(Word *) (Integer) ENTRY(mercury____Index___io__read_result_1_0),
	(Word *) (Integer) ENTRY(mercury____Compare___io__read_result_1_0),
	(Word *) (Integer) mercury_data_io__base_type_layout_io__read_result_1
};

extern Word * mercury_data_io__base_type_layout_io__res_0[];
Word * mercury_data_io__base_type_info_io__res_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury____Unify___io__res_0_0),
	(Word *) (Integer) ENTRY(mercury____Index___io__res_0_0),
	(Word *) (Integer) ENTRY(mercury____Compare___io__res_0_0),
	(Word *) (Integer) mercury_data_io__base_type_layout_io__res_0
};

extern Word * mercury_data_io__base_type_layout_io__res_1[];
Word * mercury_data_io__base_type_info_io__res_1[] = {
	(Word *) ((Integer) 1),
	(Word *) (Integer) ENTRY(mercury____Unify___io__res_1_0),
	(Word *) (Integer) ENTRY(mercury____Index___io__res_1_0),
	(Word *) (Integer) ENTRY(mercury____Compare___io__res_1_0),
	(Word *) (Integer) mercury_data_io__base_type_layout_io__res_1
};

extern Word * mercury_data_io__base_type_layout_io__result_0[];
Word * mercury_data_io__base_type_info_io__result_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury____Unify___io__result_0_0),
	(Word *) (Integer) ENTRY(mercury____Index___io__result_0_0),
	(Word *) (Integer) ENTRY(mercury____Compare___io__result_0_0),
	(Word *) (Integer) mercury_data_io__base_type_layout_io__result_0
};

extern Word * mercury_data_io__base_type_layout_io__result_1[];
Word * mercury_data_io__base_type_info_io__result_1[] = {
	(Word *) ((Integer) 1),
	(Word *) (Integer) ENTRY(mercury____Unify___io__result_1_0),
	(Word *) (Integer) ENTRY(mercury____Index___io__result_1_0),
	(Word *) (Integer) ENTRY(mercury____Compare___io__result_1_0),
	(Word *) (Integer) mercury_data_io__base_type_layout_io__result_1
};

extern Word * mercury_data_io__base_type_layout_io__state_0[];
Word * mercury_data_io__base_type_info_io__state_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury____Unify___io__state_0_0),
	(Word *) (Integer) ENTRY(mercury____Index___io__state_0_0),
	(Word *) (Integer) ENTRY(mercury____Compare___io__state_0_0),
	(Word *) (Integer) mercury_data_io__base_type_layout_io__state_0
};

Declare_entry(mercury____Unify___io__stream_0_0);
Declare_entry(mercury____Index___io__stream_0_0);
Declare_entry(mercury____Compare___io__stream_0_0);
extern Word * mercury_data_io__base_type_layout_io__stream_0[];
Word * mercury_data_io__base_type_info_io__stream_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury____Unify___io__stream_0_0),
	(Word *) (Integer) ENTRY(mercury____Index___io__stream_0_0),
	(Word *) (Integer) ENTRY(mercury____Compare___io__stream_0_0),
	(Word *) (Integer) mercury_data_io__base_type_layout_io__stream_0
};

Declare_entry(mercury__unused_0_0);
extern Word * mercury_data_io__base_type_layout_io__stream_names_0[];
Word * mercury_data_io__base_type_info_io__stream_names_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) mercury_data_io__base_type_layout_io__stream_names_0
};

extern Word * mercury_data_io__base_type_layout_io__stream_putback_0[];
Word * mercury_data_io__base_type_info_io__stream_putback_0[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) ENTRY(mercury__unused_0_0),
	(Word *) (Integer) mercury_data_io__base_type_layout_io__stream_putback_0
};

extern Word * mercury_data_io__common_10[];
Word * mercury_data_io__base_type_layout_io__stream_putback_0[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_io__common_10),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_io__common_10),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_io__common_10),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_io__common_10)
};

extern Word * mercury_data_io__common_11[];
Word * mercury_data_io__base_type_layout_io__stream_names_0[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_io__common_11),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_io__common_11),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_io__common_11),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_io__common_11)
};

extern Word * mercury_data_io__common_15[];
Word * mercury_data_io__base_type_layout_io__state_0[] = {
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_io__common_15),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1)))
};

extern Word * mercury_data_io__common_16[];
extern Word * mercury_data_io__common_17[];
extern Word * mercury_data_io__common_19[];
Word * mercury_data_io__base_type_layout_io__result_1[] = {
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_io__common_16),
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_io__common_17),
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_io__common_19),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1)))
};

extern Word * mercury_data_io__common_20[];
Word * mercury_data_io__base_type_layout_io__result_0[] = {
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_io__common_20),
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_io__common_19),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1)))
};

Word * mercury_data_io__base_type_layout_io__res_1[] = {
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_io__common_17),
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_io__common_19),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1)))
};

extern Word * mercury_data_io__common_21[];
Word * mercury_data_io__base_type_layout_io__res_0[] = {
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_io__common_21),
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_io__common_19),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1))),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1)))
};

extern Word * mercury_data_io__common_23[];
Word * mercury_data_io__base_type_layout_io__read_result_1[] = {
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_io__common_16),
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_io__common_17),
	(Word *) (Integer) mkword(mktag(1), (Integer) mercury_data_io__common_23),
	(Word *) (Integer) mkword(mktag(0), mkbody(((Integer) 1)))
};

extern Word * mercury_data_io__common_25[];
Word * mercury_data_io__base_type_layout_io__poly_type_0[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_io__common_25),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_io__common_25),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_io__common_25),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_io__common_25)
};

extern Word * mercury_data_io__common_27[];
Word * mercury_data_io__base_type_layout_io__output_stream_0[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_io__common_27),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_io__common_27),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_io__common_27),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_io__common_27)
};

Word * mercury_data_io__base_type_layout_io__input_stream_0[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_io__common_27),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_io__common_27),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_io__common_27),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_io__common_27)
};

extern Word * mercury_data_io__common_28[];
Word * mercury_data_io__base_type_layout_io__error_0[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_io__common_28),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_io__common_28),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_io__common_28),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_io__common_28)
};

Word * mercury_data_io__base_type_layout_io__binary_output_stream_0[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_io__common_27),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_io__common_27),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_io__common_27),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_io__common_27)
};

Word * mercury_data_io__base_type_layout_io__binary_input_stream_0[] = {
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_io__common_27),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_io__common_27),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_io__common_27),
	(Word *) (Integer) mkword(mktag(3), (Integer) mercury_data_io__common_27)
};

Word * mercury_data_io__common_0[] = {
	(Word *) string_const("read error", 10)
};

Word * mercury_data_io__common_1[] = {
	(Word *) string_const("io__read_anything : the term read was not a valid type", 54),
	(Word *) ((Integer) 0)
};

Word * mercury_data_io__common_2[] = {
	(Word *) string_const("can't open input file", 21)
};

Word * mercury_data_io__common_3[] = {
	(Word *) string_const("can't open output file", 22)
};

Word * mercury_data_io__common_4[] = {
	(Word *) string_const("can't append to file", 20)
};

Word * mercury_data_io__common_5[] = {
	(Word *) string_const("can't invoke system command", 27)
};

extern Word * mercury_data_mercury_builtin__base_type_info_list_1[];
extern Word * mercury_data___base_type_info_character_0[];
Word * mercury_data_io__common_6[] = {
	(Word *) (Integer) mercury_data_mercury_builtin__base_type_info_list_1,
	(Word *) (Integer) mercury_data___base_type_info_character_0
};

Word mercury_data_io__common_7[] = {
	(Integer) mkword(mktag(0), mkbody(((Integer) 0)))
};

extern Word * mercury_data_tree234__base_type_info_tree234_2[];
extern Word * mercury_data___base_type_info_string_0[];
Word * mercury_data_io__common_8[] = {
	(Word *) (Integer) mercury_data_tree234__base_type_info_tree234_2,
	(Word *) (Integer) mercury_data_io__base_type_info_io__stream_0,
	(Word *) (Integer) mercury_data___base_type_info_string_0
};

Word * mercury_data_io__common_9[] = {
	(Word *) (Integer) mercury_data_tree234__base_type_info_tree234_2,
	(Word *) (Integer) mercury_data_io__base_type_info_io__stream_0,
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_io__common_6)
};

Word * mercury_data_io__common_10[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_io__common_9)
};

Word * mercury_data_io__common_11[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_io__common_8)
};

extern Word * mercury_data_ops__base_type_info_ops__table_0[];
Word * mercury_data_io__common_12[] = {
	(Word *) (Integer) mercury_data_ops__base_type_info_ops__table_0
};

extern Word * mercury_data_std_util__base_type_info_univ_0[];
Word * mercury_data_io__common_13[] = {
	(Word *) (Integer) mercury_data_std_util__base_type_info_univ_0
};

Word * mercury_data_io__common_14[] = {
	(Word *) (Integer) mercury_data_io__base_type_info_io__external_state_0
};

Word * mercury_data_io__common_15[] = {
	(Word *) ((Integer) 5),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_io__common_8),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_io__common_9),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_io__common_12),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_io__common_13),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_io__common_14),
	(Word *) string_const("io__state", 9)
};

Word * mercury_data_io__common_16[] = {
	(Word *) ((Integer) 0),
	(Word *) ((Integer) 1),
	(Word *) string_const("eof", 3)
};

Word * mercury_data_io__common_17[] = {
	(Word *) ((Integer) 1),
	(Word *) ((Integer) 1),
	(Word *) string_const("ok", 2)
};

Word * mercury_data_io__common_18[] = {
	(Word *) (Integer) mercury_data___base_type_info_string_0
};

Word * mercury_data_io__common_19[] = {
	(Word *) ((Integer) 1),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_io__common_18),
	(Word *) string_const("error", 5)
};

Word * mercury_data_io__common_20[] = {
	(Word *) ((Integer) 0),
	(Word *) ((Integer) 2),
	(Word *) string_const("ok", 2),
	(Word *) string_const("eof", 3)
};

Word * mercury_data_io__common_21[] = {
	(Word *) ((Integer) 0),
	(Word *) ((Integer) 1),
	(Word *) string_const("ok", 2)
};

extern Word * mercury_data___base_type_info_int_0[];
Word * mercury_data_io__common_22[] = {
	(Word *) (Integer) mercury_data___base_type_info_int_0
};

Word * mercury_data_io__common_23[] = {
	(Word *) ((Integer) 2),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_io__common_18),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_io__common_22),
	(Word *) string_const("error", 5)
};

extern Word * mercury_data_string__base_type_info_string__poly_type_0[];
Word * mercury_data_io__common_24[] = {
	(Word *) (Integer) mercury_data_string__base_type_info_string__poly_type_0
};

Word * mercury_data_io__common_25[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_io__common_24)
};

Word * mercury_data_io__common_26[] = {
	(Word *) (Integer) mercury_data_io__base_type_info_io__stream_0
};

Word * mercury_data_io__common_27[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_io__common_26)
};

Word * mercury_data_io__common_28[] = {
	(Word *) ((Integer) 0),
	(Word *) (Integer) mkword(mktag(0), (Integer) mercury_data_io__common_18)
};

BEGIN_MODULE(mercury__io_module0)
	init_entry(mercury____Index___io_io__read_result_1__ua10000_2_0);
	init_label(mercury____Index___io_io__read_result_1__ua10000_2_0_i4);
	init_label(mercury____Index___io_io__read_result_1__ua10000_2_0_i5);
BEGIN_CODE

/* code for predicate '__Index___io_io__read_result_1__ua10000'/2 in mode 0 */
Define_static(mercury____Index___io_io__read_result_1__ua10000_2_0);
	r2 = tag((Integer) r1);
	if (((Integer) r2 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury____Index___io_io__read_result_1__ua10000_2_0_i4);
	r1 = ((Integer) 1);
	proceed();
Define_label(mercury____Index___io_io__read_result_1__ua10000_2_0_i4);
	if (((Integer) r2 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury____Index___io_io__read_result_1__ua10000_2_0_i5);
	r1 = ((Integer) 0);
	proceed();
Define_label(mercury____Index___io_io__read_result_1__ua10000_2_0_i5);
	r1 = ((Integer) 2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module1)
	init_entry(mercury____Index___io_io__result_1__ua10000_2_0);
	init_label(mercury____Index___io_io__result_1__ua10000_2_0_i4);
	init_label(mercury____Index___io_io__result_1__ua10000_2_0_i5);
BEGIN_CODE

/* code for predicate '__Index___io_io__result_1__ua10000'/2 in mode 0 */
Define_static(mercury____Index___io_io__result_1__ua10000_2_0);
	r2 = tag((Integer) r1);
	if (((Integer) r2 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury____Index___io_io__result_1__ua10000_2_0_i4);
	r1 = ((Integer) 1);
	proceed();
Define_label(mercury____Index___io_io__result_1__ua10000_2_0_i4);
	if (((Integer) r2 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury____Index___io_io__result_1__ua10000_2_0_i5);
	r1 = ((Integer) 0);
	proceed();
Define_label(mercury____Index___io_io__result_1__ua10000_2_0_i5);
	r1 = ((Integer) 2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module2)
	init_entry(mercury____Index___io_io__res_1__ua10000_2_0);
	init_label(mercury____Index___io_io__res_1__ua10000_2_0_i3);
BEGIN_CODE

/* code for predicate '__Index___io_io__res_1__ua10000'/2 in mode 0 */
Define_static(mercury____Index___io_io__res_1__ua10000_2_0);
	if ((tag((Integer) r1) == mktag(((Integer) 0))))
		GOTO_LABEL(mercury____Index___io_io__res_1__ua10000_2_0_i3);
	r1 = ((Integer) 1);
	proceed();
Define_label(mercury____Index___io_io__res_1__ua10000_2_0_i3);
	r1 = ((Integer) 0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module3)
	init_entry(mercury____Index___io_io__state_0__ua10000_2_0);
BEGIN_CODE

/* code for predicate '__Index___io_io__state_0__ua10000'/2 in mode 0 */
Define_static(mercury____Index___io_io__state_0__ua10000_2_0);
	r1 = ((Integer) 0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module4)
	init_entry(mercury__io__read_char_3_0);
	init_label(mercury__io__read_char_3_0_i2);
BEGIN_CODE

/* code for predicate 'io__read_char'/3 in mode 0 */
Define_entry(mercury__io__read_char_3_0);
	incr_sp_push_msg(1, "io__read_char");
	detstackvar(1) = (Integer) succip;
	{
		call_localret(STATIC(mercury__io__input_stream_3_0),
		mercury__io__read_char_3_0_i2,
		ENTRY(mercury__io__read_char_3_0));
	}
Define_label(mercury__io__read_char_3_0_i2);
	update_prof_current_proc(LABEL(mercury__io__read_char_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	{
		tailcall(STATIC(mercury__io__read_char_4_0),
		ENTRY(mercury__io__read_char_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__io_module5)
	init_entry(mercury__io__read_word_3_0);
	init_label(mercury__io__read_word_3_0_i2);
BEGIN_CODE

/* code for predicate 'io__read_word'/3 in mode 0 */
Define_entry(mercury__io__read_word_3_0);
	incr_sp_push_msg(1, "io__read_word");
	detstackvar(1) = (Integer) succip;
	{
		call_localret(STATIC(mercury__io__input_stream_3_0),
		mercury__io__read_word_3_0_i2,
		ENTRY(mercury__io__read_word_3_0));
	}
Define_label(mercury__io__read_word_3_0_i2);
	update_prof_current_proc(LABEL(mercury__io__read_word_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	{
		tailcall(STATIC(mercury__io__read_word_4_0),
		ENTRY(mercury__io__read_word_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__io_module6)
	init_entry(mercury__io__read_line_3_0);
	init_label(mercury__io__read_line_3_0_i2);
BEGIN_CODE

/* code for predicate 'io__read_line'/3 in mode 0 */
Define_entry(mercury__io__read_line_3_0);
	incr_sp_push_msg(1, "io__read_line");
	detstackvar(1) = (Integer) succip;
	{
		call_localret(STATIC(mercury__io__input_stream_3_0),
		mercury__io__read_line_3_0_i2,
		ENTRY(mercury__io__read_line_3_0));
	}
Define_label(mercury__io__read_line_3_0_i2);
	update_prof_current_proc(LABEL(mercury__io__read_line_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	{
		tailcall(STATIC(mercury__io__read_line_4_0),
		ENTRY(mercury__io__read_line_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__io_module7)
	init_entry(mercury__io__putback_char_3_0);
	init_label(mercury__io__putback_char_3_0_i2);
BEGIN_CODE

/* code for predicate 'io__putback_char'/3 in mode 0 */
Define_entry(mercury__io__putback_char_3_0);
	incr_sp_push_msg(2, "io__putback_char");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
		call_localret(STATIC(mercury__io__input_stream_3_0),
		mercury__io__putback_char_3_0_i2,
		ENTRY(mercury__io__putback_char_3_0));
	}
Define_label(mercury__io__putback_char_3_0_i2);
	update_prof_current_proc(LABEL(mercury__io__putback_char_3_0));
	r3 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
		tailcall(STATIC(mercury__io__putback_char_4_0),
		ENTRY(mercury__io__putback_char_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__io_module8)
	init_entry(mercury__io__read_char_4_0);
	init_label(mercury__io__read_char_4_0_i2);
	init_label(mercury__io__read_char_4_0_i3);
	init_label(mercury__io__read_char_4_0_i8);
	init_label(mercury__io__read_char_4_0_i7);
BEGIN_CODE

/* code for predicate 'io__read_char'/4 in mode 0 */
Define_entry(mercury__io__read_char_4_0);
	incr_sp_push_msg(2, "io__read_char");
	detstackvar(2) = (Integer) succip;
	call_localret(STATIC(mercury__io__read_char_code_4_0),
		mercury__io__read_char_4_0_i2,
		ENTRY(mercury__io__read_char_4_0));
Define_label(mercury__io__read_char_4_0_i2);
	update_prof_current_proc(LABEL(mercury__io__read_char_4_0));
	if (((Integer) r1 != ((Integer) -1)))
		GOTO_LABEL(mercury__io__read_char_4_0_i3);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__io__read_char_4_0_i3);
	detstackvar(1) = (Integer) r2;
	{
	Declare_entry(mercury__char__to_int_2_2);
	call_localret(ENTRY(mercury__char__to_int_2_2),
		mercury__io__read_char_4_0_i8,
		ENTRY(mercury__io__read_char_4_0));
	}
Define_label(mercury__io__read_char_4_0_i8);
	update_prof_current_proc(LABEL(mercury__io__read_char_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__io__read_char_4_0_i7);
	tag_incr_hp(r1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__io__read_char_4_0_i7);
	r2 = (Integer) detstackvar(1);
	r1 = (Integer) mkword(mktag(2), (Integer) mercury_data_io__common_0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module9)
	init_entry(mercury__io__read_word_4_0);
	init_label(mercury__io__read_word_4_0_i2);
	init_label(mercury__io__read_word_4_0_i6);
	init_label(mercury__io__read_word_4_0_i5);
BEGIN_CODE

/* code for predicate 'io__read_word'/4 in mode 0 */
Define_entry(mercury__io__read_word_4_0);
	incr_sp_push_msg(2, "io__read_word");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	{
		call_localret(STATIC(mercury__io__ignore_whitespace_4_0),
		mercury__io__read_word_4_0_i2,
		ENTRY(mercury__io__read_word_4_0));
	}
Define_label(mercury__io__read_word_4_0_i2);
	update_prof_current_proc(LABEL(mercury__io__read_word_4_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__io__read_word_4_0_i5);
	if ((unmkbody((Integer) r1) != ((Integer) 0)))
		GOTO_LABEL(mercury__io__read_word_4_0_i6);
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	tailcall(STATIC(mercury__io__read_word_2_4_0),
		ENTRY(mercury__io__read_word_4_0));
Define_label(mercury__io__read_word_4_0_i6);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__io__read_word_4_0_i5);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(2), ((Integer) 1));
	field(mktag(2), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module10)
	init_entry(mercury__io__read_line_4_0);
	init_label(mercury__io__read_line_4_0_i2);
	init_label(mercury__io__read_line_4_0_i5);
	init_label(mercury__io__read_line_4_0_i7);
	init_label(mercury__io__read_line_4_0_i10);
	init_label(mercury__io__read_line_4_0_i13);
	init_label(mercury__io__read_line_4_0_i3);
BEGIN_CODE

/* code for predicate 'io__read_line'/4 in mode 0 */
Define_entry(mercury__io__read_line_4_0);
	incr_sp_push_msg(2, "io__read_line");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	{
		call_localret(STATIC(mercury__io__read_char_4_0),
		mercury__io__read_line_4_0_i2,
		ENTRY(mercury__io__read_line_4_0));
	}
Define_label(mercury__io__read_line_4_0_i2);
	update_prof_current_proc(LABEL(mercury__io__read_line_4_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__io__read_line_4_0_i5);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__io__read_line_4_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__io__read_line_4_0_i3);
	r3 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	if (((Integer) r3 != ((Integer) 10)))
		GOTO_LABEL(mercury__io__read_line_4_0_i7);
	tag_incr_hp(r1, mktag(1), ((Integer) 1));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) tempr1;
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
	}
Define_label(mercury__io__read_line_4_0_i7);
	r1 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r3;
	localcall(mercury__io__read_line_4_0,
		LABEL(mercury__io__read_line_4_0_i10),
		ENTRY(mercury__io__read_line_4_0));
Define_label(mercury__io__read_line_4_0_i10);
	update_prof_current_proc(LABEL(mercury__io__read_line_4_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__io__read_line_4_0_i13);
	tag_incr_hp(r1, mktag(1), ((Integer) 1));
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__io__read_line_4_0_i13);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__io__read_line_4_0_i3);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 1));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(1);
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) tempr1;
	}
Define_label(mercury__io__read_line_4_0_i3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module11)
	init_entry(mercury__io__putback_char_4_0);
BEGIN_CODE

/* code for predicate 'io__putback_char'/4 in mode 0 */
Define_entry(mercury__io__putback_char_4_0);
	{
		Word	File;
		Char	Character;
		Word	IO0;
		Word	IO;
		File = (Integer) r1;
		Character = (Integer) r2;
		IO0 = (Integer) r3;
		{
	MercuryFile* mf = (MercuryFile *)File;
	if (Character == '\n') {
		mf->line_number--;
	}
	/* XXX should work even if ungetc() fails */
	if (ungetc(Character, mf->file) == EOF) {
		fatal_error("io__putback_char: ungetc failed");
	}
	update_io(IO0, IO);
}
		r4 = IO;

	}
	r1 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module12)
	init_entry(mercury__io__read_anything_3_0);
	init_label(mercury__io__read_anything_3_0_i2);
	init_label(mercury__io__read_anything_3_0_i5);
	init_label(mercury__io__read_anything_3_0_i6);
	init_label(mercury__io__read_anything_3_0_i9);
	init_label(mercury__io__read_anything_3_0_i8);
BEGIN_CODE

/* code for predicate 'io__read_anything'/3 in mode 0 */
Define_entry(mercury__io__read_anything_3_0);
	incr_sp_push_msg(2, "io__read_anything");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
	Declare_entry(mercury__term_io__read_term_3_0);
	call_localret(ENTRY(mercury__term_io__read_term_3_0),
		mercury__io__read_anything_3_0_i2,
		ENTRY(mercury__io__read_anything_3_0));
	}
Define_label(mercury__io__read_anything_3_0_i2);
	update_prof_current_proc(LABEL(mercury__io__read_anything_3_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__io__read_anything_3_0_i5);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__io__read_anything_3_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__io__read_anything_3_0_i6);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(2), ((Integer) 2));
	field(mktag(2), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(2), (Integer) r1, ((Integer) 1)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 1));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__io__read_anything_3_0_i6);
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) field(mktag(2), (Integer) r3, ((Integer) 1));
	{
	Declare_entry(mercury__term_to_type_2_0);
	call_localret(ENTRY(mercury__term_to_type_2_0),
		mercury__io__read_anything_3_0_i9,
		ENTRY(mercury__io__read_anything_3_0));
	}
Define_label(mercury__io__read_anything_3_0_i9);
	update_prof_current_proc(LABEL(mercury__io__read_anything_3_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__io__read_anything_3_0_i8);
	tag_incr_hp(r1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__io__read_anything_3_0_i8);
	r2 = (Integer) detstackvar(1);
	r1 = (Integer) mkword(mktag(2), (Integer) mercury_data_io__common_1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module13)
	init_entry(mercury__io__read_anything_4_0);
	init_label(mercury__io__read_anything_4_0_i2);
	init_label(mercury__io__read_anything_4_0_i3);
	init_label(mercury__io__read_anything_4_0_i4);
BEGIN_CODE

/* code for predicate 'io__read_anything'/4 in mode 0 */
Define_entry(mercury__io__read_anything_4_0);
	incr_sp_push_msg(2, "io__read_anything");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	{
		call_localret(STATIC(mercury__io__set_input_stream_4_0),
		mercury__io__read_anything_4_0_i2,
		ENTRY(mercury__io__read_anything_4_0));
	}
Define_label(mercury__io__read_anything_4_0_i2);
	update_prof_current_proc(LABEL(mercury__io__read_anything_4_0));
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r3;
	{
		call_localret(STATIC(mercury__io__read_anything_3_0),
		mercury__io__read_anything_4_0_i3,
		ENTRY(mercury__io__read_anything_4_0));
	}
Define_label(mercury__io__read_anything_4_0_i3);
	update_prof_current_proc(LABEL(mercury__io__read_anything_4_0));
	r3 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r3;
	{
		call_localret(STATIC(mercury__io__set_input_stream_4_0),
		mercury__io__read_anything_4_0_i4,
		ENTRY(mercury__io__read_anything_4_0));
	}
Define_label(mercury__io__read_anything_4_0_i4);
	update_prof_current_proc(LABEL(mercury__io__read_anything_4_0));
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module14)
	init_entry(mercury__io__ignore_whitespace_3_0);
	init_label(mercury__io__ignore_whitespace_3_0_i2);
BEGIN_CODE

/* code for predicate 'io__ignore_whitespace'/3 in mode 0 */
Define_entry(mercury__io__ignore_whitespace_3_0);
	incr_sp_push_msg(1, "io__ignore_whitespace");
	detstackvar(1) = (Integer) succip;
	{
		call_localret(STATIC(mercury__io__input_stream_3_0),
		mercury__io__ignore_whitespace_3_0_i2,
		ENTRY(mercury__io__ignore_whitespace_3_0));
	}
Define_label(mercury__io__ignore_whitespace_3_0_i2);
	update_prof_current_proc(LABEL(mercury__io__ignore_whitespace_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	{
		tailcall(STATIC(mercury__io__ignore_whitespace_4_0),
		ENTRY(mercury__io__ignore_whitespace_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__io_module15)
	init_entry(mercury__io__ignore_whitespace_4_0);
	init_label(mercury__io__ignore_whitespace_4_0_i2);
	init_label(mercury__io__ignore_whitespace_4_0_i5);
	init_label(mercury__io__ignore_whitespace_4_0_i9);
	init_label(mercury__io__ignore_whitespace_4_0_i8);
	init_label(mercury__io__ignore_whitespace_4_0_i12);
	init_label(mercury__io__ignore_whitespace_4_0_i6);
BEGIN_CODE

/* code for predicate 'io__ignore_whitespace'/4 in mode 0 */
Define_entry(mercury__io__ignore_whitespace_4_0);
	incr_sp_push_msg(4, "io__ignore_whitespace");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	{
		call_localret(STATIC(mercury__io__read_char_4_0),
		mercury__io__ignore_whitespace_4_0_i2,
		ENTRY(mercury__io__ignore_whitespace_4_0));
	}
Define_label(mercury__io__ignore_whitespace_4_0_i2);
	update_prof_current_proc(LABEL(mercury__io__ignore_whitespace_4_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__io__ignore_whitespace_4_0_i5);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 1)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__io__ignore_whitespace_4_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__io__ignore_whitespace_4_0_i6);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	{
	Declare_entry(mercury__char__is_whitespace_1_0);
	call_localret(ENTRY(mercury__char__is_whitespace_1_0),
		mercury__io__ignore_whitespace_4_0_i9,
		ENTRY(mercury__io__ignore_whitespace_4_0));
	}
Define_label(mercury__io__ignore_whitespace_4_0_i9);
	update_prof_current_proc(LABEL(mercury__io__ignore_whitespace_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__io__ignore_whitespace_4_0_i8);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	localtailcall(mercury__io__ignore_whitespace_4_0,
		ENTRY(mercury__io__ignore_whitespace_4_0));
Define_label(mercury__io__ignore_whitespace_4_0_i8);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	{
		call_localret(STATIC(mercury__io__putback_char_4_0),
		mercury__io__ignore_whitespace_4_0_i12,
		ENTRY(mercury__io__ignore_whitespace_4_0));
	}
Define_label(mercury__io__ignore_whitespace_4_0_i12);
	update_prof_current_proc(LABEL(mercury__io__ignore_whitespace_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__io__ignore_whitespace_4_0_i6);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(2), (Integer) r3, ((Integer) 0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module16)
	init_entry(mercury__io__write_string_3_0);
BEGIN_CODE

/* code for predicate 'io__write_string'/3 in mode 0 */
Define_entry(mercury__io__write_string_3_0);
	{
		String	Message;
		Word	IO0;
		Word	IO;
		Message = (String) (Integer) r1;
		IO0 = (Integer) r2;
		
	mercury_print_string(mercury_current_text_output, Message);
	update_io(IO0, IO);

		r3 = IO;

	}
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module17)
	init_entry(mercury__io__write_string_4_0);
BEGIN_CODE

/* code for predicate 'io__write_string'/4 in mode 0 */
Define_entry(mercury__io__write_string_4_0);
	{
		Word	Stream;
		String	Message;
		Word	IO0;
		Word	IO;
		Stream = (Integer) r1;
		Message = (String) (Integer) r2;
		IO0 = (Integer) r3;
		{
	MercuryFile *stream = (MercuryFile *) Stream;
	mercury_print_string(stream, Message);
	update_io(IO0, IO);
}
		r4 = IO;

	}
	r1 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module18)
	init_entry(mercury__io__write_strings_3_0);
	init_label(mercury__io__write_strings_3_0_i2);
BEGIN_CODE

/* code for predicate 'io__write_strings'/3 in mode 0 */
Define_entry(mercury__io__write_strings_3_0);
	incr_sp_push_msg(2, "io__write_strings");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
		call_localret(STATIC(mercury__io__output_stream_3_0),
		mercury__io__write_strings_3_0_i2,
		ENTRY(mercury__io__write_strings_3_0));
	}
Define_label(mercury__io__write_strings_3_0_i2);
	update_prof_current_proc(LABEL(mercury__io__write_strings_3_0));
	r3 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
		tailcall(STATIC(mercury__io__write_strings_4_0),
		ENTRY(mercury__io__write_strings_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__io_module19)
	init_entry(mercury__io__write_strings_4_0);
	init_label(mercury__io__write_strings_4_0_i4);
	init_label(mercury__io__write_strings_4_0_i1002);
BEGIN_CODE

/* code for predicate 'io__write_strings'/4 in mode 0 */
Define_entry(mercury__io__write_strings_4_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__io__write_strings_4_0_i1002);
	incr_sp_push_msg(3, "io__write_strings");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	{
		call_localret(STATIC(mercury__io__write_string_4_0),
		mercury__io__write_strings_4_0_i4,
		ENTRY(mercury__io__write_strings_4_0));
	}
Define_label(mercury__io__write_strings_4_0_i4);
	update_prof_current_proc(LABEL(mercury__io__write_strings_4_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__io__write_strings_4_0,
		ENTRY(mercury__io__write_strings_4_0));
Define_label(mercury__io__write_strings_4_0_i1002);
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module20)
	init_entry(mercury__io__write_char_3_0);
BEGIN_CODE

/* code for predicate 'io__write_char'/3 in mode 0 */
Define_entry(mercury__io__write_char_3_0);
	{
		Char	Character;
		Word	IO0;
		Word	IO;
		Character = (Integer) r1;
		IO0 = (Integer) r2;
		
	if (putc(Character, mercury_current_text_output->file) < 0) {
		mercury_output_error(mercury_current_text_output);
	}
	if (Character == '\n') {
		mercury_current_text_output->line_number++;
	}
	update_io(IO0, IO);

		r3 = IO;

	}
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module21)
	init_entry(mercury__io__write_char_4_0);
BEGIN_CODE

/* code for predicate 'io__write_char'/4 in mode 0 */
Define_entry(mercury__io__write_char_4_0);
	{
		Word	Stream;
		Char	Character;
		Word	IO0;
		Word	IO;
		Stream = (Integer) r1;
		Character = (Integer) r2;
		IO0 = (Integer) r3;
		{
	MercuryFile *stream = (MercuryFile *) Stream;
	if (putc(Character, stream->file) < 0) {
		mercury_output_error(stream);
	}
	if (Character == '\n') {
		stream->line_number++;
	}
	update_io(IO0, IO);
}
		r4 = IO;

	}
	r1 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module22)
	init_entry(mercury__io__write_int_3_0);
BEGIN_CODE

/* code for predicate 'io__write_int'/3 in mode 0 */
Define_entry(mercury__io__write_int_3_0);
	{
		Integer	Val;
		Word	IO0;
		Word	IO;
		Val = (Integer) r1;
		IO0 = (Integer) r2;
		
	if (fprintf(mercury_current_text_output->file, "%ld", (long) Val) < 0) {
		mercury_output_error(mercury_current_text_output);
	}
	update_io(IO0, IO);

		r3 = IO;

	}
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module23)
	init_entry(mercury__io__write_int_4_0);
BEGIN_CODE

/* code for predicate 'io__write_int'/4 in mode 0 */
Define_entry(mercury__io__write_int_4_0);
	{
		Word	Stream;
		Integer	Val;
		Word	IO0;
		Word	IO;
		Stream = (Integer) r1;
		Val = (Integer) r2;
		IO0 = (Integer) r3;
		{
	MercuryFile *stream = (MercuryFile *) Stream;
	if (fprintf(stream->file, "%ld", (long) Val) < 0) {
		mercury_output_error(stream);
	}
	update_io(IO0, IO);
}
		r4 = IO;

	}
	r1 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module24)
	init_entry(mercury__io__write_float_3_0);
BEGIN_CODE

/* code for predicate 'io__write_float'/3 in mode 0 */
Define_entry(mercury__io__write_float_3_0);
	{
		Float	Val;
		Word	IO0;
		Word	IO;
		Val = word_to_float((Integer) r1);
		IO0 = (Integer) r2;
		
	if (fprintf(mercury_current_text_output->file, "%#.15g", Val) < 0) {
		mercury_output_error(mercury_current_text_output);
	}
	update_io(IO0, IO);

		r3 = IO;

	}
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module25)
	init_entry(mercury__io__write_float_4_0);
BEGIN_CODE

/* code for predicate 'io__write_float'/4 in mode 0 */
Define_entry(mercury__io__write_float_4_0);
	{
		Word	Stream;
		Float	Val;
		Word	IO0;
		Word	IO;
		Stream = (Integer) r1;
		Val = word_to_float((Integer) r2);
		IO0 = (Integer) r3;
		{
	MercuryFile *stream = (MercuryFile *) Stream;
	if (fprintf(stream->file, "%.15g", Val) < 0) {
		mercury_output_error(stream);
	}
	update_io(IO0, IO);
}
		r4 = IO;

	}
	r1 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module26)
	init_entry(mercury__io__write_many_3_0);
	init_label(mercury__io__write_many_3_0_i2);
BEGIN_CODE

/* code for predicate 'io__write_many'/3 in mode 0 */
Define_entry(mercury__io__write_many_3_0);
	incr_sp_push_msg(2, "io__write_many");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
		call_localret(STATIC(mercury__io__output_stream_3_0),
		mercury__io__write_many_3_0_i2,
		ENTRY(mercury__io__write_many_3_0));
	}
Define_label(mercury__io__write_many_3_0_i2);
	update_prof_current_proc(LABEL(mercury__io__write_many_3_0));
	r3 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
		tailcall(STATIC(mercury__io__write_many_4_0),
		ENTRY(mercury__io__write_many_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__io_module27)
	init_entry(mercury__io__write_many_4_0);
	init_label(mercury__io__write_many_4_0_i7);
	init_label(mercury__io__write_many_4_0_i6);
	init_label(mercury__io__write_many_4_0_i9);
	init_label(mercury__io__write_many_4_0_i12);
	init_label(mercury__io__write_many_4_0_i1000);
BEGIN_CODE

/* code for predicate 'io__write_many'/4 in mode 0 */
Define_entry(mercury__io__write_many_4_0);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury__io__write_many_4_0_i1000);
	r4 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	incr_sp_push_msg(3, "io__write_many");
	detstackvar(3) = (Integer) succip;
	if ((tag((Integer) r4) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__io__write_many_4_0_i6);
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r2 = (Integer) field(mktag(0), (Integer) r4, ((Integer) 0));
	{
		call_localret(STATIC(mercury__io__write_float_4_0),
		mercury__io__write_many_4_0_i7,
		ENTRY(mercury__io__write_many_4_0));
	}
Define_label(mercury__io__write_many_4_0_i7);
	update_prof_current_proc(LABEL(mercury__io__write_many_4_0));
	r3 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	localtailcall(mercury__io__write_many_4_0,
		ENTRY(mercury__io__write_many_4_0));
Define_label(mercury__io__write_many_4_0_i6);
	if ((tag((Integer) r4) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__io__write_many_4_0_i9);
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r2 = (Integer) field(mktag(1), (Integer) r4, ((Integer) 0));
	{
		call_localret(STATIC(mercury__io__write_int_4_0),
		mercury__io__write_many_4_0_i7,
		ENTRY(mercury__io__write_many_4_0));
	}
Define_label(mercury__io__write_many_4_0_i9);
	if ((tag((Integer) r4) != mktag(((Integer) 2))))
		GOTO_LABEL(mercury__io__write_many_4_0_i12);
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r2 = (Integer) field(mktag(2), (Integer) r4, ((Integer) 0));
	{
		call_localret(STATIC(mercury__io__write_string_4_0),
		mercury__io__write_many_4_0_i7,
		ENTRY(mercury__io__write_many_4_0));
	}
Define_label(mercury__io__write_many_4_0_i12);
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) field(mktag(1), (Integer) r2, ((Integer) 1));
	r2 = (Integer) field(mktag(3), (Integer) r4, ((Integer) 0));
	{
		call_localret(STATIC(mercury__io__write_char_4_0),
		mercury__io__write_many_4_0_i7,
		ENTRY(mercury__io__write_many_4_0));
	}
Define_label(mercury__io__write_many_4_0_i1000);
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module28)
	init_entry(mercury__io__write_3_0);
BEGIN_CODE

/* code for predicate 'io__write'/3 in mode 0 */
Define_entry(mercury__io__write_3_0);
	{
		Word	TypeInfo_for_T;
		Word	Anything;
		Word	IO0;
		Word	IO;
		TypeInfo_for_T = (Integer) r1;
		Anything = (Integer) r2;
		IO0 = (Integer) r3;
		{
	mercury_print_type((Word *) TypeInfo_for_T, Anything);
	update_io(IO0, IO);
}
		r4 = IO;

	}
	r1 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module29)
	init_entry(mercury__io__write_4_0);
	init_label(mercury__io__write_4_0_i2);
	init_label(mercury__io__write_4_0_i3);
	init_label(mercury__io__write_4_0_i4);
BEGIN_CODE

/* code for predicate 'io__write'/4 in mode 0 */
Define_entry(mercury__io__write_4_0);
	incr_sp_push_msg(3, "io__write");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) r2;
	r2 = (Integer) r4;
	{
		call_localret(STATIC(mercury__io__set_output_stream_4_0),
		mercury__io__write_4_0_i2,
		ENTRY(mercury__io__write_4_0));
	}
Define_label(mercury__io__write_4_0_i2);
	update_prof_current_proc(LABEL(mercury__io__write_4_0));
	r3 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	{
		call_localret(STATIC(mercury__io__write_3_0),
		mercury__io__write_4_0_i3,
		ENTRY(mercury__io__write_4_0));
	}
Define_label(mercury__io__write_4_0_i3);
	update_prof_current_proc(LABEL(mercury__io__write_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	{
		call_localret(STATIC(mercury__io__set_output_stream_4_0),
		mercury__io__write_4_0_i4,
		ENTRY(mercury__io__write_4_0));
	}
Define_label(mercury__io__write_4_0_i4);
	update_prof_current_proc(LABEL(mercury__io__write_4_0));
	r1 = (Integer) r2;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module30)
	init_entry(mercury__io__flush_output_2_0);
BEGIN_CODE

/* code for predicate 'io__flush_output'/2 in mode 0 */
Define_entry(mercury__io__flush_output_2_0);
	{
		Word	IO0;
		Word	IO;
		IO0 = (Integer) r1;
		
	if (fflush(mercury_current_text_output->file) < 0) {
		mercury_output_error(mercury_current_text_output);
	}
	update_io(IO0, IO);

		r2 = IO;

	}
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module31)
	init_entry(mercury__io__flush_output_3_0);
BEGIN_CODE

/* code for predicate 'io__flush_output'/3 in mode 0 */
Define_entry(mercury__io__flush_output_3_0);
	{
		Word	Stream;
		Word	IO0;
		Word	IO;
		Stream = (Integer) r1;
		IO0 = (Integer) r2;
		{
	MercuryFile *stream = (MercuryFile *) Stream;
	if (fflush(stream->file) < 0) {
		mercury_output_error(stream);
	}
	update_io(IO0, IO);
}
		r3 = IO;

	}
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module32)
	init_entry(mercury__io__see_4_0);
	init_label(mercury__io__see_4_0_i2);
	init_label(mercury__io__see_4_0_i4);
	init_label(mercury__io__see_4_0_i5);
BEGIN_CODE

/* code for predicate 'io__see'/4 in mode 0 */
Define_entry(mercury__io__see_4_0);
	incr_sp_push_msg(1, "io__see");
	detstackvar(1) = (Integer) succip;
	{
		call_localret(STATIC(mercury__io__open_input_4_0),
		mercury__io__see_4_0_i2,
		ENTRY(mercury__io__see_4_0));
	}
Define_label(mercury__io__see_4_0_i2);
	update_prof_current_proc(LABEL(mercury__io__see_4_0));
	if ((tag((Integer) r1) == mktag(((Integer) 0))))
		GOTO_LABEL(mercury__io__see_4_0_i4);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
Define_label(mercury__io__see_4_0_i4);
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	{
		call_localret(STATIC(mercury__io__set_input_stream_4_0),
		mercury__io__see_4_0_i5,
		ENTRY(mercury__io__see_4_0));
	}
Define_label(mercury__io__see_4_0_i5);
	update_prof_current_proc(LABEL(mercury__io__see_4_0));
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module33)
	init_entry(mercury__io__seen_2_0);
	init_label(mercury__io__seen_2_0_i2);
	init_label(mercury__io__seen_2_0_i3);
BEGIN_CODE

/* code for predicate 'io__seen'/2 in mode 0 */
Define_entry(mercury__io__seen_2_0);
	incr_sp_push_msg(1, "io__seen");
	detstackvar(1) = (Integer) succip;
	{
		call_localret(STATIC(mercury__io__stdin_stream_3_0),
		mercury__io__seen_2_0_i2,
		ENTRY(mercury__io__seen_2_0));
	}
Define_label(mercury__io__seen_2_0_i2);
	update_prof_current_proc(LABEL(mercury__io__seen_2_0));
	{
		call_localret(STATIC(mercury__io__set_input_stream_4_0),
		mercury__io__seen_2_0_i3,
		ENTRY(mercury__io__seen_2_0));
	}
Define_label(mercury__io__seen_2_0_i3);
	update_prof_current_proc(LABEL(mercury__io__seen_2_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	{
		tailcall(STATIC(mercury__io__close_input_3_0),
		ENTRY(mercury__io__seen_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__io_module34)
	init_entry(mercury__io__open_input_4_0);
	init_label(mercury__io__open_input_4_0_i2);
	init_label(mercury__io__open_input_4_0_i8);
	init_label(mercury__io__open_input_4_0_i3);
BEGIN_CODE

/* code for predicate 'io__open_input'/4 in mode 0 */
Define_entry(mercury__io__open_input_4_0);
	incr_sp_push_msg(3, "io__open_input");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r3 = (Integer) r2;
	r2 = string_const("r", 1);
	call_localret(STATIC(mercury__io__do_open_6_0),
		mercury__io__open_input_4_0_i2,
		ENTRY(mercury__io__open_input_4_0));
Define_label(mercury__io__open_input_4_0_i2);
	update_prof_current_proc(LABEL(mercury__io__open_input_4_0));
	if (((Integer) r1 == ((Integer) -1)))
		GOTO_LABEL(mercury__io__open_input_4_0_i3);
	r1 = (Integer) r2;
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) tempr1;
	call_localret(STATIC(mercury__io__insert_stream_name_4_0),
		mercury__io__open_input_4_0_i8,
		ENTRY(mercury__io__open_input_4_0));
	}
Define_label(mercury__io__open_input_4_0_i8);
	update_prof_current_proc(LABEL(mercury__io__open_input_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__io__open_input_4_0_i3);
	r1 = (Integer) mkword(mktag(1), (Integer) mercury_data_io__common_2);
	r2 = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module35)
	init_entry(mercury__io__close_input_3_0);
BEGIN_CODE

/* code for predicate 'io__close_input'/3 in mode 0 */
Define_entry(mercury__io__close_input_3_0);
	{
		Word	Stream;
		Word	IO0;
		Word	IO;
		Stream = (Integer) r1;
		IO0 = (Integer) r2;
		
	mercury_close((MercuryFile*)Stream);
	update_io(IO0, IO);

		r3 = IO;

	}
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module36)
	init_entry(mercury__io__input_stream_3_0);
BEGIN_CODE

/* code for predicate 'io__input_stream'/3 in mode 0 */
Define_entry(mercury__io__input_stream_3_0);
	{
		Word	Stream;
		Word	IO0;
		Word	IO;
		IO0 = (Integer) r1;
		
	Stream = (Word) mercury_current_text_input;
	update_io(IO0, IO);

		r2 = Stream;
		r3 = IO;

	}
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module37)
	init_entry(mercury__io__set_input_stream_4_0);
BEGIN_CODE

/* code for predicate 'io__set_input_stream'/4 in mode 0 */
Define_entry(mercury__io__set_input_stream_4_0);
	{
		Word	NewStream;
		Word	OutStream;
		Word	IO0;
		Word	IO;
		NewStream = (Integer) r1;
		IO0 = (Integer) r2;
		
	OutStream = (Word) mercury_current_text_input;
	mercury_current_text_input = (MercuryFile*) NewStream;
	update_io(IO0, IO);

		r3 = OutStream;
		r4 = IO;

	}
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module38)
	init_entry(mercury__io__stdin_stream_3_0);
BEGIN_CODE

/* code for predicate 'io__stdin_stream'/3 in mode 0 */
Define_entry(mercury__io__stdin_stream_3_0);
	{
		Word	Stream;
		Word	IO0;
		Word	IO;
		IO0 = (Integer) r1;
		
	Stream = (Word) &mercury_stdin;
	update_io(IO0, IO);

		r2 = Stream;
		r3 = IO;

	}
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module39)
	init_entry(mercury__io__input_stream_name_3_0);
	init_label(mercury__io__input_stream_name_3_0_i2);
BEGIN_CODE

/* code for predicate 'io__input_stream_name'/3 in mode 0 */
Define_entry(mercury__io__input_stream_name_3_0);
	incr_sp_push_msg(1, "io__input_stream_name");
	detstackvar(1) = (Integer) succip;
	{
		call_localret(STATIC(mercury__io__input_stream_3_0),
		mercury__io__input_stream_name_3_0_i2,
		ENTRY(mercury__io__input_stream_name_3_0));
	}
Define_label(mercury__io__input_stream_name_3_0_i2);
	update_prof_current_proc(LABEL(mercury__io__input_stream_name_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	tailcall(STATIC(mercury__io__stream_name_4_0),
		ENTRY(mercury__io__input_stream_name_3_0));
END_MODULE

BEGIN_MODULE(mercury__io_module40)
	init_entry(mercury__io__input_stream_name_4_0);
BEGIN_CODE

/* code for predicate 'io__input_stream_name'/4 in mode 0 */
Define_entry(mercury__io__input_stream_name_4_0);
	tailcall(STATIC(mercury__io__stream_name_4_0),
		ENTRY(mercury__io__input_stream_name_4_0));
END_MODULE

BEGIN_MODULE(mercury__io_module41)
	init_entry(mercury__io__get_line_number_3_0);
BEGIN_CODE

/* code for predicate 'io__get_line_number'/3 in mode 0 */
Define_entry(mercury__io__get_line_number_3_0);
	{
		Integer	LineNum;
		Word	IO0;
		Word	IO;
		IO0 = (Integer) r1;
		
	LineNum = mercury_current_text_input->line_number;
	update_io(IO0, IO);

		r2 = LineNum;
		r3 = IO;

	}
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module42)
	init_entry(mercury__io__get_line_number_4_0);
BEGIN_CODE

/* code for predicate 'io__get_line_number'/4 in mode 0 */
Define_entry(mercury__io__get_line_number_4_0);
	{
		Word	Stream;
		Integer	LineNum;
		Word	IO0;
		Word	IO;
		Stream = (Integer) r1;
		IO0 = (Integer) r2;
		{
	MercuryFile *stream = (MercuryFile *) Stream;
	LineNum = stream->line_number;
	update_io(IO0, IO);
}
		r3 = LineNum;
		r4 = IO;

	}
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module43)
	init_entry(mercury__io__set_line_number_3_0);
BEGIN_CODE

/* code for predicate 'io__set_line_number'/3 in mode 0 */
Define_entry(mercury__io__set_line_number_3_0);
	{
		Integer	LineNum;
		Word	IO0;
		Word	IO;
		LineNum = (Integer) r1;
		IO0 = (Integer) r2;
		
	mercury_current_text_input->line_number = LineNum;
	update_io(IO0, IO);

		r3 = IO;

	}
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module44)
	init_entry(mercury__io__set_line_number_4_0);
BEGIN_CODE

/* code for predicate 'io__set_line_number'/4 in mode 0 */
Define_entry(mercury__io__set_line_number_4_0);
	{
		Word	Stream;
		Integer	LineNum;
		Word	IO0;
		Word	IO;
		Stream = (Integer) r1;
		LineNum = (Integer) r2;
		IO0 = (Integer) r3;
		{
	MercuryFile *stream = (MercuryFile *) Stream;
	stream->line_number = LineNum;
	update_io(IO0, IO);
}
		r4 = IO;

	}
	r1 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module45)
	init_entry(mercury__io__tell_4_0);
	init_label(mercury__io__tell_4_0_i2);
	init_label(mercury__io__tell_4_0_i6);
	init_label(mercury__io__tell_4_0_i3);
BEGIN_CODE

/* code for predicate 'io__tell'/4 in mode 0 */
Define_entry(mercury__io__tell_4_0);
	incr_sp_push_msg(1, "io__tell");
	detstackvar(1) = (Integer) succip;
	{
		call_localret(STATIC(mercury__io__open_output_4_0),
		mercury__io__tell_4_0_i2,
		ENTRY(mercury__io__tell_4_0));
	}
Define_label(mercury__io__tell_4_0_i2);
	update_prof_current_proc(LABEL(mercury__io__tell_4_0));
	if ((tag((Integer) r1) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__io__tell_4_0_i3);
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	{
		call_localret(STATIC(mercury__io__set_output_stream_4_0),
		mercury__io__tell_4_0_i6,
		ENTRY(mercury__io__tell_4_0));
	}
Define_label(mercury__io__tell_4_0_i6);
	update_prof_current_proc(LABEL(mercury__io__tell_4_0));
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
Define_label(mercury__io__tell_4_0_i3);
	r1 = (Integer) mkword(mktag(1), (Integer) mercury_data_io__common_3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module46)
	init_entry(mercury__io__told_2_0);
	init_label(mercury__io__told_2_0_i2);
	init_label(mercury__io__told_2_0_i3);
BEGIN_CODE

/* code for predicate 'io__told'/2 in mode 0 */
Define_entry(mercury__io__told_2_0);
	incr_sp_push_msg(1, "io__told");
	detstackvar(1) = (Integer) succip;
	{
		call_localret(STATIC(mercury__io__stdout_stream_3_0),
		mercury__io__told_2_0_i2,
		ENTRY(mercury__io__told_2_0));
	}
Define_label(mercury__io__told_2_0_i2);
	update_prof_current_proc(LABEL(mercury__io__told_2_0));
	{
		call_localret(STATIC(mercury__io__set_output_stream_4_0),
		mercury__io__told_2_0_i3,
		ENTRY(mercury__io__told_2_0));
	}
Define_label(mercury__io__told_2_0_i3);
	update_prof_current_proc(LABEL(mercury__io__told_2_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	{
		tailcall(STATIC(mercury__io__close_output_3_0),
		ENTRY(mercury__io__told_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__io_module47)
	init_entry(mercury__io__open_output_4_0);
	init_label(mercury__io__open_output_4_0_i2);
	init_label(mercury__io__open_output_4_0_i8);
	init_label(mercury__io__open_output_4_0_i3);
BEGIN_CODE

/* code for predicate 'io__open_output'/4 in mode 0 */
Define_entry(mercury__io__open_output_4_0);
	incr_sp_push_msg(3, "io__open_output");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r3 = (Integer) r2;
	r2 = string_const("w", 1);
	call_localret(STATIC(mercury__io__do_open_6_0),
		mercury__io__open_output_4_0_i2,
		ENTRY(mercury__io__open_output_4_0));
Define_label(mercury__io__open_output_4_0_i2);
	update_prof_current_proc(LABEL(mercury__io__open_output_4_0));
	if (((Integer) r1 == ((Integer) -1)))
		GOTO_LABEL(mercury__io__open_output_4_0_i3);
	r1 = (Integer) r2;
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) tempr1;
	call_localret(STATIC(mercury__io__insert_stream_name_4_0),
		mercury__io__open_output_4_0_i8,
		ENTRY(mercury__io__open_output_4_0));
	}
Define_label(mercury__io__open_output_4_0_i8);
	update_prof_current_proc(LABEL(mercury__io__open_output_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__io__open_output_4_0_i3);
	r1 = (Integer) mkword(mktag(1), (Integer) mercury_data_io__common_3);
	r2 = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module48)
	init_entry(mercury__io__open_append_4_0);
	init_label(mercury__io__open_append_4_0_i2);
	init_label(mercury__io__open_append_4_0_i8);
	init_label(mercury__io__open_append_4_0_i3);
BEGIN_CODE

/* code for predicate 'io__open_append'/4 in mode 0 */
Define_entry(mercury__io__open_append_4_0);
	incr_sp_push_msg(3, "io__open_append");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r3 = (Integer) r2;
	r2 = string_const("a", 1);
	call_localret(STATIC(mercury__io__do_open_6_0),
		mercury__io__open_append_4_0_i2,
		ENTRY(mercury__io__open_append_4_0));
Define_label(mercury__io__open_append_4_0_i2);
	update_prof_current_proc(LABEL(mercury__io__open_append_4_0));
	if (((Integer) r1 == ((Integer) -1)))
		GOTO_LABEL(mercury__io__open_append_4_0_i3);
	r1 = (Integer) r2;
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) tempr1;
	call_localret(STATIC(mercury__io__insert_stream_name_4_0),
		mercury__io__open_append_4_0_i8,
		ENTRY(mercury__io__open_append_4_0));
	}
Define_label(mercury__io__open_append_4_0_i8);
	update_prof_current_proc(LABEL(mercury__io__open_append_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__io__open_append_4_0_i3);
	r1 = (Integer) mkword(mktag(1), (Integer) mercury_data_io__common_4);
	r2 = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module49)
	init_entry(mercury__io__close_output_3_0);
BEGIN_CODE

/* code for predicate 'io__close_output'/3 in mode 0 */
Define_entry(mercury__io__close_output_3_0);
	{
		Word	Stream;
		Word	IO0;
		Word	IO;
		Stream = (Integer) r1;
		IO0 = (Integer) r2;
		
	mercury_close((MercuryFile*)Stream);
	update_io(IO0, IO);

		r3 = IO;

	}
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module50)
	init_entry(mercury__io__output_stream_3_0);
BEGIN_CODE

/* code for predicate 'io__output_stream'/3 in mode 0 */
Define_entry(mercury__io__output_stream_3_0);
	{
		Word	Stream;
		Word	IO0;
		Word	IO;
		IO0 = (Integer) r1;
		
	Stream = (Word) mercury_current_text_output;
	update_io(IO0, IO);

		r2 = Stream;
		r3 = IO;

	}
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module51)
	init_entry(mercury__io__set_output_stream_4_0);
BEGIN_CODE

/* code for predicate 'io__set_output_stream'/4 in mode 0 */
Define_entry(mercury__io__set_output_stream_4_0);
	{
		Word	NewStream;
		Word	OutStream;
		Word	IO0;
		Word	IO;
		NewStream = (Integer) r1;
		IO0 = (Integer) r2;
		
	OutStream = (Word) mercury_current_text_output;
	mercury_current_text_output = (MercuryFile*) NewStream;
	update_io(IO0, IO);

		r3 = OutStream;
		r4 = IO;

	}
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module52)
	init_entry(mercury__io__stdout_stream_3_0);
BEGIN_CODE

/* code for predicate 'io__stdout_stream'/3 in mode 0 */
Define_entry(mercury__io__stdout_stream_3_0);
	{
		Word	Stream;
		Word	IO0;
		Word	IO;
		IO0 = (Integer) r1;
		
	Stream = (Word) &mercury_stdout;
	update_io(IO0, IO);

		r2 = Stream;
		r3 = IO;

	}
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module53)
	init_entry(mercury__io__stderr_stream_3_0);
BEGIN_CODE

/* code for predicate 'io__stderr_stream'/3 in mode 0 */
Define_entry(mercury__io__stderr_stream_3_0);
	{
		Word	Stream;
		Word	IO0;
		Word	IO;
		IO0 = (Integer) r1;
		
	Stream = (Word) &mercury_stderr;
	update_io(IO0, IO);

		r2 = Stream;
		r3 = IO;

	}
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module54)
	init_entry(mercury__io__output_stream_name_3_0);
	init_label(mercury__io__output_stream_name_3_0_i2);
BEGIN_CODE

/* code for predicate 'io__output_stream_name'/3 in mode 0 */
Define_entry(mercury__io__output_stream_name_3_0);
	incr_sp_push_msg(1, "io__output_stream_name");
	detstackvar(1) = (Integer) succip;
	{
		call_localret(STATIC(mercury__io__output_stream_3_0),
		mercury__io__output_stream_name_3_0_i2,
		ENTRY(mercury__io__output_stream_name_3_0));
	}
Define_label(mercury__io__output_stream_name_3_0_i2);
	update_prof_current_proc(LABEL(mercury__io__output_stream_name_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	tailcall(STATIC(mercury__io__stream_name_4_0),
		ENTRY(mercury__io__output_stream_name_3_0));
END_MODULE

BEGIN_MODULE(mercury__io_module55)
	init_entry(mercury__io__output_stream_name_4_0);
BEGIN_CODE

/* code for predicate 'io__output_stream_name'/4 in mode 0 */
Define_entry(mercury__io__output_stream_name_4_0);
	tailcall(STATIC(mercury__io__stream_name_4_0),
		ENTRY(mercury__io__output_stream_name_4_0));
END_MODULE

BEGIN_MODULE(mercury__io_module56)
	init_entry(mercury__io__read_byte_3_0);
	init_label(mercury__io__read_byte_3_0_i2);
BEGIN_CODE

/* code for predicate 'io__read_byte'/3 in mode 0 */
Define_entry(mercury__io__read_byte_3_0);
	incr_sp_push_msg(1, "io__read_byte");
	detstackvar(1) = (Integer) succip;
	{
		call_localret(STATIC(mercury__io__binary_input_stream_3_0),
		mercury__io__read_byte_3_0_i2,
		ENTRY(mercury__io__read_byte_3_0));
	}
Define_label(mercury__io__read_byte_3_0_i2);
	update_prof_current_proc(LABEL(mercury__io__read_byte_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	{
		tailcall(STATIC(mercury__io__read_byte_4_0),
		ENTRY(mercury__io__read_byte_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__io_module57)
	init_entry(mercury__io__read_byte_4_0);
	init_label(mercury__io__read_byte_4_0_i2);
	init_label(mercury__io__read_byte_4_0_i3);
	init_label(mercury__io__read_byte_4_0_i6);
BEGIN_CODE

/* code for predicate 'io__read_byte'/4 in mode 0 */
Define_entry(mercury__io__read_byte_4_0);
	incr_sp_push_msg(1, "io__read_byte");
	detstackvar(1) = (Integer) succip;
	call_localret(STATIC(mercury__io__read_char_code_4_0),
		mercury__io__read_byte_4_0_i2,
		ENTRY(mercury__io__read_byte_4_0));
Define_label(mercury__io__read_byte_4_0_i2);
	update_prof_current_proc(LABEL(mercury__io__read_byte_4_0));
	if (((Integer) r1 != ((Integer) -1)))
		GOTO_LABEL(mercury__io__read_byte_4_0_i3);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
Define_label(mercury__io__read_byte_4_0_i3);
	if (((Integer) r1 != ((Integer) -2)))
		GOTO_LABEL(mercury__io__read_byte_4_0_i6);
	r1 = (Integer) mkword(mktag(2), (Integer) mercury_data_io__common_0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
Define_label(mercury__io__read_byte_4_0_i6);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module58)
	init_entry(mercury__io__putback_byte_3_0);
	init_label(mercury__io__putback_byte_3_0_i2);
BEGIN_CODE

/* code for predicate 'io__putback_byte'/3 in mode 0 */
Define_entry(mercury__io__putback_byte_3_0);
	incr_sp_push_msg(2, "io__putback_byte");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) r2;
	{
		call_localret(STATIC(mercury__io__binary_input_stream_3_0),
		mercury__io__putback_byte_3_0_i2,
		ENTRY(mercury__io__putback_byte_3_0));
	}
Define_label(mercury__io__putback_byte_3_0_i2);
	update_prof_current_proc(LABEL(mercury__io__putback_byte_3_0));
	r3 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	{
		tailcall(STATIC(mercury__io__putback_byte_4_0),
		ENTRY(mercury__io__putback_byte_3_0));
	}
END_MODULE

BEGIN_MODULE(mercury__io_module59)
	init_entry(mercury__io__putback_byte_4_0);
BEGIN_CODE

/* code for predicate 'io__putback_byte'/4 in mode 0 */
Define_entry(mercury__io__putback_byte_4_0);
	{
		Word	File;
		Integer	Character;
		Word	IO0;
		Word	IO;
		File = (Integer) r1;
		Character = (Integer) r2;
		IO0 = (Integer) r3;
		{
	MercuryFile* mf = (MercuryFile *)File;
	/* XXX should work even if ungetc() fails */
	if (ungetc(Character, mf->file) == EOF) {
		fatal_error("io__putback_byte: ungetc failed");
	}
	update_io(IO0, IO);
}
		r4 = IO;

	}
	r1 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module60)
	init_entry(mercury__io__write_byte_3_0);
BEGIN_CODE

/* code for predicate 'io__write_byte'/3 in mode 0 */
Define_entry(mercury__io__write_byte_3_0);
	{
		Integer	Byte;
		Word	IO0;
		Word	IO;
		Byte = (Integer) r1;
		IO0 = (Integer) r2;
		
	if (putc(Byte, mercury_current_binary_output->file) < 0) {
		mercury_output_error(mercury_current_text_output);
	}
	update_io(IO0, IO);

		r3 = IO;

	}
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module61)
	init_entry(mercury__io__write_byte_4_0);
BEGIN_CODE

/* code for predicate 'io__write_byte'/4 in mode 0 */
Define_entry(mercury__io__write_byte_4_0);
	{
		Word	Stream;
		Integer	Byte;
		Word	IO0;
		Word	IO;
		Stream = (Integer) r1;
		Byte = (Integer) r2;
		IO0 = (Integer) r3;
		{
	MercuryFile *stream = (MercuryFile *) Stream;
	if (putc(Byte, stream->file) < 0) {
		mercury_output_error(stream);
	}
	update_io(IO0, IO);
}
		r4 = IO;

	}
	r1 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module62)
	init_entry(mercury__io__write_bytes_3_0);
BEGIN_CODE

/* code for predicate 'io__write_bytes'/3 in mode 0 */
Define_entry(mercury__io__write_bytes_3_0);
	{
		String	Message;
		Word	IO0;
		Word	IO;
		Message = (String) (Integer) r1;
		IO0 = (Integer) r2;
		{
	mercury_print_binary_string(mercury_current_binary_output, Message);
	update_io(IO0, IO);
}
		r3 = IO;

	}
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module63)
	init_entry(mercury__io__write_bytes_4_0);
BEGIN_CODE

/* code for predicate 'io__write_bytes'/4 in mode 0 */
Define_entry(mercury__io__write_bytes_4_0);
	{
		Word	Stream;
		String	Message;
		Word	IO0;
		Word	IO;
		Stream = (Integer) r1;
		Message = (String) (Integer) r2;
		IO0 = (Integer) r3;
		{
	MercuryFile *stream = (MercuryFile *) Stream;
	mercury_print_binary_string(stream, Message);
	update_io(IO0, IO);
}
		r4 = IO;

	}
	r1 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module64)
	init_entry(mercury__io__flush_binary_output_2_0);
BEGIN_CODE

/* code for predicate 'io__flush_binary_output'/2 in mode 0 */
Define_entry(mercury__io__flush_binary_output_2_0);
	{
		Word	IO0;
		Word	IO;
		IO0 = (Integer) r1;
		
	if (fflush(mercury_current_binary_output->file) < 0) {
		mercury_output_error(mercury_current_binary_output);
	}
	update_io(IO0, IO);

		r2 = IO;

	}
	r1 = (Integer) r2;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module65)
	init_entry(mercury__io__flush_binary_output_3_0);
BEGIN_CODE

/* code for predicate 'io__flush_binary_output'/3 in mode 0 */
Define_entry(mercury__io__flush_binary_output_3_0);
	{
		Word	Stream;
		Word	IO0;
		Word	IO;
		Stream = (Integer) r1;
		IO0 = (Integer) r2;
		{
	MercuryFile *stream = (MercuryFile *) Stream;
	if (fflush(stream->file) < 0) {
		mercury_output_error(stream);
	}
	update_io(IO0, IO);
}
		r3 = IO;

	}
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module66)
	init_entry(mercury__io__see_binary_4_0);
	init_label(mercury__io__see_binary_4_0_i2);
	init_label(mercury__io__see_binary_4_0_i4);
	init_label(mercury__io__see_binary_4_0_i5);
BEGIN_CODE

/* code for predicate 'io__see_binary'/4 in mode 0 */
Define_entry(mercury__io__see_binary_4_0);
	incr_sp_push_msg(1, "io__see_binary");
	detstackvar(1) = (Integer) succip;
	{
		call_localret(STATIC(mercury__io__open_binary_input_4_0),
		mercury__io__see_binary_4_0_i2,
		ENTRY(mercury__io__see_binary_4_0));
	}
Define_label(mercury__io__see_binary_4_0_i2);
	update_prof_current_proc(LABEL(mercury__io__see_binary_4_0));
	if ((tag((Integer) r1) == mktag(((Integer) 0))))
		GOTO_LABEL(mercury__io__see_binary_4_0_i4);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
Define_label(mercury__io__see_binary_4_0_i4);
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	{
		call_localret(STATIC(mercury__io__set_binary_input_stream_4_0),
		mercury__io__see_binary_4_0_i5,
		ENTRY(mercury__io__see_binary_4_0));
	}
Define_label(mercury__io__see_binary_4_0_i5);
	update_prof_current_proc(LABEL(mercury__io__see_binary_4_0));
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module67)
	init_entry(mercury__io__seen_binary_2_0);
	init_label(mercury__io__seen_binary_2_0_i2);
	init_label(mercury__io__seen_binary_2_0_i3);
BEGIN_CODE

/* code for predicate 'io__seen_binary'/2 in mode 0 */
Define_entry(mercury__io__seen_binary_2_0);
	incr_sp_push_msg(1, "io__seen_binary");
	detstackvar(1) = (Integer) succip;
	{
		call_localret(STATIC(mercury__io__stdin_binary_stream_3_0),
		mercury__io__seen_binary_2_0_i2,
		ENTRY(mercury__io__seen_binary_2_0));
	}
Define_label(mercury__io__seen_binary_2_0_i2);
	update_prof_current_proc(LABEL(mercury__io__seen_binary_2_0));
	{
		call_localret(STATIC(mercury__io__set_binary_input_stream_4_0),
		mercury__io__seen_binary_2_0_i3,
		ENTRY(mercury__io__seen_binary_2_0));
	}
Define_label(mercury__io__seen_binary_2_0_i3);
	update_prof_current_proc(LABEL(mercury__io__seen_binary_2_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	{
		tailcall(STATIC(mercury__io__close_binary_input_3_0),
		ENTRY(mercury__io__seen_binary_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__io_module68)
	init_entry(mercury__io__open_binary_input_4_0);
	init_label(mercury__io__open_binary_input_4_0_i2);
	init_label(mercury__io__open_binary_input_4_0_i8);
	init_label(mercury__io__open_binary_input_4_0_i3);
BEGIN_CODE

/* code for predicate 'io__open_binary_input'/4 in mode 0 */
Define_entry(mercury__io__open_binary_input_4_0);
	incr_sp_push_msg(3, "io__open_binary_input");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r3 = (Integer) r2;
	r2 = string_const("rb", 2);
	call_localret(STATIC(mercury__io__do_open_6_0),
		mercury__io__open_binary_input_4_0_i2,
		ENTRY(mercury__io__open_binary_input_4_0));
Define_label(mercury__io__open_binary_input_4_0_i2);
	update_prof_current_proc(LABEL(mercury__io__open_binary_input_4_0));
	if (((Integer) r1 == ((Integer) -1)))
		GOTO_LABEL(mercury__io__open_binary_input_4_0_i3);
	r1 = (Integer) r2;
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) tempr1;
	call_localret(STATIC(mercury__io__insert_stream_name_4_0),
		mercury__io__open_binary_input_4_0_i8,
		ENTRY(mercury__io__open_binary_input_4_0));
	}
Define_label(mercury__io__open_binary_input_4_0_i8);
	update_prof_current_proc(LABEL(mercury__io__open_binary_input_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__io__open_binary_input_4_0_i3);
	r1 = (Integer) mkword(mktag(1), (Integer) mercury_data_io__common_2);
	r2 = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module69)
	init_entry(mercury__io__close_binary_input_3_0);
BEGIN_CODE

/* code for predicate 'io__close_binary_input'/3 in mode 0 */
Define_entry(mercury__io__close_binary_input_3_0);
	{
		Word	Stream;
		Word	IO0;
		Word	IO;
		Stream = (Integer) r1;
		IO0 = (Integer) r2;
		
	mercury_close((MercuryFile*)Stream);
	update_io(IO0, IO);

		r3 = IO;

	}
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module70)
	init_entry(mercury__io__binary_input_stream_3_0);
BEGIN_CODE

/* code for predicate 'io__binary_input_stream'/3 in mode 0 */
Define_entry(mercury__io__binary_input_stream_3_0);
	{
		Word	Stream;
		Word	IO0;
		Word	IO;
		IO0 = (Integer) r1;
		
	Stream = (Word) mercury_current_binary_input;
	update_io(IO0, IO);

		r2 = Stream;
		r3 = IO;

	}
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module71)
	init_entry(mercury__io__set_binary_input_stream_4_0);
BEGIN_CODE

/* code for predicate 'io__set_binary_input_stream'/4 in mode 0 */
Define_entry(mercury__io__set_binary_input_stream_4_0);
	{
		Word	NewStream;
		Word	OutStream;
		Word	IO0;
		Word	IO;
		NewStream = (Integer) r1;
		IO0 = (Integer) r2;
		
	OutStream = (Word) mercury_current_binary_input;
	mercury_current_binary_input = (MercuryFile*) NewStream;
	update_io(IO0, IO);

		r3 = OutStream;
		r4 = IO;

	}
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module72)
	init_entry(mercury__io__stdin_binary_stream_3_0);
BEGIN_CODE

/* code for predicate 'io__stdin_binary_stream'/3 in mode 0 */
Define_entry(mercury__io__stdin_binary_stream_3_0);
	{
		Word	Stream;
		Word	IO0;
		Word	IO;
		IO0 = (Integer) r1;
		
	Stream = (Word) &mercury_stdin;
	update_io(IO0, IO);

		r2 = Stream;
		r3 = IO;

	}
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module73)
	init_entry(mercury__io__binary_input_stream_name_3_0);
	init_label(mercury__io__binary_input_stream_name_3_0_i2);
BEGIN_CODE

/* code for predicate 'io__binary_input_stream_name'/3 in mode 0 */
Define_entry(mercury__io__binary_input_stream_name_3_0);
	incr_sp_push_msg(1, "io__binary_input_stream_name");
	detstackvar(1) = (Integer) succip;
	{
		call_localret(STATIC(mercury__io__binary_input_stream_3_0),
		mercury__io__binary_input_stream_name_3_0_i2,
		ENTRY(mercury__io__binary_input_stream_name_3_0));
	}
Define_label(mercury__io__binary_input_stream_name_3_0_i2);
	update_prof_current_proc(LABEL(mercury__io__binary_input_stream_name_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	tailcall(STATIC(mercury__io__stream_name_4_0),
		ENTRY(mercury__io__binary_input_stream_name_3_0));
END_MODULE

BEGIN_MODULE(mercury__io_module74)
	init_entry(mercury__io__binary_input_stream_name_4_0);
BEGIN_CODE

/* code for predicate 'io__binary_input_stream_name'/4 in mode 0 */
Define_entry(mercury__io__binary_input_stream_name_4_0);
	tailcall(STATIC(mercury__io__stream_name_4_0),
		ENTRY(mercury__io__binary_input_stream_name_4_0));
END_MODULE

BEGIN_MODULE(mercury__io_module75)
	init_entry(mercury__io__tell_binary_4_0);
	init_label(mercury__io__tell_binary_4_0_i2);
	init_label(mercury__io__tell_binary_4_0_i6);
	init_label(mercury__io__tell_binary_4_0_i3);
BEGIN_CODE

/* code for predicate 'io__tell_binary'/4 in mode 0 */
Define_entry(mercury__io__tell_binary_4_0);
	incr_sp_push_msg(1, "io__tell_binary");
	detstackvar(1) = (Integer) succip;
	{
		call_localret(STATIC(mercury__io__open_binary_output_4_0),
		mercury__io__tell_binary_4_0_i2,
		ENTRY(mercury__io__tell_binary_4_0));
	}
Define_label(mercury__io__tell_binary_4_0_i2);
	update_prof_current_proc(LABEL(mercury__io__tell_binary_4_0));
	if ((tag((Integer) r1) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__io__tell_binary_4_0_i3);
	r1 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	{
		call_localret(STATIC(mercury__io__set_binary_output_stream_4_0),
		mercury__io__tell_binary_4_0_i6,
		ENTRY(mercury__io__tell_binary_4_0));
	}
Define_label(mercury__io__tell_binary_4_0_i6);
	update_prof_current_proc(LABEL(mercury__io__tell_binary_4_0));
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
Define_label(mercury__io__tell_binary_4_0_i3);
	r1 = (Integer) mkword(mktag(1), (Integer) mercury_data_io__common_3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module76)
	init_entry(mercury__io__told_binary_2_0);
	init_label(mercury__io__told_binary_2_0_i2);
	init_label(mercury__io__told_binary_2_0_i3);
BEGIN_CODE

/* code for predicate 'io__told_binary'/2 in mode 0 */
Define_entry(mercury__io__told_binary_2_0);
	incr_sp_push_msg(1, "io__told_binary");
	detstackvar(1) = (Integer) succip;
	{
		call_localret(STATIC(mercury__io__stdout_binary_stream_3_0),
		mercury__io__told_binary_2_0_i2,
		ENTRY(mercury__io__told_binary_2_0));
	}
Define_label(mercury__io__told_binary_2_0_i2);
	update_prof_current_proc(LABEL(mercury__io__told_binary_2_0));
	{
		call_localret(STATIC(mercury__io__set_binary_output_stream_4_0),
		mercury__io__told_binary_2_0_i3,
		ENTRY(mercury__io__told_binary_2_0));
	}
Define_label(mercury__io__told_binary_2_0_i3);
	update_prof_current_proc(LABEL(mercury__io__told_binary_2_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	{
		tailcall(STATIC(mercury__io__close_binary_output_3_0),
		ENTRY(mercury__io__told_binary_2_0));
	}
END_MODULE

BEGIN_MODULE(mercury__io_module77)
	init_entry(mercury__io__open_binary_output_4_0);
	init_label(mercury__io__open_binary_output_4_0_i2);
	init_label(mercury__io__open_binary_output_4_0_i8);
	init_label(mercury__io__open_binary_output_4_0_i3);
BEGIN_CODE

/* code for predicate 'io__open_binary_output'/4 in mode 0 */
Define_entry(mercury__io__open_binary_output_4_0);
	incr_sp_push_msg(3, "io__open_binary_output");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r3 = (Integer) r2;
	r2 = string_const("wb", 2);
	call_localret(STATIC(mercury__io__do_open_6_0),
		mercury__io__open_binary_output_4_0_i2,
		ENTRY(mercury__io__open_binary_output_4_0));
Define_label(mercury__io__open_binary_output_4_0_i2);
	update_prof_current_proc(LABEL(mercury__io__open_binary_output_4_0));
	if (((Integer) r1 == ((Integer) -1)))
		GOTO_LABEL(mercury__io__open_binary_output_4_0_i3);
	r1 = (Integer) r2;
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) tempr1;
	call_localret(STATIC(mercury__io__insert_stream_name_4_0),
		mercury__io__open_binary_output_4_0_i8,
		ENTRY(mercury__io__open_binary_output_4_0));
	}
Define_label(mercury__io__open_binary_output_4_0_i8);
	update_prof_current_proc(LABEL(mercury__io__open_binary_output_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__io__open_binary_output_4_0_i3);
	r1 = (Integer) mkword(mktag(1), (Integer) mercury_data_io__common_3);
	r2 = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module78)
	init_entry(mercury__io__open_binary_append_4_0);
	init_label(mercury__io__open_binary_append_4_0_i2);
	init_label(mercury__io__open_binary_append_4_0_i8);
	init_label(mercury__io__open_binary_append_4_0_i3);
BEGIN_CODE

/* code for predicate 'io__open_binary_append'/4 in mode 0 */
Define_entry(mercury__io__open_binary_append_4_0);
	incr_sp_push_msg(3, "io__open_binary_append");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r3 = (Integer) r2;
	r2 = string_const("ab", 2);
	call_localret(STATIC(mercury__io__do_open_6_0),
		mercury__io__open_binary_append_4_0_i2,
		ENTRY(mercury__io__open_binary_append_4_0));
Define_label(mercury__io__open_binary_append_4_0_i2);
	update_prof_current_proc(LABEL(mercury__io__open_binary_append_4_0));
	if (((Integer) r1 == ((Integer) -1)))
		GOTO_LABEL(mercury__io__open_binary_append_4_0_i3);
	r1 = (Integer) r2;
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) tempr1;
	call_localret(STATIC(mercury__io__insert_stream_name_4_0),
		mercury__io__open_binary_append_4_0_i8,
		ENTRY(mercury__io__open_binary_append_4_0));
	}
Define_label(mercury__io__open_binary_append_4_0_i8);
	update_prof_current_proc(LABEL(mercury__io__open_binary_append_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__io__open_binary_append_4_0_i3);
	r1 = (Integer) mkword(mktag(1), (Integer) mercury_data_io__common_4);
	r2 = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module79)
	init_entry(mercury__io__close_binary_output_3_0);
BEGIN_CODE

/* code for predicate 'io__close_binary_output'/3 in mode 0 */
Define_entry(mercury__io__close_binary_output_3_0);
	{
		Word	Stream;
		Word	IO0;
		Word	IO;
		Stream = (Integer) r1;
		IO0 = (Integer) r2;
		
	mercury_close((MercuryFile*)Stream);
	update_io(IO0, IO);

		r3 = IO;

	}
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module80)
	init_entry(mercury__io__binary_output_stream_3_0);
BEGIN_CODE

/* code for predicate 'io__binary_output_stream'/3 in mode 0 */
Define_entry(mercury__io__binary_output_stream_3_0);
	{
		Word	Stream;
		Word	IO0;
		Word	IO;
		IO0 = (Integer) r1;
		
	Stream = (Word) mercury_current_binary_output;
	update_io(IO0, IO);

		r2 = Stream;
		r3 = IO;

	}
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module81)
	init_entry(mercury__io__stdout_binary_stream_3_0);
BEGIN_CODE

/* code for predicate 'io__stdout_binary_stream'/3 in mode 0 */
Define_entry(mercury__io__stdout_binary_stream_3_0);
	{
		Word	Stream;
		Word	IO0;
		Word	IO;
		IO0 = (Integer) r1;
		
	Stream = (Word) &mercury_stdout;
	update_io(IO0, IO);

		r2 = Stream;
		r3 = IO;

	}
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module82)
	init_entry(mercury__io__set_binary_output_stream_4_0);
BEGIN_CODE

/* code for predicate 'io__set_binary_output_stream'/4 in mode 0 */
Define_entry(mercury__io__set_binary_output_stream_4_0);
	{
		Word	NewStream;
		Word	OutStream;
		Word	IO0;
		Word	IO;
		NewStream = (Integer) r1;
		IO0 = (Integer) r2;
		
	OutStream = (Word) mercury_current_binary_output;
	mercury_current_binary_output = (MercuryFile*) NewStream;
	update_io(IO0, IO);

		r3 = OutStream;
		r4 = IO;

	}
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module83)
	init_entry(mercury__io__binary_output_stream_name_3_0);
	init_label(mercury__io__binary_output_stream_name_3_0_i2);
BEGIN_CODE

/* code for predicate 'io__binary_output_stream_name'/3 in mode 0 */
Define_entry(mercury__io__binary_output_stream_name_3_0);
	incr_sp_push_msg(1, "io__binary_output_stream_name");
	detstackvar(1) = (Integer) succip;
	{
		call_localret(STATIC(mercury__io__binary_output_stream_3_0),
		mercury__io__binary_output_stream_name_3_0_i2,
		ENTRY(mercury__io__binary_output_stream_name_3_0));
	}
Define_label(mercury__io__binary_output_stream_name_3_0_i2);
	update_prof_current_proc(LABEL(mercury__io__binary_output_stream_name_3_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	tailcall(STATIC(mercury__io__stream_name_4_0),
		ENTRY(mercury__io__binary_output_stream_name_3_0));
END_MODULE

BEGIN_MODULE(mercury__io_module84)
	init_entry(mercury__io__binary_output_stream_name_4_0);
BEGIN_CODE

/* code for predicate 'io__binary_output_stream_name'/4 in mode 0 */
Define_entry(mercury__io__binary_output_stream_name_4_0);
	tailcall(STATIC(mercury__io__stream_name_4_0),
		ENTRY(mercury__io__binary_output_stream_name_4_0));
END_MODULE

BEGIN_MODULE(mercury__io_module85)
	init_entry(mercury__io__progname_4_0);
BEGIN_CODE

/* code for predicate 'io__progname'/4 in mode 0 */
Define_entry(mercury__io__progname_4_0);
	{
		String	DefaultProgname;
		String	Progname;
		Word	IO0;
		Word	IO;
		DefaultProgname = (String) (Integer) r1;
		IO0 = (Integer) r2;
		
	/*
	** XXX need to guarantee alignment of strings
	** (in this case, the string `progname')
	*/
	Progname = (progname ? progname : DefaultProgname);
	update_io(IO0, IO);

		r3 = (Word) Progname;
		r4 = IO;

	}
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module86)
	init_entry(mercury__io__progname_base_4_0);
	init_label(mercury__io__progname_base_4_0_i2);
	init_label(mercury__io__progname_base_4_0_i3);
BEGIN_CODE

/* code for predicate 'io__progname_base'/4 in mode 0 */
Define_entry(mercury__io__progname_base_4_0);
	incr_sp_push_msg(2, "io__progname_base");
	detstackvar(2) = (Integer) succip;
	{
		call_localret(STATIC(mercury__io__progname_4_0),
		mercury__io__progname_base_4_0_i2,
		ENTRY(mercury__io__progname_base_4_0));
	}
Define_label(mercury__io__progname_base_4_0_i2);
	update_prof_current_proc(LABEL(mercury__io__progname_base_4_0));
	detstackvar(1) = (Integer) r2;
	{
	Declare_entry(mercury__dir__basename_2_0);
	call_localret(ENTRY(mercury__dir__basename_2_0),
		mercury__io__progname_base_4_0_i3,
		ENTRY(mercury__io__progname_base_4_0));
	}
Define_label(mercury__io__progname_base_4_0_i3);
	update_prof_current_proc(LABEL(mercury__io__progname_base_4_0));
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module87)
	init_entry(mercury__io__command_line_arguments_3_0);
BEGIN_CODE

/* code for predicate 'io__command_line_arguments'/3 in mode 0 */
Define_entry(mercury__io__command_line_arguments_3_0);
	{
		Word	Args;
		Word	IO0;
		Word	IO;
		IO0 = (Integer) r1;
		
	/* convert mercury_argv from a vector to a list */
	{ int i = mercury_argc;
	  Args = list_empty();
	  while (--i >= 0) {
		Args = list_cons((Word) mercury_argv[i], Args);
	  }
	}
	update_io(IO0, IO);

		r2 = Args;
		r3 = IO;

	}
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module88)
	init_entry(mercury__io__get_exit_status_3_0);
BEGIN_CODE

/* code for predicate 'io__get_exit_status'/3 in mode 0 */
Define_entry(mercury__io__get_exit_status_3_0);
	{
		Integer	ExitStatus;
		Word	IO0;
		Word	IO;
		IO0 = (Integer) r1;
		
	ExitStatus = mercury_exit_status;
	update_io(IO0, IO);

		r2 = ExitStatus;
		r3 = IO;

	}
	r1 = (Integer) r2;
	r2 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module89)
	init_entry(mercury__io__set_exit_status_3_0);
BEGIN_CODE

/* code for predicate 'io__set_exit_status'/3 in mode 0 */
Define_entry(mercury__io__set_exit_status_3_0);
	{
		Integer	ExitStatus;
		Word	IO0;
		Word	IO;
		ExitStatus = (Integer) r1;
		IO0 = (Integer) r2;
		
	mercury_exit_status = ExitStatus;
	update_io(IO0, IO);

		r3 = IO;

	}
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module90)
	init_entry(mercury__io__get_globals_3_0);
	init_label(mercury__io__get_globals_3_0_i2);
BEGIN_CODE

/* code for predicate 'io__get_globals'/3 in mode 0 */
Define_entry(mercury__io__get_globals_3_0);
	r2 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 3));
	incr_sp_push_msg(6, "io__get_globals");
	detstackvar(6) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 2));
	detstackvar(5) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 4));
	r1 = (Integer) mercury_data_std_util__base_type_info_univ_0;
	{
	Declare_entry(mercury__copy_2_0);
	call_localret(ENTRY(mercury__copy_2_0),
		mercury__io__get_globals_3_0_i2,
		ENTRY(mercury__io__get_globals_3_0));
	}
Define_label(mercury__io__get_globals_3_0_i2);
	update_prof_current_proc(LABEL(mercury__io__get_globals_3_0));
	tag_incr_hp(r2, mktag(0), ((Integer) 5));
	field(mktag(0), (Integer) r2, ((Integer) 3)) = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r2, ((Integer) 4)) = (Integer) detstackvar(5);
	field(mktag(0), (Integer) r2, ((Integer) 2)) = (Integer) detstackvar(4);
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module91)
	init_entry(mercury__io__set_globals_3_0);
BEGIN_CODE

/* code for predicate 'io__set_globals'/3 in mode 0 */
Define_entry(mercury__io__set_globals_3_0);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 5));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	field(mktag(0), (Integer) r1, ((Integer) 2)) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 2));
	field(mktag(0), (Integer) r1, ((Integer) 3)) = (Integer) r3;
	field(mktag(0), (Integer) r1, ((Integer) 4)) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 4));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module92)
	init_entry(mercury__io__get_environment_var_4_0);
	init_label(mercury__io__get_environment_var_4_0_i4);
	init_label(mercury__io__get_environment_var_4_0_i3);
BEGIN_CODE

/* code for predicate 'io__get_environment_var'/4 in mode 0 */
Define_entry(mercury__io__get_environment_var_4_0);
	incr_sp_push_msg(2, "io__get_environment_var");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	call_localret(STATIC(mercury__io__getenv_2_0),
		mercury__io__get_environment_var_4_0_i4,
		ENTRY(mercury__io__get_environment_var_4_0));
Define_label(mercury__io__get_environment_var_4_0_i4);
	update_prof_current_proc(LABEL(mercury__io__get_environment_var_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__io__get_environment_var_4_0_i3);
	tag_incr_hp(r1, mktag(1), ((Integer) 1));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
Define_label(mercury__io__get_environment_var_4_0_i3);
	r2 = (Integer) detstackvar(1);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module93)
	init_entry(mercury__io__set_environment_var_4_0);
	init_label(mercury__io__set_environment_var_4_0_i2);
	init_label(mercury__io__set_environment_var_4_0_i5);
	init_label(mercury__io__set_environment_var_4_0_i4);
	init_label(mercury__io__set_environment_var_4_0_i7);
BEGIN_CODE

/* code for predicate 'io__set_environment_var'/4 in mode 0 */
Define_entry(mercury__io__set_environment_var_4_0);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(2), ((Integer) 1));
	field(mktag(2), (Integer) tempr1, ((Integer) 0)) = (Integer) r1;
	r1 = string_const("%s=%s", 5);
	r5 = (Integer) r2;
	incr_sp_push_msg(3, "io__set_environment_var");
	detstackvar(3) = (Integer) succip;
	detstackvar(1) = (Integer) r3;
	detstackvar(2) = (Integer) tempr1;
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(2);
	tag_incr_hp(r6, mktag(1), ((Integer) 2));
	tag_incr_hp(r3, mktag(2), ((Integer) 1));
	field(mktag(1), (Integer) r6, ((Integer) 0)) = (Integer) r3;
	field(mktag(2), (Integer) r3, ((Integer) 0)) = (Integer) r5;
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) r6;
	field(mktag(1), (Integer) r6, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	{
	Declare_entry(mercury__string__format_3_0);
	call_localret(ENTRY(mercury__string__format_3_0),
		mercury__io__set_environment_var_4_0_i2,
		ENTRY(mercury__io__set_environment_var_4_0));
	}
	}
Define_label(mercury__io__set_environment_var_4_0_i2);
	update_prof_current_proc(LABEL(mercury__io__set_environment_var_4_0));
	call_localret(STATIC(mercury__io__putenv_1_0),
		mercury__io__set_environment_var_4_0_i5,
		ENTRY(mercury__io__set_environment_var_4_0));
Define_label(mercury__io__set_environment_var_4_0_i5);
	update_prof_current_proc(LABEL(mercury__io__set_environment_var_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__io__set_environment_var_4_0_i4);
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	proceed();
Define_label(mercury__io__set_environment_var_4_0_i4);
	r1 = string_const("Could not set environment variable %s", 37);
	tag_incr_hp(r2, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r2, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	{
	Declare_entry(mercury__string__format_3_0);
	call_localret(ENTRY(mercury__string__format_3_0),
		mercury__io__set_environment_var_4_0_i7,
		ENTRY(mercury__io__set_environment_var_4_0));
	}
Define_label(mercury__io__set_environment_var_4_0_i7);
	update_prof_current_proc(LABEL(mercury__io__set_environment_var_4_0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(3);
	decr_sp_pop_msg(3);
	{
	Declare_entry(mercury__require__error_1_0);
	tailcall(ENTRY(mercury__require__error_1_0),
		ENTRY(mercury__io__set_environment_var_4_0));
	}
END_MODULE

BEGIN_MODULE(mercury__io_module94)
	init_entry(mercury__io__report_stats_2_0);
	init_label(mercury__io__report_stats_2_0_i2);
BEGIN_CODE

/* code for predicate 'io__report_stats'/2 in mode 0 */
Define_entry(mercury__io__report_stats_2_0);
	incr_sp_push_msg(2, "io__report_stats");
	detstackvar(2) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	{
	Declare_entry(mercury__time__report_stats_0_0);
	call_localret(ENTRY(mercury__time__report_stats_0_0),
		mercury__io__report_stats_2_0_i2,
		ENTRY(mercury__io__report_stats_2_0));
	}
Define_label(mercury__io__report_stats_2_0_i2);
	update_prof_current_proc(LABEL(mercury__io__report_stats_2_0));
	r1 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(2);
	decr_sp_pop_msg(2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module95)
	init_entry(mercury__io__preallocate_heap_space_3_0);
BEGIN_CODE

/* code for predicate 'io__preallocate_heap_space'/3 in mode 0 */
Define_entry(mercury__io__preallocate_heap_space_3_0);
	{
		Integer	HeapSpace;
		Word	IO0;
		Word	IO;
		HeapSpace = (Integer) r1;
		IO0 = (Integer) r2;
		
	/* HeapSpace not used */
	/* don't do anything - preallocate_heap_space was just a
	   hack for NU-Prolog */
	update_io(IO0, IO);

		r3 = IO;

	}
	r1 = (Integer) r3;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module96)
	init_entry(mercury__io__call_system_4_0);
	init_label(mercury__io__call_system_4_0_i2);
	init_label(mercury__io__call_system_4_0_i3);
BEGIN_CODE

/* code for predicate 'io__call_system'/4 in mode 0 */
Define_entry(mercury__io__call_system_4_0);
	incr_sp_push_msg(1, "io__call_system");
	detstackvar(1) = (Integer) succip;
	call_localret(STATIC(mercury__io__call_system_code_4_0),
		mercury__io__call_system_4_0_i2,
		ENTRY(mercury__io__call_system_4_0));
Define_label(mercury__io__call_system_4_0_i2);
	update_prof_current_proc(LABEL(mercury__io__call_system_4_0));
	if (((Integer) r1 != ((Integer) -1)))
		GOTO_LABEL(mercury__io__call_system_4_0_i3);
	r1 = (Integer) mkword(mktag(1), (Integer) mercury_data_io__common_5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
Define_label(mercury__io__call_system_4_0_i3);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 1));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module97)
	init_entry(mercury__io__error_message_2_0);
BEGIN_CODE

/* code for predicate 'io__error_message'/2 in mode 0 */
Define_entry(mercury__io__error_message_2_0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module98)
	init_entry(mercury__io__get_op_table_3_0);
	init_label(mercury__io__get_op_table_3_0_i2);
BEGIN_CODE

/* code for predicate 'io__get_op_table'/3 in mode 0 */
Define_entry(mercury__io__get_op_table_3_0);
	r2 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 2));
	incr_sp_push_msg(6, "io__get_op_table");
	detstackvar(6) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 3));
	detstackvar(5) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 4));
	r1 = (Integer) mercury_data_ops__base_type_info_ops__table_0;
	{
	Declare_entry(mercury__copy_2_1);
	call_localret(ENTRY(mercury__copy_2_1),
		mercury__io__get_op_table_3_0_i2,
		ENTRY(mercury__io__get_op_table_3_0));
	}
Define_label(mercury__io__get_op_table_3_0_i2);
	update_prof_current_proc(LABEL(mercury__io__get_op_table_3_0));
	tag_incr_hp(r2, mktag(0), ((Integer) 5));
	field(mktag(0), (Integer) r2, ((Integer) 2)) = (Integer) r1;
	r1 = (Integer) detstackvar(1);
	field(mktag(0), (Integer) r2, ((Integer) 4)) = (Integer) detstackvar(5);
	field(mktag(0), (Integer) r2, ((Integer) 3)) = (Integer) detstackvar(4);
	field(mktag(0), (Integer) r2, ((Integer) 1)) = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r2, ((Integer) 0)) = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(6);
	decr_sp_pop_msg(6);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module99)
	init_entry(mercury__io__set_op_table_3_0);
BEGIN_CODE

/* code for predicate 'io__set_op_table'/3 in mode 0 */
Define_entry(mercury__io__set_op_table_3_0);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 5));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	field(mktag(0), (Integer) r1, ((Integer) 2)) = (Integer) r3;
	field(mktag(0), (Integer) r1, ((Integer) 3)) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 3));
	field(mktag(0), (Integer) r1, ((Integer) 4)) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 4));
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module100)
	init_entry(mercury__io__init_state_2_0);
	init_label(mercury__io__init_state_2_0_i2);
	init_label(mercury__io__init_state_2_0_i3);
	init_label(mercury__io__init_state_2_0_i4);
	init_label(mercury__io__init_state_2_0_i5);
	init_label(mercury__io__init_state_2_0_i6);
	init_label(mercury__io__init_state_2_0_i7);
	init_label(mercury__io__init_state_2_0_i8);
	init_label(mercury__io__init_state_2_0_i9);
	init_label(mercury__io__init_state_2_0_i10);
BEGIN_CODE

/* code for predicate 'io__init_state'/2 in mode 0 */
Define_entry(mercury__io__init_state_2_0);
	incr_sp_push_msg(5, "io__init_state");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mercury_data_io__base_type_info_io__stream_0;
	r2 = (Integer) mercury_data___base_type_info_string_0;
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__io__init_state_2_0_i2,
		ENTRY(mercury__io__init_state_2_0));
	}
Define_label(mercury__io__init_state_2_0_i2);
	update_prof_current_proc(LABEL(mercury__io__init_state_2_0));
	detstackvar(2) = (Integer) r1;
	r1 = (Integer) mercury_data_io__base_type_info_io__stream_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_io__common_6);
	{
	Declare_entry(mercury__map__init_1_0);
	call_localret(ENTRY(mercury__map__init_1_0),
		mercury__io__init_state_2_0_i3,
		ENTRY(mercury__io__init_state_2_0));
	}
Define_label(mercury__io__init_state_2_0_i3);
	update_prof_current_proc(LABEL(mercury__io__init_state_2_0));
	detstackvar(3) = (Integer) r1;
	{
	Declare_entry(mercury__ops__init_op_table_1_0);
	call_localret(ENTRY(mercury__ops__init_op_table_1_0),
		mercury__io__init_state_2_0_i4,
		ENTRY(mercury__io__init_state_2_0));
	}
Define_label(mercury__io__init_state_2_0_i4);
	update_prof_current_proc(LABEL(mercury__io__init_state_2_0));
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_string_0;
	r2 = string_const("<globals>", 9);
	{
	Declare_entry(mercury__std_util__type_to_univ_2_0);
	call_localret(ENTRY(mercury__std_util__type_to_univ_2_0),
		mercury__io__init_state_2_0_i5,
		ENTRY(mercury__io__init_state_2_0));
	}
Define_label(mercury__io__init_state_2_0_i5);
	update_prof_current_proc(LABEL(mercury__io__init_state_2_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 5));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(4);
	field(mktag(0), (Integer) r1, ((Integer) 3)) = (Integer) r2;
	field(mktag(0), (Integer) r1, ((Integer) 4)) = (Integer) detstackvar(1);
	{
		call_localret(STATIC(mercury__io__stdin_stream_3_0),
		mercury__io__init_state_2_0_i6,
		ENTRY(mercury__io__init_state_2_0));
	}
Define_label(mercury__io__init_state_2_0_i6);
	update_prof_current_proc(LABEL(mercury__io__init_state_2_0));
	r3 = (Integer) r2;
	r2 = string_const("<standard input>", 16);
	call_localret(STATIC(mercury__io__insert_stream_name_4_0),
		mercury__io__init_state_2_0_i7,
		ENTRY(mercury__io__init_state_2_0));
Define_label(mercury__io__init_state_2_0_i7);
	update_prof_current_proc(LABEL(mercury__io__init_state_2_0));
	{
		call_localret(STATIC(mercury__io__stdout_stream_3_0),
		mercury__io__init_state_2_0_i8,
		ENTRY(mercury__io__init_state_2_0));
	}
Define_label(mercury__io__init_state_2_0_i8);
	update_prof_current_proc(LABEL(mercury__io__init_state_2_0));
	r3 = (Integer) r2;
	r2 = string_const("<standard output>", 17);
	call_localret(STATIC(mercury__io__insert_stream_name_4_0),
		mercury__io__init_state_2_0_i9,
		ENTRY(mercury__io__init_state_2_0));
Define_label(mercury__io__init_state_2_0_i9);
	update_prof_current_proc(LABEL(mercury__io__init_state_2_0));
	{
		call_localret(STATIC(mercury__io__stderr_stream_3_0),
		mercury__io__init_state_2_0_i10,
		ENTRY(mercury__io__init_state_2_0));
	}
Define_label(mercury__io__init_state_2_0_i10);
	update_prof_current_proc(LABEL(mercury__io__init_state_2_0));
	r3 = (Integer) r2;
	r2 = string_const("<standard error>", 16);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	tailcall(STATIC(mercury__io__insert_stream_name_4_0),
		ENTRY(mercury__io__init_state_2_0));
END_MODULE

BEGIN_MODULE(mercury__io_module101)
	init_entry(mercury__io__read_char_code_4_0);
BEGIN_CODE

/* code for predicate 'io__read_char_code'/4 in mode 0 */
Define_static(mercury__io__read_char_code_4_0);
	{
		Word	File;
		Integer	CharCode;
		Word	IO0;
		Word	IO;
		File = (Integer) r1;
		IO0 = (Integer) r2;
		
	CharCode = mercury_getc((MercuryFile*)File);
	update_io(IO0, IO);

		r3 = CharCode;
		r4 = IO;

	}
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module102)
	init_entry(mercury__io__call_system_code_4_0);
BEGIN_CODE

/* code for predicate 'io__call_system_code'/4 in mode 0 */
Define_static(mercury__io__call_system_code_4_0);
	{
		String	Command;
		Integer	Status;
		Word	IO0;
		Word	IO;
		Command = (String) (Integer) r1;
		IO0 = (Integer) r2;
		
	Status = system(Command);
	update_io(IO0, IO);

		r3 = Status;
		r4 = IO;

	}
	r1 = (Integer) r3;
	r2 = (Integer) r4;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module103)
	init_entry(mercury__io__do_open_6_0);
BEGIN_CODE

/* code for predicate 'io__do_open'/6 in mode 0 */
Define_static(mercury__io__do_open_6_0);
	{
		String	FileName;
		String	Mode;
		Integer	ResultCode;
		Word	Stream;
		Word	IO0;
		Word	IO;
		FileName = (String) (Integer) r1;
		Mode = (String) (Integer) r2;
		IO0 = (Integer) r3;
		
	Stream = (Word) mercury_open(FileName, Mode);
	ResultCode = (Stream ? 0 : -1);
	update_io(IO0, IO);

		r4 = ResultCode;
		r5 = Stream;
		r6 = IO;

	}
	r1 = (Integer) r4;
	r2 = (Integer) r5;
	r3 = (Integer) r6;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module104)
	init_entry(mercury__io__getenv_2_0);
	init_label(mercury__io__getenv_2_0_i1);
BEGIN_CODE

/* code for predicate 'io__getenv'/2 in mode 0 */
Define_static(mercury__io__getenv_2_0);
	r2 = (Integer) r1;
	{
		String	Var;
		String	Value;
		Var = (String) (Integer) r1;
		{
	Value = getenv(Var);
	SUCCESS_INDICATOR = (Value != 0);
}
		r3 = (Word) Value;

	}
	if (!((Integer) r1))
		GOTO_LABEL(mercury__io__getenv_2_0_i1);
	r2 = (Integer) r3;
	r1 = TRUE;
	proceed();
Define_label(mercury__io__getenv_2_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module105)
	init_entry(mercury__io__putenv_1_0);
	init_label(mercury__io__putenv_1_0_i1);
BEGIN_CODE

/* code for predicate 'io__putenv'/1 in mode 0 */
Define_static(mercury__io__putenv_1_0);
	r2 = (Integer) r1;
	{
		String	VarAndValue;
		VarAndValue = (String) (Integer) r1;
		
	SUCCESS_INDICATOR = (putenv(VarAndValue) == 0);


	}
	if (!((Integer) r1))
		GOTO_LABEL(mercury__io__putenv_1_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury__io__putenv_1_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module106)
	init_entry(mercury__io__read_word_2_4_0);
	init_label(mercury__io__read_word_2_4_0_i2);
	init_label(mercury__io__read_word_2_4_0_i5);
	init_label(mercury__io__read_word_2_4_0_i9);
	init_label(mercury__io__read_word_2_4_0_i11);
	init_label(mercury__io__read_word_2_4_0_i8);
	init_label(mercury__io__read_word_2_4_0_i12);
	init_label(mercury__io__read_word_2_4_0_i15);
	init_label(mercury__io__read_word_2_4_0_i3);
BEGIN_CODE

/* code for predicate 'io__read_word_2'/4 in mode 0 */
Define_static(mercury__io__read_word_2_4_0);
	incr_sp_push_msg(4, "io__read_word_2");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	{
		call_localret(STATIC(mercury__io__read_char_4_0),
		mercury__io__read_word_2_4_0_i2,
		STATIC(mercury__io__read_word_2_4_0));
	}
Define_label(mercury__io__read_word_2_4_0_i2);
	update_prof_current_proc(LABEL(mercury__io__read_word_2_4_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__io__read_word_2_4_0_i5);
	r1 = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__io__read_word_2_4_0_i5);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__io__read_word_2_4_0_i3);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) r1;
	detstackvar(3) = (Integer) r2;
	{
	Declare_entry(mercury__char__is_whitespace_1_0);
	call_localret(ENTRY(mercury__char__is_whitespace_1_0),
		mercury__io__read_word_2_4_0_i9,
		STATIC(mercury__io__read_word_2_4_0));
	}
Define_label(mercury__io__read_word_2_4_0_i9);
	update_prof_current_proc(LABEL(mercury__io__read_word_2_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__io__read_word_2_4_0_i8);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	r3 = (Integer) detstackvar(3);
	{
		call_localret(STATIC(mercury__io__putback_char_4_0),
		mercury__io__read_word_2_4_0_i11,
		STATIC(mercury__io__read_word_2_4_0));
	}
Define_label(mercury__io__read_word_2_4_0_i11);
	update_prof_current_proc(LABEL(mercury__io__read_word_2_4_0));
	r2 = (Integer) r1;
	r1 = (Integer) mkword(mktag(1), (Integer) mercury_data_io__common_7);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__io__read_word_2_4_0_i8);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(3);
	localcall(mercury__io__read_word_2_4_0,
		LABEL(mercury__io__read_word_2_4_0_i12),
		STATIC(mercury__io__read_word_2_4_0));
Define_label(mercury__io__read_word_2_4_0_i12);
	update_prof_current_proc(LABEL(mercury__io__read_word_2_4_0));
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury__io__read_word_2_4_0_i15);
	tag_incr_hp(r1, mktag(1), ((Integer) 1));
	tag_incr_hp(r3, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) r3, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r3, ((Integer) 1)) = (Integer) mkword(mktag(0), mkbody(((Integer) 0)));
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) r3;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury__io__read_word_2_4_0_i15);
	if (((Integer) r3 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury__io__read_word_2_4_0_i3);
	r3 = (Integer) r1;
	tag_incr_hp(r1, mktag(1), ((Integer) 1));
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(1), ((Integer) 2));
	field(mktag(1), (Integer) tempr1, ((Integer) 1)) = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	field(mktag(1), (Integer) tempr1, ((Integer) 0)) = (Integer) detstackvar(2);
	field(mktag(1), (Integer) r1, ((Integer) 0)) = (Integer) tempr1;
	}
Define_label(mercury__io__read_word_2_4_0_i3);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module107)
	init_entry(mercury__io__stream_name_4_0);
	init_label(mercury__io__stream_name_4_0_i2);
	init_label(mercury__io__stream_name_4_0_i5);
	init_label(mercury__io__stream_name_4_0_i4);
BEGIN_CODE

/* code for predicate 'io__stream_name'/4 in mode 0 */
Define_static(mercury__io__stream_name_4_0);
	incr_sp_push_msg(7, "io__stream_name");
	detstackvar(7) = (Integer) succip;
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 2));
	detstackvar(5) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 3));
	detstackvar(6) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 4));
	detstackvar(1) = (Integer) r1;
	r2 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	detstackvar(2) = (Integer) r2;
	r1 = (Integer) mkword(mktag(0), (Integer) mercury_data_io__common_8);
	{
	Declare_entry(mercury__copy_2_0);
	call_localret(ENTRY(mercury__copy_2_0),
		mercury__io__stream_name_4_0_i2,
		STATIC(mercury__io__stream_name_4_0));
	}
Define_label(mercury__io__stream_name_4_0_i2);
	update_prof_current_proc(LABEL(mercury__io__stream_name_4_0));
	r4 = (Integer) detstackvar(1);
	{
	Word tempr1;
	tag_incr_hp(tempr1, mktag(0), ((Integer) 5));
	detstackvar(1) = (Integer) tempr1;
	field(mktag(0), (Integer) tempr1, ((Integer) 0)) = (Integer) r1;
	r1 = (Integer) mercury_data_io__base_type_info_io__stream_0;
	r2 = (Integer) mercury_data___base_type_info_string_0;
	r3 = (Integer) detstackvar(2);
	field(mktag(0), (Integer) tempr1, ((Integer) 4)) = (Integer) detstackvar(6);
	field(mktag(0), (Integer) tempr1, ((Integer) 3)) = (Integer) detstackvar(5);
	field(mktag(0), (Integer) tempr1, ((Integer) 2)) = (Integer) detstackvar(4);
	field(mktag(0), (Integer) tempr1, ((Integer) 1)) = (Integer) detstackvar(3);
	{
	Declare_entry(mercury__map__search_3_1);
	call_localret(ENTRY(mercury__map__search_3_1),
		mercury__io__stream_name_4_0_i5,
		STATIC(mercury__io__stream_name_4_0));
	}
	}
Define_label(mercury__io__stream_name_4_0_i5);
	update_prof_current_proc(LABEL(mercury__io__stream_name_4_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury__io__stream_name_4_0_i4);
	r1 = (Integer) r2;
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
Define_label(mercury__io__stream_name_4_0_i4);
	r1 = string_const("<stream name unavailable>", 25);
	r2 = (Integer) detstackvar(1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module108)
	init_entry(mercury__io__insert_stream_name_4_0);
	init_label(mercury__io__insert_stream_name_4_0_i2);
	init_label(mercury__io__insert_stream_name_4_0_i3);
	init_label(mercury__io__insert_stream_name_4_0_i4);
BEGIN_CODE

/* code for predicate 'io__insert_stream_name'/4 in mode 0 */
Define_static(mercury__io__insert_stream_name_4_0);
	incr_sp_push_msg(7, "io__insert_stream_name");
	detstackvar(7) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	r2 = (Integer) r1;
	detstackvar(6) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 4));
	detstackvar(5) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 3));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 2));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	r1 = (Integer) mercury_data_io__base_type_info_io__stream_0;
	{
	Declare_entry(mercury__copy_2_1);
	call_localret(ENTRY(mercury__copy_2_1),
		mercury__io__insert_stream_name_4_0_i2,
		STATIC(mercury__io__insert_stream_name_4_0));
	}
Define_label(mercury__io__insert_stream_name_4_0_i2);
	update_prof_current_proc(LABEL(mercury__io__insert_stream_name_4_0));
	r2 = (Integer) detstackvar(1);
	detstackvar(1) = (Integer) r1;
	r1 = (Integer) mercury_data___base_type_info_string_0;
	{
	Declare_entry(mercury__copy_2_1);
	call_localret(ENTRY(mercury__copy_2_1),
		mercury__io__insert_stream_name_4_0_i3,
		STATIC(mercury__io__insert_stream_name_4_0));
	}
Define_label(mercury__io__insert_stream_name_4_0_i3);
	update_prof_current_proc(LABEL(mercury__io__insert_stream_name_4_0));
	r5 = (Integer) r1;
	r1 = (Integer) mercury_data_io__base_type_info_io__stream_0;
	r2 = (Integer) mercury_data___base_type_info_string_0;
	r3 = (Integer) detstackvar(2);
	r4 = (Integer) detstackvar(1);
	{
	Declare_entry(mercury__map__set_4_0);
	call_localret(ENTRY(mercury__map__set_4_0),
		mercury__io__insert_stream_name_4_0_i4,
		STATIC(mercury__io__insert_stream_name_4_0));
	}
Define_label(mercury__io__insert_stream_name_4_0_i4);
	update_prof_current_proc(LABEL(mercury__io__insert_stream_name_4_0));
	r2 = (Integer) r1;
	tag_incr_hp(r1, mktag(0), ((Integer) 5));
	field(mktag(0), (Integer) r1, ((Integer) 0)) = (Integer) r2;
	field(mktag(0), (Integer) r1, ((Integer) 1)) = (Integer) detstackvar(3);
	field(mktag(0), (Integer) r1, ((Integer) 2)) = (Integer) detstackvar(4);
	field(mktag(0), (Integer) r1, ((Integer) 3)) = (Integer) detstackvar(5);
	field(mktag(0), (Integer) r1, ((Integer) 4)) = (Integer) detstackvar(6);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(7);
	decr_sp_pop_msg(7);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module109)
	init_entry(mercury____Unify___io__state_0_0);
	init_label(mercury____Unify___io__state_0_0_i2);
	init_label(mercury____Unify___io__state_0_0_i4);
	init_label(mercury____Unify___io__state_0_0_i6);
	init_label(mercury____Unify___io__state_0_0_i8);
	init_label(mercury____Unify___io__state_0_0_i1);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___io__state_0_0);
	r3 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	r4 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	incr_sp_push_msg(9, "__Unify__");
	detstackvar(9) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 2));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 3));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 4));
	detstackvar(5) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	detstackvar(6) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 2));
	detstackvar(7) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 3));
	detstackvar(8) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 4));
	r1 = (Integer) mercury_data_io__base_type_info_io__stream_0;
	r2 = (Integer) mercury_data___base_type_info_string_0;
	{
	Declare_entry(mercury____Unify___tree234__tree234_2_0);
	call_localret(ENTRY(mercury____Unify___tree234__tree234_2_0),
		mercury____Unify___io__state_0_0_i2,
		ENTRY(mercury____Unify___io__state_0_0));
	}
Define_label(mercury____Unify___io__state_0_0_i2);
	update_prof_current_proc(LABEL(mercury____Unify___io__state_0_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury____Unify___io__state_0_0_i1);
	r1 = (Integer) mercury_data_io__base_type_info_io__stream_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_io__common_6);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury____Unify___tree234__tree234_2_0);
	call_localret(ENTRY(mercury____Unify___tree234__tree234_2_0),
		mercury____Unify___io__state_0_0_i4,
		ENTRY(mercury____Unify___io__state_0_0));
	}
Define_label(mercury____Unify___io__state_0_0_i4);
	update_prof_current_proc(LABEL(mercury____Unify___io__state_0_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury____Unify___io__state_0_0_i1);
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury____Unify___ops__table_0_0);
	call_localret(ENTRY(mercury____Unify___ops__table_0_0),
		mercury____Unify___io__state_0_0_i6,
		ENTRY(mercury____Unify___io__state_0_0));
	}
Define_label(mercury____Unify___io__state_0_0_i6);
	update_prof_current_proc(LABEL(mercury____Unify___io__state_0_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury____Unify___io__state_0_0_i1);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury____Unify___std_util__univ_0_0);
	call_localret(ENTRY(mercury____Unify___std_util__univ_0_0),
		mercury____Unify___io__state_0_0_i8,
		ENTRY(mercury____Unify___io__state_0_0));
	}
Define_label(mercury____Unify___io__state_0_0_i8);
	update_prof_current_proc(LABEL(mercury____Unify___io__state_0_0));
	if (!((Integer) r1))
		GOTO_LABEL(mercury____Unify___io__state_0_0_i1);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(8);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	tailcall(ENTRY(mercury____Unify___io__external_state_0_0),
		ENTRY(mercury____Unify___io__state_0_0));
Define_label(mercury____Unify___io__state_0_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module110)
	init_entry(mercury____Index___io__state_0_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___io__state_0_0);
	tailcall(STATIC(mercury____Index___io_io__state_0__ua10000_2_0),
		ENTRY(mercury____Index___io__state_0_0));
END_MODULE

BEGIN_MODULE(mercury__io_module111)
	init_entry(mercury____Compare___io__state_0_0);
	init_label(mercury____Compare___io__state_0_0_i4);
	init_label(mercury____Compare___io__state_0_0_i5);
	init_label(mercury____Compare___io__state_0_0_i3);
	init_label(mercury____Compare___io__state_0_0_i10);
	init_label(mercury____Compare___io__state_0_0_i16);
	init_label(mercury____Compare___io__state_0_0_i22);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___io__state_0_0);
	r3 = (Integer) field(mktag(0), (Integer) r1, ((Integer) 0));
	r4 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	incr_sp_push_msg(9, "__Compare__");
	detstackvar(9) = (Integer) succip;
	detstackvar(1) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 1));
	detstackvar(2) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 2));
	detstackvar(3) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 3));
	detstackvar(4) = (Integer) field(mktag(0), (Integer) r1, ((Integer) 4));
	detstackvar(5) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 1));
	detstackvar(6) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 2));
	detstackvar(7) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 3));
	detstackvar(8) = (Integer) field(mktag(0), (Integer) r2, ((Integer) 4));
	r1 = (Integer) mercury_data_io__base_type_info_io__stream_0;
	r2 = (Integer) mercury_data___base_type_info_string_0;
	{
	Declare_entry(mercury____Compare___tree234__tree234_2_0);
	call_localret(ENTRY(mercury____Compare___tree234__tree234_2_0),
		mercury____Compare___io__state_0_0_i4,
		ENTRY(mercury____Compare___io__state_0_0));
	}
Define_label(mercury____Compare___io__state_0_0_i4);
	update_prof_current_proc(LABEL(mercury____Compare___io__state_0_0));
	if (((Integer) r1 == ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___io__state_0_0_i3);
Define_label(mercury____Compare___io__state_0_0_i5);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	proceed();
Define_label(mercury____Compare___io__state_0_0_i3);
	r1 = (Integer) mercury_data_io__base_type_info_io__stream_0;
	r2 = (Integer) mkword(mktag(0), (Integer) mercury_data_io__common_6);
	r3 = (Integer) detstackvar(1);
	r4 = (Integer) detstackvar(5);
	{
	Declare_entry(mercury____Compare___tree234__tree234_2_0);
	call_localret(ENTRY(mercury____Compare___tree234__tree234_2_0),
		mercury____Compare___io__state_0_0_i10,
		ENTRY(mercury____Compare___io__state_0_0));
	}
Define_label(mercury____Compare___io__state_0_0_i10);
	update_prof_current_proc(LABEL(mercury____Compare___io__state_0_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___io__state_0_0_i5);
	r1 = (Integer) detstackvar(2);
	r2 = (Integer) detstackvar(6);
	{
	Declare_entry(mercury____Compare___ops__table_0_0);
	call_localret(ENTRY(mercury____Compare___ops__table_0_0),
		mercury____Compare___io__state_0_0_i16,
		ENTRY(mercury____Compare___io__state_0_0));
	}
Define_label(mercury____Compare___io__state_0_0_i16);
	update_prof_current_proc(LABEL(mercury____Compare___io__state_0_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___io__state_0_0_i5);
	r1 = (Integer) detstackvar(3);
	r2 = (Integer) detstackvar(7);
	{
	Declare_entry(mercury____Compare___std_util__univ_0_0);
	call_localret(ENTRY(mercury____Compare___std_util__univ_0_0),
		mercury____Compare___io__state_0_0_i22,
		ENTRY(mercury____Compare___io__state_0_0));
	}
Define_label(mercury____Compare___io__state_0_0_i22);
	update_prof_current_proc(LABEL(mercury____Compare___io__state_0_0));
	if (((Integer) r1 != ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___io__state_0_0_i5);
	r1 = (Integer) detstackvar(4);
	r2 = (Integer) detstackvar(8);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(9);
	decr_sp_pop_msg(9);
	tailcall(ENTRY(mercury____Compare___io__external_state_0_0),
		ENTRY(mercury____Compare___io__state_0_0));
END_MODULE

BEGIN_MODULE(mercury__io_module112)
	init_entry(mercury____Unify___io__input_stream_0_0);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___io__input_stream_0_0);
	tailcall(ENTRY(mercury____Unify___io__stream_0_0),
		ENTRY(mercury____Unify___io__input_stream_0_0));
END_MODULE

BEGIN_MODULE(mercury__io_module113)
	init_entry(mercury____Index___io__input_stream_0_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___io__input_stream_0_0);
	tailcall(ENTRY(mercury____Index___io__stream_0_0),
		ENTRY(mercury____Index___io__input_stream_0_0));
END_MODULE

BEGIN_MODULE(mercury__io_module114)
	init_entry(mercury____Compare___io__input_stream_0_0);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___io__input_stream_0_0);
	tailcall(ENTRY(mercury____Compare___io__stream_0_0),
		ENTRY(mercury____Compare___io__input_stream_0_0));
END_MODULE

BEGIN_MODULE(mercury__io_module115)
	init_entry(mercury____Unify___io__output_stream_0_0);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___io__output_stream_0_0);
	tailcall(ENTRY(mercury____Unify___io__stream_0_0),
		ENTRY(mercury____Unify___io__output_stream_0_0));
END_MODULE

BEGIN_MODULE(mercury__io_module116)
	init_entry(mercury____Index___io__output_stream_0_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___io__output_stream_0_0);
	tailcall(ENTRY(mercury____Index___io__stream_0_0),
		ENTRY(mercury____Index___io__output_stream_0_0));
END_MODULE

BEGIN_MODULE(mercury__io_module117)
	init_entry(mercury____Compare___io__output_stream_0_0);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___io__output_stream_0_0);
	tailcall(ENTRY(mercury____Compare___io__stream_0_0),
		ENTRY(mercury____Compare___io__output_stream_0_0));
END_MODULE

BEGIN_MODULE(mercury__io_module118)
	init_entry(mercury____Unify___io__binary_input_stream_0_0);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___io__binary_input_stream_0_0);
	tailcall(ENTRY(mercury____Unify___io__stream_0_0),
		ENTRY(mercury____Unify___io__binary_input_stream_0_0));
END_MODULE

BEGIN_MODULE(mercury__io_module119)
	init_entry(mercury____Index___io__binary_input_stream_0_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___io__binary_input_stream_0_0);
	tailcall(ENTRY(mercury____Index___io__stream_0_0),
		ENTRY(mercury____Index___io__binary_input_stream_0_0));
END_MODULE

BEGIN_MODULE(mercury__io_module120)
	init_entry(mercury____Compare___io__binary_input_stream_0_0);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___io__binary_input_stream_0_0);
	tailcall(ENTRY(mercury____Compare___io__stream_0_0),
		ENTRY(mercury____Compare___io__binary_input_stream_0_0));
END_MODULE

BEGIN_MODULE(mercury__io_module121)
	init_entry(mercury____Unify___io__binary_output_stream_0_0);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___io__binary_output_stream_0_0);
	tailcall(ENTRY(mercury____Unify___io__stream_0_0),
		ENTRY(mercury____Unify___io__binary_output_stream_0_0));
END_MODULE

BEGIN_MODULE(mercury__io_module122)
	init_entry(mercury____Index___io__binary_output_stream_0_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___io__binary_output_stream_0_0);
	tailcall(ENTRY(mercury____Index___io__stream_0_0),
		ENTRY(mercury____Index___io__binary_output_stream_0_0));
END_MODULE

BEGIN_MODULE(mercury__io_module123)
	init_entry(mercury____Compare___io__binary_output_stream_0_0);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___io__binary_output_stream_0_0);
	tailcall(ENTRY(mercury____Compare___io__stream_0_0),
		ENTRY(mercury____Compare___io__binary_output_stream_0_0));
END_MODULE

BEGIN_MODULE(mercury__io_module124)
	init_entry(mercury____Unify___io__res_0_0);
	init_label(mercury____Unify___io__res_0_0_i3);
	init_label(mercury____Unify___io__res_0_0_i2);
	init_label(mercury____Unify___io__res_0_0_i1);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___io__res_0_0);
	if (((Integer) r1 != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Unify___io__res_0_0_i3);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Unify___io__res_0_0_i2);
	r1 = FALSE;
	proceed();
Define_label(mercury____Unify___io__res_0_0_i3);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Unify___io__res_0_0_i1);
	if ((strcmp((char *)(Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), (char *)(Integer) field(mktag(1), (Integer) r2, ((Integer) 0))) !=0))
		GOTO_LABEL(mercury____Unify___io__res_0_0_i1);
Define_label(mercury____Unify___io__res_0_0_i2);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___io__res_0_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module125)
	init_entry(mercury____Index___io__res_0_0);
	init_label(mercury____Index___io__res_0_0_i3);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___io__res_0_0);
	if (((Integer) r1 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Index___io__res_0_0_i3);
	r1 = ((Integer) 1);
	proceed();
Define_label(mercury____Index___io__res_0_0_i3);
	r1 = ((Integer) 0);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module126)
	init_entry(mercury____Compare___io__res_0_0);
	init_label(mercury____Compare___io__res_0_0_i2);
	init_label(mercury____Compare___io__res_0_0_i3);
	init_label(mercury____Compare___io__res_0_0_i4);
	init_label(mercury____Compare___io__res_0_0_i6);
	init_label(mercury____Compare___io__res_0_0_i11);
	init_label(mercury____Compare___io__res_0_0_i9);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___io__res_0_0);
	incr_sp_push_msg(4, "__Compare__");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	{
		call_localret(STATIC(mercury____Index___io__res_0_0),
		mercury____Compare___io__res_0_0_i2,
		ENTRY(mercury____Compare___io__res_0_0));
	}
Define_label(mercury____Compare___io__res_0_0_i2);
	update_prof_current_proc(LABEL(mercury____Compare___io__res_0_0));
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	{
		call_localret(STATIC(mercury____Index___io__res_0_0),
		mercury____Compare___io__res_0_0_i3,
		ENTRY(mercury____Compare___io__res_0_0));
	}
Define_label(mercury____Compare___io__res_0_0_i3);
	update_prof_current_proc(LABEL(mercury____Compare___io__res_0_0));
	if (((Integer) detstackvar(3) >= (Integer) r1))
		GOTO_LABEL(mercury____Compare___io__res_0_0_i4);
	r1 = ((Integer) 1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury____Compare___io__res_0_0_i4);
	if (((Integer) detstackvar(3) <= (Integer) r1))
		GOTO_LABEL(mercury____Compare___io__res_0_0_i6);
	r1 = ((Integer) 2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury____Compare___io__res_0_0_i6);
	if (((Integer) detstackvar(1) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Compare___io__res_0_0_i11);
	if (((Integer) detstackvar(2) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Compare___io__res_0_0_i9);
	r1 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury____Compare___io__res_0_0_i11);
	r3 = (Integer) detstackvar(2);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Compare___io__res_0_0_i9);
	r1 = (Integer) field(mktag(1), (Integer) detstackvar(1), ((Integer) 0));
	r2 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
	Declare_entry(mercury__builtin_compare_string_3_0);
	tailcall(ENTRY(mercury__builtin_compare_string_3_0),
		ENTRY(mercury____Compare___io__res_0_0));
	}
Define_label(mercury____Compare___io__res_0_0_i9);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
	Declare_entry(mercury__compare_error_0_0);
	tailcall(ENTRY(mercury__compare_error_0_0),
		ENTRY(mercury____Compare___io__res_0_0));
	}
END_MODULE

BEGIN_MODULE(mercury__io_module127)
	init_entry(mercury____Unify___io__res_1_0);
	init_label(mercury____Unify___io__res_1_0_i1008);
	init_label(mercury____Unify___io__res_1_0_i1005);
	init_label(mercury____Unify___io__res_1_0_i1);
	init_label(mercury____Unify___io__res_1_0_i1007);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___io__res_1_0);
	if ((tag((Integer) r2) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury____Unify___io__res_1_0_i1008);
	if ((tag((Integer) r3) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury____Unify___io__res_1_0_i1005);
	r2 = (Integer) field(mktag(0), (Integer) r2, ((Integer) 0));
	r3 = (Integer) field(mktag(0), (Integer) r3, ((Integer) 0));
	{
	Declare_entry(mercury__unify_2_0);
	tailcall(ENTRY(mercury__unify_2_0),
		ENTRY(mercury____Unify___io__res_1_0));
	}
Define_label(mercury____Unify___io__res_1_0_i1008);
	incr_sp_push_msg(1, "__Unify__");
	detstackvar(1) = (Integer) succip;
	if ((tag((Integer) r3) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury____Unify___io__res_1_0_i1);
	decr_sp_pop_msg(1);
	if ((strcmp((char *)(Integer) field(mktag(1), (Integer) r2, ((Integer) 0)), (char *)(Integer) field(mktag(1), (Integer) r3, ((Integer) 0))) !=0))
		GOTO_LABEL(mercury____Unify___io__res_1_0_i1007);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___io__res_1_0_i1005);
	r1 = FALSE;
	proceed();
Define_label(mercury____Unify___io__res_1_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
Define_label(mercury____Unify___io__res_1_0_i1007);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module128)
	init_entry(mercury____Index___io__res_1_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___io__res_1_0);
	r1 = (Integer) r2;
	tailcall(STATIC(mercury____Index___io_io__res_1__ua10000_2_0),
		ENTRY(mercury____Index___io__res_1_0));
END_MODULE

BEGIN_MODULE(mercury__io_module129)
	init_entry(mercury____Compare___io__res_1_0);
	init_label(mercury____Compare___io__res_1_0_i2);
	init_label(mercury____Compare___io__res_1_0_i3);
	init_label(mercury____Compare___io__res_1_0_i4);
	init_label(mercury____Compare___io__res_1_0_i6);
	init_label(mercury____Compare___io__res_1_0_i11);
	init_label(mercury____Compare___io__res_1_0_i9);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___io__res_1_0);
	incr_sp_push_msg(5, "__Compare__");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury____Index___io_io__res_1__ua10000_2_0),
		mercury____Compare___io__res_1_0_i2,
		ENTRY(mercury____Compare___io__res_1_0));
Define_label(mercury____Compare___io__res_1_0_i2);
	update_prof_current_proc(LABEL(mercury____Compare___io__res_1_0));
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury____Index___io_io__res_1__ua10000_2_0),
		mercury____Compare___io__res_1_0_i3,
		ENTRY(mercury____Compare___io__res_1_0));
Define_label(mercury____Compare___io__res_1_0_i3);
	update_prof_current_proc(LABEL(mercury____Compare___io__res_1_0));
	if (((Integer) detstackvar(3) >= (Integer) r1))
		GOTO_LABEL(mercury____Compare___io__res_1_0_i4);
	r1 = ((Integer) 1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury____Compare___io__res_1_0_i4);
	if (((Integer) detstackvar(3) <= (Integer) r1))
		GOTO_LABEL(mercury____Compare___io__res_1_0_i6);
	r1 = ((Integer) 2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury____Compare___io__res_1_0_i6);
	{
	Word tempr1, tempr2;
	tempr1 = (Integer) detstackvar(1);
	if ((tag((Integer) tempr1) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury____Compare___io__res_1_0_i11);
	tempr2 = (Integer) detstackvar(2);
	if ((tag((Integer) tempr2) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury____Compare___io__res_1_0_i9);
	r3 = (Integer) field(mktag(0), (Integer) tempr2, ((Integer) 0));
	r2 = (Integer) field(mktag(0), (Integer) tempr1, ((Integer) 0));
	r1 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	{
	Declare_entry(mercury__compare_3_3);
	tailcall(ENTRY(mercury__compare_3_3),
		ENTRY(mercury____Compare___io__res_1_0));
	}
	}
Define_label(mercury____Compare___io__res_1_0_i11);
	r3 = (Integer) detstackvar(2);
	if ((tag((Integer) r3) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury____Compare___io__res_1_0_i9);
	r1 = (Integer) field(mktag(1), (Integer) detstackvar(1), ((Integer) 0));
	r2 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	{
	Declare_entry(mercury__builtin_compare_string_3_0);
	tailcall(ENTRY(mercury__builtin_compare_string_3_0),
		ENTRY(mercury____Compare___io__res_1_0));
	}
Define_label(mercury____Compare___io__res_1_0_i9);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	{
	Declare_entry(mercury__compare_error_0_0);
	tailcall(ENTRY(mercury__compare_error_0_0),
		ENTRY(mercury____Compare___io__res_1_0));
	}
END_MODULE

BEGIN_MODULE(mercury__io_module130)
	init_entry(mercury____Unify___io__result_0_0);
	init_label(mercury____Unify___io__result_0_0_i5);
	init_label(mercury____Unify___io__result_0_0_i4);
	init_label(mercury____Unify___io__result_0_0_i2);
	init_label(mercury____Unify___io__result_0_0_i1);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___io__result_0_0);
	r3 = tag((Integer) r1);
	if (((Integer) r3 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury____Unify___io__result_0_0_i4);
	if ((unmkbody((Integer) r1) != ((Integer) 0)))
		GOTO_LABEL(mercury____Unify___io__result_0_0_i5);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Unify___io__result_0_0_i2);
	r1 = FALSE;
	proceed();
Define_label(mercury____Unify___io__result_0_0_i5);
	if (((Integer) r2 == (Integer) mkword(mktag(0), mkbody(((Integer) 1)))))
		GOTO_LABEL(mercury____Unify___io__result_0_0_i2);
	r1 = FALSE;
	proceed();
Define_label(mercury____Unify___io__result_0_0_i4);
	if ((tag((Integer) r2) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury____Unify___io__result_0_0_i1);
	if ((strcmp((char *)(Integer) field(mktag(1), (Integer) r1, ((Integer) 0)), (char *)(Integer) field(mktag(1), (Integer) r2, ((Integer) 0))) !=0))
		GOTO_LABEL(mercury____Unify___io__result_0_0_i1);
Define_label(mercury____Unify___io__result_0_0_i2);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___io__result_0_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module131)
	init_entry(mercury____Index___io__result_0_0);
	init_label(mercury____Index___io__result_0_0_i5);
	init_label(mercury____Index___io__result_0_0_i4);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___io__result_0_0);
	if ((tag((Integer) r1) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury____Index___io__result_0_0_i4);
	if ((unmkbody((Integer) r1) != ((Integer) 0)))
		GOTO_LABEL(mercury____Index___io__result_0_0_i5);
	r1 = ((Integer) 0);
	proceed();
Define_label(mercury____Index___io__result_0_0_i5);
	r1 = ((Integer) 1);
	proceed();
Define_label(mercury____Index___io__result_0_0_i4);
	r1 = ((Integer) 2);
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module132)
	init_entry(mercury____Compare___io__result_0_0);
	init_label(mercury____Compare___io__result_0_0_i2);
	init_label(mercury____Compare___io__result_0_0_i3);
	init_label(mercury____Compare___io__result_0_0_i4);
	init_label(mercury____Compare___io__result_0_0_i6);
	init_label(mercury____Compare___io__result_0_0_i13);
	init_label(mercury____Compare___io__result_0_0_i12);
	init_label(mercury____Compare___io__result_0_0_i9);
	init_label(mercury____Compare___io__result_0_0_i1000);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___io__result_0_0);
	incr_sp_push_msg(4, "__Compare__");
	detstackvar(4) = (Integer) succip;
	detstackvar(1) = (Integer) r1;
	detstackvar(2) = (Integer) r2;
	{
		call_localret(STATIC(mercury____Index___io__result_0_0),
		mercury____Compare___io__result_0_0_i2,
		ENTRY(mercury____Compare___io__result_0_0));
	}
Define_label(mercury____Compare___io__result_0_0_i2);
	update_prof_current_proc(LABEL(mercury____Compare___io__result_0_0));
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	{
		call_localret(STATIC(mercury____Index___io__result_0_0),
		mercury____Compare___io__result_0_0_i3,
		ENTRY(mercury____Compare___io__result_0_0));
	}
Define_label(mercury____Compare___io__result_0_0_i3);
	update_prof_current_proc(LABEL(mercury____Compare___io__result_0_0));
	if (((Integer) detstackvar(3) >= (Integer) r1))
		GOTO_LABEL(mercury____Compare___io__result_0_0_i4);
	r1 = ((Integer) 1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury____Compare___io__result_0_0_i4);
	if (((Integer) detstackvar(3) <= (Integer) r1))
		GOTO_LABEL(mercury____Compare___io__result_0_0_i6);
	r1 = ((Integer) 2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury____Compare___io__result_0_0_i6);
	r1 = (Integer) detstackvar(1);
	r2 = tag((Integer) r1);
	if (((Integer) r2 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury____Compare___io__result_0_0_i12);
	if ((unmkbody((Integer) r1) != ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___io__result_0_0_i13);
	if (((Integer) detstackvar(2) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Compare___io__result_0_0_i9);
	r1 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury____Compare___io__result_0_0_i13);
	if (((Integer) detstackvar(2) != (Integer) mkword(mktag(0), mkbody(((Integer) 1)))))
		GOTO_LABEL(mercury____Compare___io__result_0_0_i9);
	r1 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	proceed();
Define_label(mercury____Compare___io__result_0_0_i12);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	if ((tag((Integer) r3) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury____Compare___io__result_0_0_i1000);
	r1 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r2 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	{
	Declare_entry(mercury__builtin_compare_string_3_0);
	tailcall(ENTRY(mercury__builtin_compare_string_3_0),
		ENTRY(mercury____Compare___io__result_0_0));
	}
Define_label(mercury____Compare___io__result_0_0_i9);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(4);
	decr_sp_pop_msg(4);
	{
	Declare_entry(mercury__compare_error_0_0);
	tailcall(ENTRY(mercury__compare_error_0_0),
		ENTRY(mercury____Compare___io__result_0_0));
	}
Define_label(mercury____Compare___io__result_0_0_i1000);
	{
	Declare_entry(mercury__compare_error_0_0);
	tailcall(ENTRY(mercury__compare_error_0_0),
		ENTRY(mercury____Compare___io__result_0_0));
	}
END_MODULE

BEGIN_MODULE(mercury__io_module133)
	init_entry(mercury____Unify___io__result_1_0);
	init_label(mercury____Unify___io__result_1_0_i1010);
	init_label(mercury____Unify___io__result_1_0_i6);
	init_label(mercury____Unify___io__result_1_0_i1007);
	init_label(mercury____Unify___io__result_1_0_i1);
	init_label(mercury____Unify___io__result_1_0_i1009);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___io__result_1_0);
	if ((tag((Integer) r2) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury____Unify___io__result_1_0_i1010);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Unify___io__result_1_0_i1007);
	r1 = FALSE;
	proceed();
Define_label(mercury____Unify___io__result_1_0_i1010);
	incr_sp_push_msg(1, "__Unify__");
	detstackvar(1) = (Integer) succip;
	if ((tag((Integer) r2) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury____Unify___io__result_1_0_i6);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if ((tag((Integer) r3) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury____Unify___io__result_1_0_i1009);
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r3 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	{
	Declare_entry(mercury__unify_2_0);
	tailcall(ENTRY(mercury__unify_2_0),
		ENTRY(mercury____Unify___io__result_1_0));
	}
Define_label(mercury____Unify___io__result_1_0_i6);
	if ((tag((Integer) r3) != mktag(((Integer) 2))))
		GOTO_LABEL(mercury____Unify___io__result_1_0_i1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if ((strcmp((char *)(Integer) field(mktag(2), (Integer) r2, ((Integer) 0)), (char *)(Integer) field(mktag(2), (Integer) r3, ((Integer) 0))) !=0))
		GOTO_LABEL(mercury____Unify___io__result_1_0_i1009);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___io__result_1_0_i1007);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___io__result_1_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
Define_label(mercury____Unify___io__result_1_0_i1009);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module134)
	init_entry(mercury____Index___io__result_1_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___io__result_1_0);
	r1 = (Integer) r2;
	tailcall(STATIC(mercury____Index___io_io__result_1__ua10000_2_0),
		ENTRY(mercury____Index___io__result_1_0));
END_MODULE

BEGIN_MODULE(mercury__io_module135)
	init_entry(mercury____Compare___io__result_1_0);
	init_label(mercury____Compare___io__result_1_0_i2);
	init_label(mercury____Compare___io__result_1_0_i3);
	init_label(mercury____Compare___io__result_1_0_i4);
	init_label(mercury____Compare___io__result_1_0_i6);
	init_label(mercury____Compare___io__result_1_0_i12);
	init_label(mercury____Compare___io__result_1_0_i14);
	init_label(mercury____Compare___io__result_1_0_i9);
	init_label(mercury____Compare___io__result_1_0_i1000);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___io__result_1_0);
	incr_sp_push_msg(5, "__Compare__");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury____Index___io_io__result_1__ua10000_2_0),
		mercury____Compare___io__result_1_0_i2,
		ENTRY(mercury____Compare___io__result_1_0));
Define_label(mercury____Compare___io__result_1_0_i2);
	update_prof_current_proc(LABEL(mercury____Compare___io__result_1_0));
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury____Index___io_io__result_1__ua10000_2_0),
		mercury____Compare___io__result_1_0_i3,
		ENTRY(mercury____Compare___io__result_1_0));
Define_label(mercury____Compare___io__result_1_0_i3);
	update_prof_current_proc(LABEL(mercury____Compare___io__result_1_0));
	if (((Integer) detstackvar(3) >= (Integer) r1))
		GOTO_LABEL(mercury____Compare___io__result_1_0_i4);
	r1 = ((Integer) 1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury____Compare___io__result_1_0_i4);
	if (((Integer) detstackvar(3) <= (Integer) r1))
		GOTO_LABEL(mercury____Compare___io__result_1_0_i6);
	r1 = ((Integer) 2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury____Compare___io__result_1_0_i6);
	r1 = (Integer) detstackvar(1);
	r2 = tag((Integer) r1);
	if (((Integer) r2 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury____Compare___io__result_1_0_i12);
	if (((Integer) detstackvar(2) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Compare___io__result_1_0_i9);
	r1 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury____Compare___io__result_1_0_i12);
	if (((Integer) r2 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury____Compare___io__result_1_0_i14);
	{
	Word tempr1;
	tempr1 = (Integer) detstackvar(2);
	if ((tag((Integer) tempr1) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury____Compare___io__result_1_0_i9);
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r3 = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 0));
	r1 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	{
	Declare_entry(mercury__compare_3_3);
	tailcall(ENTRY(mercury__compare_3_3),
		ENTRY(mercury____Compare___io__result_1_0));
	}
	}
Define_label(mercury____Compare___io__result_1_0_i14);
	r3 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	if ((tag((Integer) r3) != mktag(((Integer) 2))))
		GOTO_LABEL(mercury____Compare___io__result_1_0_i1000);
	r1 = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	r2 = (Integer) field(mktag(2), (Integer) r3, ((Integer) 0));
	{
	Declare_entry(mercury__builtin_compare_string_3_0);
	tailcall(ENTRY(mercury__builtin_compare_string_3_0),
		ENTRY(mercury____Compare___io__result_1_0));
	}
Define_label(mercury____Compare___io__result_1_0_i9);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	{
	Declare_entry(mercury__compare_error_0_0);
	tailcall(ENTRY(mercury__compare_error_0_0),
		ENTRY(mercury____Compare___io__result_1_0));
	}
Define_label(mercury____Compare___io__result_1_0_i1000);
	{
	Declare_entry(mercury__compare_error_0_0);
	tailcall(ENTRY(mercury__compare_error_0_0),
		ENTRY(mercury____Compare___io__result_1_0));
	}
END_MODULE

BEGIN_MODULE(mercury__io_module136)
	init_entry(mercury____Unify___io__read_result_1_0);
	init_label(mercury____Unify___io__read_result_1_0_i1011);
	init_label(mercury____Unify___io__read_result_1_0_i6);
	init_label(mercury____Unify___io__read_result_1_0_i1008);
	init_label(mercury____Unify___io__read_result_1_0_i1);
	init_label(mercury____Unify___io__read_result_1_0_i1010);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___io__read_result_1_0);
	if ((tag((Integer) r2) != mktag(((Integer) 0))))
		GOTO_LABEL(mercury____Unify___io__read_result_1_0_i1011);
	if (((Integer) r3 == (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Unify___io__read_result_1_0_i1008);
	r1 = FALSE;
	proceed();
Define_label(mercury____Unify___io__read_result_1_0_i1011);
	incr_sp_push_msg(1, "__Unify__");
	detstackvar(1) = (Integer) succip;
	if ((tag((Integer) r2) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury____Unify___io__read_result_1_0_i6);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if ((tag((Integer) r3) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury____Unify___io__read_result_1_0_i1010);
	r2 = (Integer) field(mktag(1), (Integer) r2, ((Integer) 0));
	r3 = (Integer) field(mktag(1), (Integer) r3, ((Integer) 0));
	{
	Declare_entry(mercury__unify_2_0);
	tailcall(ENTRY(mercury__unify_2_0),
		ENTRY(mercury____Unify___io__read_result_1_0));
	}
Define_label(mercury____Unify___io__read_result_1_0_i6);
	if ((tag((Integer) r3) != mktag(((Integer) 2))))
		GOTO_LABEL(mercury____Unify___io__read_result_1_0_i1);
	if ((strcmp((char *)(Integer) field(mktag(2), (Integer) r2, ((Integer) 0)), (char *)(Integer) field(mktag(2), (Integer) r3, ((Integer) 0))) !=0))
		GOTO_LABEL(mercury____Unify___io__read_result_1_0_i1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	if (((Integer) field(mktag(2), (Integer) r2, ((Integer) 1)) != (Integer) field(mktag(2), (Integer) r3, ((Integer) 1))))
		GOTO_LABEL(mercury____Unify___io__read_result_1_0_i1010);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___io__read_result_1_0_i1008);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___io__read_result_1_0_i1);
	r1 = FALSE;
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(1);
	decr_sp_pop_msg(1);
	proceed();
Define_label(mercury____Unify___io__read_result_1_0_i1010);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module137)
	init_entry(mercury____Index___io__read_result_1_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___io__read_result_1_0);
	r1 = (Integer) r2;
	tailcall(STATIC(mercury____Index___io_io__read_result_1__ua10000_2_0),
		ENTRY(mercury____Index___io__read_result_1_0));
END_MODULE

BEGIN_MODULE(mercury__io_module138)
	init_entry(mercury____Compare___io__read_result_1_0);
	init_label(mercury____Compare___io__read_result_1_0_i2);
	init_label(mercury____Compare___io__read_result_1_0_i3);
	init_label(mercury____Compare___io__read_result_1_0_i4);
	init_label(mercury____Compare___io__read_result_1_0_i6);
	init_label(mercury____Compare___io__read_result_1_0_i12);
	init_label(mercury____Compare___io__read_result_1_0_i14);
	init_label(mercury____Compare___io__read_result_1_0_i20);
	init_label(mercury____Compare___io__read_result_1_0_i19);
	init_label(mercury____Compare___io__read_result_1_0_i9);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___io__read_result_1_0);
	incr_sp_push_msg(5, "__Compare__");
	detstackvar(5) = (Integer) succip;
	detstackvar(1) = (Integer) r2;
	detstackvar(2) = (Integer) r3;
	detstackvar(4) = (Integer) r1;
	r1 = (Integer) r2;
	call_localret(STATIC(mercury____Index___io_io__read_result_1__ua10000_2_0),
		mercury____Compare___io__read_result_1_0_i2,
		ENTRY(mercury____Compare___io__read_result_1_0));
Define_label(mercury____Compare___io__read_result_1_0_i2);
	update_prof_current_proc(LABEL(mercury____Compare___io__read_result_1_0));
	detstackvar(3) = (Integer) r1;
	r1 = (Integer) detstackvar(2);
	call_localret(STATIC(mercury____Index___io_io__read_result_1__ua10000_2_0),
		mercury____Compare___io__read_result_1_0_i3,
		ENTRY(mercury____Compare___io__read_result_1_0));
Define_label(mercury____Compare___io__read_result_1_0_i3);
	update_prof_current_proc(LABEL(mercury____Compare___io__read_result_1_0));
	if (((Integer) detstackvar(3) >= (Integer) r1))
		GOTO_LABEL(mercury____Compare___io__read_result_1_0_i4);
	r1 = ((Integer) 1);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury____Compare___io__read_result_1_0_i4);
	if (((Integer) detstackvar(3) <= (Integer) r1))
		GOTO_LABEL(mercury____Compare___io__read_result_1_0_i6);
	r1 = ((Integer) 2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury____Compare___io__read_result_1_0_i6);
	r1 = (Integer) detstackvar(1);
	r2 = tag((Integer) r1);
	if (((Integer) r2 != mktag(((Integer) 0))))
		GOTO_LABEL(mercury____Compare___io__read_result_1_0_i12);
	if (((Integer) detstackvar(2) != (Integer) mkword(mktag(0), mkbody(((Integer) 0)))))
		GOTO_LABEL(mercury____Compare___io__read_result_1_0_i9);
	r1 = ((Integer) 0);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury____Compare___io__read_result_1_0_i12);
	if (((Integer) r2 != mktag(((Integer) 1))))
		GOTO_LABEL(mercury____Compare___io__read_result_1_0_i14);
	{
	Word tempr1;
	tempr1 = (Integer) detstackvar(2);
	if ((tag((Integer) tempr1) != mktag(((Integer) 1))))
		GOTO_LABEL(mercury____Compare___io__read_result_1_0_i9);
	r2 = (Integer) field(mktag(1), (Integer) r1, ((Integer) 0));
	r3 = (Integer) field(mktag(1), (Integer) tempr1, ((Integer) 0));
	r1 = (Integer) detstackvar(4);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	{
	Declare_entry(mercury__compare_3_3);
	tailcall(ENTRY(mercury__compare_3_3),
		ENTRY(mercury____Compare___io__read_result_1_0));
	}
	}
Define_label(mercury____Compare___io__read_result_1_0_i14);
	r3 = (Integer) detstackvar(2);
	if ((tag((Integer) r3) != mktag(((Integer) 2))))
		GOTO_LABEL(mercury____Compare___io__read_result_1_0_i9);
	r2 = (Integer) field(mktag(2), (Integer) r3, ((Integer) 0));
	detstackvar(1) = (Integer) field(mktag(2), (Integer) r1, ((Integer) 1));
	r1 = (Integer) field(mktag(2), (Integer) r1, ((Integer) 0));
	detstackvar(2) = (Integer) field(mktag(2), (Integer) r3, ((Integer) 1));
	{
	Declare_entry(mercury__builtin_compare_string_3_0);
	call_localret(ENTRY(mercury__builtin_compare_string_3_0),
		mercury____Compare___io__read_result_1_0_i20,
		ENTRY(mercury____Compare___io__read_result_1_0));
	}
Define_label(mercury____Compare___io__read_result_1_0_i20);
	update_prof_current_proc(LABEL(mercury____Compare___io__read_result_1_0));
	if (((Integer) r1 == ((Integer) 0)))
		GOTO_LABEL(mercury____Compare___io__read_result_1_0_i19);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	proceed();
Define_label(mercury____Compare___io__read_result_1_0_i19);
	r1 = (Integer) detstackvar(1);
	r2 = (Integer) detstackvar(2);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	{
	Declare_entry(mercury__builtin_compare_int_3_0);
	tailcall(ENTRY(mercury__builtin_compare_int_3_0),
		ENTRY(mercury____Compare___io__read_result_1_0));
	}
Define_label(mercury____Compare___io__read_result_1_0_i9);
	LVALUE_CAST(Word,succip) = (Integer) detstackvar(5);
	decr_sp_pop_msg(5);
	{
	Declare_entry(mercury__compare_error_0_0);
	tailcall(ENTRY(mercury__compare_error_0_0),
		ENTRY(mercury____Compare___io__read_result_1_0));
	}
END_MODULE

BEGIN_MODULE(mercury__io_module139)
	init_entry(mercury____Unify___io__error_0_0);
	init_label(mercury____Unify___io__error_0_0_i1);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___io__error_0_0);
	if ((strcmp((char *)(Integer) r1, (char *)(Integer) r2) !=0))
		GOTO_LABEL(mercury____Unify___io__error_0_0_i1);
	r1 = TRUE;
	proceed();
Define_label(mercury____Unify___io__error_0_0_i1);
	r1 = FALSE;
	proceed();
END_MODULE

BEGIN_MODULE(mercury__io_module140)
	init_entry(mercury____Index___io__error_0_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___io__error_0_0);
	{
	Declare_entry(mercury__builtin_index_string_2_0);
	tailcall(ENTRY(mercury__builtin_index_string_2_0),
		ENTRY(mercury____Index___io__error_0_0));
	}
END_MODULE

BEGIN_MODULE(mercury__io_module141)
	init_entry(mercury____Compare___io__error_0_0);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___io__error_0_0);
	{
	Declare_entry(mercury__builtin_compare_string_3_0);
	tailcall(ENTRY(mercury__builtin_compare_string_3_0),
		ENTRY(mercury____Compare___io__error_0_0));
	}
END_MODULE

BEGIN_MODULE(mercury__io_module142)
	init_entry(mercury____Unify___io__poly_type_0_0);
BEGIN_CODE

/* code for predicate '__Unify__'/2 in mode 0 */
Define_entry(mercury____Unify___io__poly_type_0_0);
	{
	Declare_entry(mercury____Unify___string__poly_type_0_0);
	tailcall(ENTRY(mercury____Unify___string__poly_type_0_0),
		ENTRY(mercury____Unify___io__poly_type_0_0));
	}
END_MODULE

BEGIN_MODULE(mercury__io_module143)
	init_entry(mercury____Index___io__poly_type_0_0);
BEGIN_CODE

/* code for predicate '__Index__'/2 in mode 0 */
Define_entry(mercury____Index___io__poly_type_0_0);
	{
	Declare_entry(mercury____Index___string__poly_type_0_0);
	tailcall(ENTRY(mercury____Index___string__poly_type_0_0),
		ENTRY(mercury____Index___io__poly_type_0_0));
	}
END_MODULE

BEGIN_MODULE(mercury__io_module144)
	init_entry(mercury____Compare___io__poly_type_0_0);
BEGIN_CODE

/* code for predicate '__Compare__'/3 in mode 0 */
Define_entry(mercury____Compare___io__poly_type_0_0);
	{
	Declare_entry(mercury____Compare___string__poly_type_0_0);
	tailcall(ENTRY(mercury____Compare___string__poly_type_0_0),
		ENTRY(mercury____Compare___io__poly_type_0_0));
	}
END_MODULE

#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

static void mercury__io_bunch_0(void)
{
	mercury__io_module0();
	mercury__io_module1();
	mercury__io_module2();
	mercury__io_module3();
	mercury__io_module4();
	mercury__io_module5();
	mercury__io_module6();
	mercury__io_module7();
	mercury__io_module8();
	mercury__io_module9();
	mercury__io_module10();
	mercury__io_module11();
	mercury__io_module12();
	mercury__io_module13();
	mercury__io_module14();
	mercury__io_module15();
	mercury__io_module16();
	mercury__io_module17();
	mercury__io_module18();
	mercury__io_module19();
	mercury__io_module20();
	mercury__io_module21();
	mercury__io_module22();
	mercury__io_module23();
	mercury__io_module24();
	mercury__io_module25();
	mercury__io_module26();
	mercury__io_module27();
	mercury__io_module28();
	mercury__io_module29();
	mercury__io_module30();
	mercury__io_module31();
	mercury__io_module32();
	mercury__io_module33();
	mercury__io_module34();
	mercury__io_module35();
	mercury__io_module36();
	mercury__io_module37();
	mercury__io_module38();
	mercury__io_module39();
	mercury__io_module40();
}

static void mercury__io_bunch_1(void)
{
	mercury__io_module41();
	mercury__io_module42();
	mercury__io_module43();
	mercury__io_module44();
	mercury__io_module45();
	mercury__io_module46();
	mercury__io_module47();
	mercury__io_module48();
	mercury__io_module49();
	mercury__io_module50();
	mercury__io_module51();
	mercury__io_module52();
	mercury__io_module53();
	mercury__io_module54();
	mercury__io_module55();
	mercury__io_module56();
	mercury__io_module57();
	mercury__io_module58();
	mercury__io_module59();
	mercury__io_module60();
	mercury__io_module61();
	mercury__io_module62();
	mercury__io_module63();
	mercury__io_module64();
	mercury__io_module65();
	mercury__io_module66();
	mercury__io_module67();
	mercury__io_module68();
	mercury__io_module69();
	mercury__io_module70();
	mercury__io_module71();
	mercury__io_module72();
	mercury__io_module73();
	mercury__io_module74();
	mercury__io_module75();
	mercury__io_module76();
	mercury__io_module77();
	mercury__io_module78();
	mercury__io_module79();
	mercury__io_module80();
	mercury__io_module81();
}

static void mercury__io_bunch_2(void)
{
	mercury__io_module82();
	mercury__io_module83();
	mercury__io_module84();
	mercury__io_module85();
	mercury__io_module86();
	mercury__io_module87();
	mercury__io_module88();
	mercury__io_module89();
	mercury__io_module90();
	mercury__io_module91();
	mercury__io_module92();
	mercury__io_module93();
	mercury__io_module94();
	mercury__io_module95();
	mercury__io_module96();
	mercury__io_module97();
	mercury__io_module98();
	mercury__io_module99();
	mercury__io_module100();
	mercury__io_module101();
	mercury__io_module102();
	mercury__io_module103();
	mercury__io_module104();
	mercury__io_module105();
	mercury__io_module106();
	mercury__io_module107();
	mercury__io_module108();
	mercury__io_module109();
	mercury__io_module110();
	mercury__io_module111();
	mercury__io_module112();
	mercury__io_module113();
	mercury__io_module114();
	mercury__io_module115();
	mercury__io_module116();
	mercury__io_module117();
	mercury__io_module118();
	mercury__io_module119();
	mercury__io_module120();
	mercury__io_module121();
	mercury__io_module122();
}

static void mercury__io_bunch_3(void)
{
	mercury__io_module123();
	mercury__io_module124();
	mercury__io_module125();
	mercury__io_module126();
	mercury__io_module127();
	mercury__io_module128();
	mercury__io_module129();
	mercury__io_module130();
	mercury__io_module131();
	mercury__io_module132();
	mercury__io_module133();
	mercury__io_module134();
	mercury__io_module135();
	mercury__io_module136();
	mercury__io_module137();
	mercury__io_module138();
	mercury__io_module139();
	mercury__io_module140();
	mercury__io_module141();
	mercury__io_module142();
	mercury__io_module143();
	mercury__io_module144();
}

#endif

void mercury__io__init(void); /* suppress gcc warning */
void mercury__io__init(void)
{
#if (defined(USE_GCC_NONLOCAL_GOTOS) && !defined(USE_ASM_LABELS)) \
	|| defined(PROFILE_CALLS) || defined(DEBUG_GOTOS) \
	|| defined(DEBUG_LABELS) || !defined(SPEED) \
	|| defined(NATIVE_GC) 

	mercury__io_bunch_0();
	mercury__io_bunch_1();
	mercury__io_bunch_2();
	mercury__io_bunch_3();
#endif
}
