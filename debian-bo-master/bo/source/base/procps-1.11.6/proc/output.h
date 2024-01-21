/*
 * some output routines for libproc
 * Copyright (C) 1996 Chuck Blake, Helmut Geyer. See COPYING for details
 *
 * some routines are derived from w, top and ps. please look there
 * for more copyright information.
 */
#ifndef _PROC_OUTPUT_H
#define _PROC_OUTPUT_H

#include <stdio.h>
#include <time.h>
/*
 * General convention: 
 * sprint_routine (char *s, output_type output,unsigned max, special sp);
 * s must be at least of Length max+2 and has to be allocated/defined
 * by the calling function.
 *
 * output routines are provided for the following output types:
 *  type     routine       special optim  what
 *                                 length
 * char *s  sprint_str        -      ?    print a string
 * char **s sprint_strlist   sep     ?    print an array of strings
 * int hex  sprint_hex        -       8   print an integer as Hex
 * int dec  sprint_dec        -      10   print an integer as Dec
 * int oct  sprint_oct        -      11   print an integer as Oct
 * int dec  sprint_bin        -      32   print an integer as Bin
 * int perc sprint_perc       -       4   print an integer (0<= i < 999)
 *                                        as 0.1*i%
 * int size sprint_size      mode     5   print a size in M or k or print
 *                                        number of pages (in dec)
 * time_t t sprint_time_ival centis   6   print a time interval
 * time_t t sprint_ltime                  print a time or a date
 *                                            (using localtime)
 * 
 * in this table optimal length is a worst case scenario, often much
 * shorter lengths suffice. all functions return the number of
 * characters printed. The calling function must make sure that the returned
 * string is aligned with the field length provided for it.
 * If a field length is too small to provide sensible data, nothing is
 * printed. Here are the minimal field lengths:
 *
 * sprint_perc sprint_time_ival sprint_time 
 *     2            3               
 *
 * If a value is too large to fit on the field length, a string
 * containing of '>'s is printed
 *
 * special notes:
 * sprint_time_ival and sprint_perc really have just two modes:
 *     output to 6 characters and to 3 characters length
 * sprint_size modi:
 *   OUT_SZ_PM : show number of pages.
 *   OUT_SZ_MK : show value in M or k depending on field length.
 *              value is already in k!
 */
int sprint_str(char *s, char *str, int max);
int sprint_strlist(char *s, char **strs, int max, char* sep);
int sprint_hex(char *s, unsigned long d, int max);
int sprint_dec(char *s, long d, int max);
int sprint_oct(char *s, unsigned long d, int max);
int sprint_bin(char *s, unsigned long d, int max);
int sprint_perc(char *s,unsigned int d, int max);
int sprint_size(char *s, long d, int max, int mode);
int sprint_time_ival(char *s, time_t t, int max, int centi_sec);
int sprint_time_old_ival(char *s, time_t t, int max);

int fprint_str(FILE *fp, char *str, int max);
int fprint_strlist(FILE *fp, char **strs, int max, char* sep);
int fprint_hex(FILE *fp, unsigned long d, int max);
int fprint_dec(FILE *fp, long d, int max);
int fprint_oct(FILE *fp, unsigned long d, int max);
int fprint_perc(FILE *fp,unsigned int d, int max);
int fprint_size(FILE *fp, long d, int max, int mode);
int fprint_time_ival(FILE *fp, time_t t, int max, int centi_sec);
int fprint_time_old_ival(FILE *fp, time_t t, int max);

#define print_str(A,B)       fprint_str(stdout,A,B)
#define print_strlist(A,B,C)   fprint_strlist(stdout,A,B,C)
#define print_hex(A,B)       fprint_hex(stdout,A,B)
#define print_dec(A,B)       fprint_dec(stdout,A,B)
#define print_oct(A,B)       fprint_oct(stdout,A,B)
#define print_perc(A,B)      fprint_perc(stdout,A,B)
#define print_size(A,B,C)      fprint_size(stdout,A,B,C)
#define print_time_ival(A,B,C) fprint_time_ival(stdout,A,B,C)
#define print_time_old_ival(A,B) fprint_time_old_ival(stdout,A,B)

#define OUT_SZ_PM 0
#define OUT_SZ_MK 1

extern int force_table_size;

#endif /* _PROC_OUTPUT_H */


