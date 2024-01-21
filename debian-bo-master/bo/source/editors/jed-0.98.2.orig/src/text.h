/*
 *  Copyright (c) 1992, 1995 John E. Davis  (davis@space.mit.edu)
 *  All Rights Reserved.
 */
extern unsigned char *text_format_line(void);
extern void wrap_line(int);
extern int forward_paragraph(void);
extern void set_paragraph_separator (char *);
extern int backward_paragraph(void);
extern int text_format_paragraph(void);
extern int narrow_paragraph(void);
extern int center_line(void);
extern int text_smart_quote(void);
extern void define_word(char *);
extern void transform_region(int *);
extern void skip_word_chars(void);
extern void bskip_word_chars(void);
extern void skip_non_word_chars(void);
extern void bskip_non_word_chars(void);

extern char Jed_Word_Range[256];
