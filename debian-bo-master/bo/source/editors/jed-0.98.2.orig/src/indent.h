/*
 *  Copyright (c) 1995 John E. Davis  (davis@space.mit.edu)
 *  All Rights Reserved.
 */
#ifndef __JED_INDENT_H_
#define __JED_INDENT_H_

#if JED_HAS_DFA_SYNTAX
typedef struct Highlight Highlight;    /* opaque to all but dfasyntx.c */
extern void define_highlight_rule (char *, char *, char *);
extern void build_highlight_table (char *);
extern void enable_highlight_cache (char *, char *);
extern void jed_set_dfa_cache_dir (void);
#endif

typedef struct Syntax_Table_Type
{
   unsigned char string_char;	       /* character for string delim */
   unsigned char char_char;	       /* char for char delim */
   unsigned int flags;
#define SYNTAX_NOT_CASE_SENSITIVE 1
#define FORTRAN_TYPE 0x2
   /* means if non-digit in first column, it is a comment */
#define C_COMMENT_TYPE 0x4
   /* This means that a '* ' combination preceeded by whitespace marks the 
    * line as a comment line (like this one).
    */
#define TEX_LIKE_KEYWORDS 0x8


#define MULTICHAR_TYPE 0x0100
   /* More than one character form the comment delimeters */
#define EOL_COMMENT_TYPE 0x0200
   /* Comments start and stop on the same line, e.g., 
    * C++ (yuk), S-Lang, Postscript, Fortran, ...
    */
   unsigned char comment_beg;	       /* beginning of comment */
   unsigned char comment_beg2;	       /* second comment beginning char */
   unsigned char comment_end;	       /* end of comment */
   unsigned char comment_end2;	       /* second comment end char */
   unsigned char quote_char;	       /* used for quoting in strings */
   unsigned char preprocess;	       /* start preprocessor lines */
   unsigned short char_syntax[256];     /* syntax type for characters */
   unsigned char matching_delim[256];  /* matching pairs */
   char name[16];		       /* name of this table */
#define MAX_KEYWORD_LEN 20
#define MAX_KEYWORD_TABLES 3
   char *keywords[MAX_KEYWORD_TABLES][MAX_KEYWORD_LEN];
   struct Syntax_Table_Type *next;     /* pointer to next table */
#if JED_HAS_DFA_SYNTAX
   Highlight *hilite;
#endif
}
Syntax_Table_Type;


#define WORD_SYNTAX		0x001
#define NUMBER_SYNTAX		0x002
#define DELIM_SYNTAX		0x004

#define SYNTAX_MASK		0xFFF8
#define QUOTE_SYNTAX		0x008
#define STRING_SYNTAX		0x010
#define OPEN_DELIM_SYNTAX	0x020
#define CLOSE_DELIM_SYNTAX	0x040
#define COMMENT_SYNTAX		0x080
#define OP_SYNTAX		0x100
#define HTML_START_SYNTAX	0x200
#define HTML_END_SYNTAX		0x400

extern Syntax_Table_Type *Default_Syntax_Table;

extern int find_matching_delimiter (int *);
extern int parse_to_point (void);
extern void use_syntax_table (char *);
extern void create_syntax_table (char *);
extern void define_syntax (int *, char *);
extern void init_syntax_tables (void);
extern void blink_match(void);
extern int goto_match(void);
extern void set_syntax_flags (char *, int *);
extern void define_keywords (char *, char *, int *, int *);
extern Syntax_Table_Type *find_syntax_table (char *, int);

extern int map_color_object_to_number (char *);

#endif /* __JED_INDENT_H_ */
