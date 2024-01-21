/*
 *  Copyright (c) 1992, 1995 John E. Davis  (davis@space.mit.edu)
 *  All Rights Reserved.
 */
    
/* For undo, these flags assumed to be <= 0xFF */
#define CDELETE  0x1
#define CINSERT  0x2
#define LDELETE  0x4
#define NLINSERT  0x8   /* causes marks to be moved to next line */
#define NLDELETE  0x10    /* opposite of above */
  
extern void update_marks(int, int);  
extern int No_Screen_Update;
extern int deln(int *);
extern void generic_deln(int *);
extern void del(void);
extern void ins( char);
extern void fast_del(void);
extern void fast_ins(char);
extern void insert_buffer(Buffer *);
extern void quick_insert(register unsigned char *, int);
extern void ins_chars(unsigned char *, int);
extern void ins_char_n_times(char, int);
extern void del_newline(void);
extern void replace_char(unsigned char);
extern int Suspend_Screen_Update;
