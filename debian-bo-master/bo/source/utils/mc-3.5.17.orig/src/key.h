#ifndef __KEY_H
#define __KEY_H

void init_key (void);
int mi_getch (void);
int get_event (Gpm_Event *event, int redo_event);
int is_idle (void);
int ctrl_pressed ();

/* Used to get the modifier information          */
/* Currently, it just works on the Linux console */
#define SHIFT_PRESSED 1
#define ALTR_PRESSED 2
#define CONTROL_PRESSED 4
#define ALTL_PRESSED 8
int get_modifier ();

extern int double_click_speed;
extern int old_esc_mode;
extern int irix_fn_keys;
extern int use_8th_bit_as_meta;

/* While waiting for input, the program can select on more than one file */

typedef int (*select_fn)(int fd, void *info);

/* Channel manipulation */
void add_select_channel    (int fd, select_fn callback, void *info);
void remove_select_channel (int fd);

/* Activate/deactivate the channel checking */
void channels_up (void);
void channels_down (void);

/* Abort/Quit chars */
int is_abort_char (int c);
int is_quit_char (int c);

#define XCTRL(x) ((x) & 31)
#define ALT(x) (0x200 | (x))

/* To define sequences and return codes */
#define MCKEY_NOACTION	0
#define MCKEY_ESCAPE	1
void do_define_key (int code, char *strcap);
void define_sequence (int code, char *seq, int action);

/* Learn a single key */
char *learn_key (void);

/* Returns a key code (interpreted) */
int get_key_code (int nodelay);

typedef struct {
    int code;
    char *name;
    char *longname;
} key_code_name_t;

extern key_code_name_t key_name_conv_tab [];
extern int we_are_background;
#endif	/* __KEY_H */
