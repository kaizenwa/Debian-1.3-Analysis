#ifndef _SLRN_MISC_H
#define _SLRN_MISC_H

#include <stdio.h>
#include <slang.h>

#include "ttymsg.h"

extern void slrn_make_home_filename (char *, char *);
extern int slrn_make_home_dirname (char *, char *);
extern void slrn_redraw (void);
extern void slrn_update_screen (int);
extern int Slrn_Full_Screen_Update;
extern int slrn_get_yesno (int, char *, ...);
extern int slrn_get_yesno_cancel (char *str, ...);
extern void slrn_clear_message (void);
extern void slrn_print_percent (int, int, SLscroll_Window_Type *);
extern FILE *slrn_open_home_file (char *, char *, char *, int);
extern void slrn_suspend_cmd (void);
extern int slrn_read_integer (char *, int *, int *);
extern int slrn_read_input (char *, char *, char *, int);
extern int slrn_read_input_no_echo (char *, char *, char *, int);
extern void slrn_update_top_status_line (void);
extern void slrn_set_color (int);
extern char slrn_get_response (char *, char *str, ...);
extern int slrn_is_fqdn (char *);
extern int slrn_init_readline (void);
extern int slrn_check_batch (void);

extern unsigned char *slrn_regexp_match (SLRegexp_Type *, char *);
extern SLRegexp_Type *slrn_compile_regexp_pattern (char *);

#define MAX_HOST_NAME_LEN 256
typedef struct
{
   char *realname;
   char *username;
   char host[MAX_HOST_NAME_LEN];
   char *replyto;
   char *org;
   char *followup_string;
   char *signature;
   char *posting_host;		       /* FQDN or NULL */
   char *login_name;
}
Slrn_User_Info_Type;

extern Slrn_User_Info_Type Slrn_User_Info;
extern void slrn_get_user_info (void);
extern int slrn_edit_file (char *, char *, unsigned int);
extern  int slrn_mail_file (char *, int, unsigned int, char *, char *);

extern void slrn_article_help (void);
extern void slrn_group_help (void);
void slrn_set_input_string (char *);

extern int Slrn_Message_Present;
extern int Slrn_User_Wants_Confirmation;
extern void slrn_get_mouse_rc (int *, int *);
#ifndef VMS
extern char *Slrn_SendMail_Command;
#endif
extern SLKeyMap_List_Type *Slrn_RLine_Keymap;

extern FILE *slrn_popen (char *, char *);
extern int slrn_pclose (FILE *);


extern int Slrn_Use_Tmpdir;
extern FILE *slrn_open_tmpfile_in_dir (char *, char *, char *);
extern FILE *slrn_open_tmpfile (char *, char *);

extern int slrn_posix_system (char *, int);
extern char *Slrn_Editor;
extern char *Slrn_Editor_Post;
extern char *Slrn_Editor_Score;
extern char *Slrn_Editor_Mail;

extern void slrn_push_keymap (SLKeyMap_List_Type *);
extern void slrn_pop_keymap (void);
extern SLKeyMap_List_Type *Slrn_Current_Keymap;

#endif				       /* _SLRN_MISC_H */
