#include "melib.h"

#define PUBLIC 

/* curses.c */

extern int ReadCh            P_((int));

/* args.c */

extern char * parse_arguments P_((int, char *[], char *, int *));

/* init.c */
void initialize P_((char *, int));

/* elm.c */
extern void motion P_((int));

/* in_utils.c */

extern int want_to          P_((char *, int, int, int));
extern int read_number      P_((int, char *));
extern int optionally_enter P_((char *, int, int, int, int));
extern int pattern_enter    P_((char *, char *, int, int, char *, int, int));
extern int GetPrompt        P_((void));

/* out_utils.c */

extern void sleep_message P_((void));

/* addr_util.c */

int build_address P_((char *, char *, int, int ));

/* Alias.c */

extern void alias P_((void));

/* file.c */

int expand_filename P_((char *, int, int));

/* fileutil.c */

extern FILE *open_end_update P_((char *));

/* src/remailer.c */

#ifdef USE_REMAILER
#ifdef MIME
extern int remailer_copy_message_across P_((FILE *, FILE *, int, mime_send_t *));
#else
extern int remailer_copy_message_across P_((FILE *, FILE *, int));
#endif
extern int remailer_proc P_((void));
#endif 

#ifdef USE_PGP
/* pgp.c */
extern int pgp_encrypt			P_((char *, char *, char *, int, int));
extern int pgp_menu			P_((char *));
extern int pgp_extract_public_key	P_((void));
#endif

/* read_rc.c */

extern int init_defaults        P_((void));
extern int read_rc_file         P_((void));

extern int metapager		P_((FILE *, struct header_rec *, int));
extern int builtinplusplus	P_((FILE *, long, int, char **, int));

/* mime.c */

extern int have_metamail        P_((void));

/* strings.c */

extern char **argv_from_to      P_((char *to));

/* fileio.h */
extern void copy_message P_((FILE *, struct header_rec *,
			     char *, FILE *, int));

extern void copy_plain P_((char *,FILE *,int, struct header_rec *, FILE *));
#ifdef MIME
extern void copy_mime P_((char *,FILE *,int, struct header_rec *, FILE *));
#endif

typedef void copy_decoder P_((char *,FILE *,int, struct header_rec *, FILE *));
typedef copy_decoder *copy_decoder_t;
extern copy_decoder_t select_copy_decoder P_((struct header_rec *));

/* newmbox.c */

extern void header_zero P_((struct header_rec *));
extern void header_clear P_((struct header_rec *));

#ifdef MIME
extern int is_pre_mime_content_type P_((mime_t *,char *));
#endif /* MIME */

/* file_util.c */

extern long fsize P_((FILE *));
extern long bytes P_((char *));

/* mailmsg1.c */

int send_msg            P_((char *, char *, char *, int, int));
int copy_the_msg        P_((int *,int));
int get_to P_((char *, char *, int, int)); 

/* mailmsg2.c */

int mail P_((struct header_rec *, int, int));
int mail_form P_((struct header_rec *, char *, char *));

/* syscall.c */
#define FDVEC_TO_PRG       1
#define FDVEC_DEFINE       2
#define FDVEC_STDIN        4
#define FDVEC_STDOUT       8
#define FDVEC_STDERR      16
#define FDVEC_FILE        32
#define FDVEC_END         -1

struct fdvec {
  int fd;
  char * name;
  int flag;

  /* used only internally: */
  int fd2;
};

extern int system_call    P_((char *, int));

struct run_state {
  int pid;
  int errno;
  int raw;
  int options;
};

extern int start_run        P_((struct run_state *rs, int options,
				char * argv[], int infd, int outfd));
extern int run_already_done P_((struct run_state *rs, int *exit_code));
extern int wait_end         P_((struct run_state *rs, int *exit_code));
extern char ** join_argv    P_((char * argv1[], char * argv2[]));

/* lib/errno.c */

extern char *error_description P_((int));

/* savecopy.c */

extern int name_copy_file P_((char *, int));

#ifdef MIME


extern int save_copy		P_((char *, char *, char *, char *, 
				       char *, int, mime_send_t *));
extern int append_copy_to_file	P_((char *, char *, char *, char *, 
				       char *, int, mime_send_t *));
extern FILE *write_header_info	P_((char *, char *, char *, char *, 
				       int, int, mime_send_t *));
extern int copy_message_across	P_((FILE *, FILE *, int, mime_send_t *));

/* mime.c */

extern int check_for_multipart	P_((FILE *, mime_send_t *));
extern int Include_Part		P_((FILE *, char *, int, mime_send_t *, int));
extern int needs_encoding       P_((FILE *));

/* mime_encode.c */

extern void attach_generate_message P_((mime_t *, FILE *, int, mime_send_t *));
extern void base64_encode	    P_((FILE *, FILE *, int, mime_send_t *));
extern void line_quoted_printable_encode P_((char *, FILE *, int, int,
					       int, mime_send_t *));
extern void quoted_printable_encode	P_((FILE *, FILE *, int, int,
						mime_send_t *));
extern int is_text_type			P_((char *, char *, int));
extern char *mime_generate_boundary	P_((char *));
extern void add_parameter		P_((char *,char *,char *,int, int));
extern void add_parameter_t             P_((mime_t *, char *, char *, int));

extern void print_EOLN			P_((FILE *,int));
extern int update_encoding		P_((int *,int));
extern void write_encoded   P_((FILE *, FILE *, int, int, int, mime_send_t *));
extern void mime_write_header		P_((FILE *, mime_send_t *, int));
extern void rfc1522_encode_text         P_((char *,int,char *,int));

/* attach_menu.c */

extern mime_t *attach_menu		P_((mime_t *, int));
extern int Attach_it                    P_((char *));
#endif

/* returnadd.c */
extern int get_return                   P_((char *, int, int));







