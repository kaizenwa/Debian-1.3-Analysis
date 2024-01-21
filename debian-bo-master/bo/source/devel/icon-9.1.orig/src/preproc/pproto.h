novalue            addrmlst     Params((char *fname));
novalue            advance_tok  Params((struct token **tp));
int                chk_eq_sign  Params((noargs));
long               conditional  Params((struct token **tp,
                                  struct token *trigger));
struct token      *copy_t       Params((struct token *t));
novalue            err1         Params((char *s));
novalue            err2         Params((char *s1, char *s2));
novalue            errfl1       Params((char *f, int l, char *s));
novalue            errfl2       Params((char *f, int l, char *s1, char *s2));
novalue            errfl3       Params((char *f, int l, char *s1, char *s2,
                                   char *s3));
novalue            errt1        Params((struct token *t, char *s));
novalue            errt2        Params((struct token *t, char *s1, char *s2));
novalue            errt3        Params((struct token *t, char *s1, char *s2,
                                  char *s3));
int                eval         Params((struct token *trigger));
novalue            fill_cbuf    Params((noargs));
novalue            free_id_lst  Params((struct id_lst *ilst));
novalue            free_plsts   Params((struct paste_lsts *plsts));
novalue            free_m       Params((struct macro *m));
novalue            free_m_lst   Params((struct macro *m));
novalue            free_t       Params((struct token *t));
novalue            free_t_lst   Params((struct tok_lst *tlst));
struct str_buf    *get_sbuf     Params((noargs));
novalue            include      Params((struct token *trigger, char *fname,
                                  int start));
novalue	init_files	Params((char *opt_lst,char * *opt_args));
novalue	init_files	Params((char *opt_lst,char * *opt_args));
novalue            init_macro   Params((noargs));
novalue            init_preproc Params((char *fname, char *opt_lst,
                                  char **opt_args));
novalue            init_sys     Params((char *fname, int argc, char *argv[]));
novalue            init_tok     Params((noargs));
struct token      *interp_dir   Params((noargs));
struct token      *mac_tok      Params((noargs));
novalue            merge_whsp   Params((struct token **whsp,
                                  struct token **next_t,
                                  struct token *(*t_src)Params((noargs))));
novalue            m_delete     Params((struct token *mname));
novalue            m_install    Params((struct token *mname, int category,
                                  int multi_line, struct id_lst *prmlst,
                                  struct tok_lst *body));
struct macro      *m_lookup     Params((struct token *mname));
struct char_src   *new_cs       Params((char *fname, FILE *f, int bufsize));
struct id_lst     *new_id_lst   Params((char *id));
struct macro      *new_macro    Params((char *mname, int category,
                                  int multi_line, struct id_lst *prmlst,
                                  struct tok_lst *body));
struct mac_expand *new_me       Params((struct macro *m, struct tok_lst **args,
                                   struct tok_lst **exp_args));
struct paste_lsts *new_plsts    Params((struct token *trigger,
                                  struct tok_lst *tlst,
                                  struct paste_lsts *plst));
struct token      *new_token    Params((int id, char *image, char *fname,
                                  int line));
struct tok_lst    *new_t_lst    Params((struct token *tok));
struct token      *next_tok     Params((noargs));
novalue            nxt_non_wh   Params((struct token **tp));
novalue            output       Params((FILE *out_file));
struct token      *paste        Params((noargs));
novalue            pop_src      Params((noargs));
struct token      *preproc      Params((noargs));
novalue            push_src     Params((int flag, union src_ref *ref));
novalue            rel_sbuf     Params((struct str_buf *sbuf));
int                rt_state     Params((int tok_id));
novalue            show_usage   Params((noargs));
novalue            source       Params((char *fname));
novalue            str_src      Params((char *src_name, char *s, int len));
struct token      *tokenize     Params((noargs));
