novalue           add_dpnd  Params((struct srcfile *sfile, char *objname));
int               alloc_tnd Params((int typ, struct node *init, int lvl));
struct node      *arith_nd Params((struct token *tok, struct node *p1,
                             struct node *p2, struct node *c_int,
                             struct node *ci_act, struct node *intgr,
                             struct node *i_act, struct node *dbl,
                             struct node *d_act));
struct il_c      *bdy_prm   Params((int addr_of, int just_desc, struct sym_entry *sym, int may_mod));
int               c_walk    Params((struct node *n, int indent, int brace));
int               call_ret  Params((struct node *n));
struct token     *chk_exct  Params((struct token *tok));
novalue           chkabsret Params((struct token *tok, int ret_typ));
novalue           clr_def   Params((noargs));
novalue           clr_dpnd  Params((char *srcname));
novalue           clr_prmloc Params((noargs));
struct token     *cnv_to_id Params((struct token *t));
char             *cnv_name  Params((int typcd, struct node *dflt,
                              int *dflt_to_ptr));
struct node      *comp_nd   Params((struct token *tok, struct node *dcls,
                              struct node *stmts));
int               creat_obj Params((noargs));
novalue           d_lst_typ Params((struct node *dcls));
novalue           dclout    Params((struct node *n));
struct node      *dest_node Params((struct token *tok));
novalue           dst_alloc Params((struct node *cnv_typ, struct node *var));
novalue           dumpdb    Params((char *dbname));
novalue           fncout    Params((struct node *head, struct node *prm_dcl,
                              struct node *block));
novalue           force_nl  Params((int indent));
novalue           free_sym  Params((struct sym_entry *sym));
novalue           free_tree Params((struct node *n));
novalue           free_tend Params((noargs));
novalue           full_lst  Params((char *fname));
novalue           func_def  Params((struct node *dcltor));
novalue           id_def    Params((struct node *dcltor, struct node *x));
novalue           keepdir   Params((struct token *s));
int               icn_typ   Params((struct node *n));
struct il_c      *ilc_dcl   Params((struct node *tqual, struct node *dcltor,
                              struct node *init));
novalue           impl_fnc  Params((struct token *name));
novalue           impl_key  Params((struct token *name));
novalue           impl_op   Params((struct token *op_sym, struct token *name));
novalue           init_lex  Params((noargs));
novalue           init_sym  Params((noargs));
struct il_c      *inlin_c   Params((struct node *n, int may_mod));
novalue           in_line   Params((struct node *n));
novalue           just_type Params((struct node *typ, int indent, int ilc));
novalue           keyconst  Params((struct token *t));
struct node      *lbl       Params((struct token *t));
novalue           ld_prmloc Params((struct parminfo *parminfo));
novalue           loaddb    Params((char *db));
novalue           mrg_prmloc Params((struct parminfo *parminfo));
struct parminfo  *new_prmloc Params((noargs));
struct node      *node0     Params((int id, struct token *tok));
struct node      *node1     Params((int id, struct token *tok,
                              struct node *n1));
struct node      *node2     Params((int id, struct token *tok, struct node *n1,
                              struct node *n2));
struct node      *node3     Params((int id, struct token *tok, struct node *n1,
                              struct node *n2, struct node *n3));
struct node      *node4     Params((int id, struct token *tok, struct node *n1,
                              struct node *n2, struct node *n3,
                              struct node *n4));
struct il_c      *parm_dcl  Params((int addr_of, struct sym_entry *sym));
novalue	pop_cntxt	Params((noargs));
novalue           pop_lvl   Params((noargs));
novalue           prologue  Params((noargs));
novalue           prt_str   Params((char *s, int indent));
novalue		  ptout	    Params((struct token * x));
novalue	push_cntxt	Params((int lvl_incr));
novalue           push_lvl  Params((noargs));
novalue           put_c_fl  Params((char *fname, int keep));
novalue           defout    Params((struct node *n));
novalue           set_r_seq Params((long min, long max, int resume));
struct il_c      *simpl_dcl Params((char *tqual, int addr_of,
                              struct sym_entry *sym));
novalue           spcl_dcls Params((struct sym_entry *op_params));
struct srcfile   *src_lkup  Params((char *srcname));
novalue           strt_def  Params((noargs));
novalue           sv_prmloc Params((struct parminfo *parminfo));
struct sym_entry *sym_add   Params((int tok_id, char *image, int id_type,
                               int nest_lvl));
struct sym_entry *sym_lkup  Params((char *image));
struct node      *sym_node  Params((struct token *tok));
novalue           s_prm_def Params((struct token *u_ident,
                              struct token *d_ident));
novalue           tnd_char  Params((noargs));
novalue           tnd_strct Params((struct token *t));
novalue           tnd_union Params((struct token *t));
novalue           trans     Params((char *src_file));
long              ttol      Params((struct token *t));
char             *typ_name  Params((int typ, struct token *tok));
novalue           unuse     Params((struct init_tend *t_lst, int lvl));
novalue           var_args  Params((struct token *ident));
novalue           yyerror   Params((char *s));
int               yylex     Params((noargs));
int               yyparse   Params((noargs));
