/*
 * Prototypes for functions in iconc.
 */
struct sig_lst   *add_sig    Params((struct code *sig, struct c_fnc *fnc));
novalue           addlib     Params((char *libname));
struct code      *alc_ary    Params((int n));
int               alc_cbufs  Params((int num, nodeptr lifetime));
int               alc_dtmp   Params((nodeptr lifetime));
int               alc_itmp   Params((nodeptr lifetime));
struct code      *alc_lbl    Params((char *desc, int flag));
int               alc_sbufs  Params((int num, nodeptr lifetime));
novalue           arth_anlz  Params((struct il_code *var1, struct il_code *var2,
                               int *maybe_int, int *maybe_dbl, int *chk1,
                               struct code **conv1p, int *chk2,
                               struct code **conv2p));
struct node      *aug_nd     Params((nodeptr op, nodeptr arg1, nodeptr arg2));
struct node      *binary_nd  Params((nodeptr op, nodeptr arg1, nodeptr arg2));
nodeptr           buildarray Params((nodeptr a, nodeptr lb, nodeptr e));
novalue           callc_add  Params((struct c_fnc *cont));
novalue           callo_add  Params((char *oper_nm, int ret_flag,
                               struct c_fnc *cont, int need_cont,
                               struct code *arglist, struct code *on_ret));
struct node      *case_nd    Params((nodeptr loc_model, nodeptr expr,
                               nodeptr cases));
int               ccomp      Params((char *srcname, char *exename));
novalue           cd_add     Params((struct code *cd));
struct val_loc   *chk_alc    Params((struct val_loc *rslt, nodeptr lifetime));
novalue           chkinv     Params((noargs));
novalue           chkstrinv  Params((noargs));
struct node      *c_str_leaf Params((int type,struct node *loc_model,
			       char *c));
novalue	          codegen    Params((struct node *t));
int               cond_anlz  Params((struct il_code *il, struct code **cdp));
novalue           const_blks Params((noargs));
struct val_loc   *cvar_loc   Params((char *name));
int               do_inlin   Params((struct implement *impl, nodeptr n,
                               int *sep_cont, struct op_symentry *symtab, int n_va));
novalue	          doiconx    Params((char *s));
struct val_loc   *dtmp_loc   Params((int n));
novalue           eval_arith Params((int indx1, int indx2, int *maybe_int,
                               int *maybe_dbl));
int               eval_cnv   Params((int typcd, int indx, int def,
                               int *cnv_flags));
int	eval_is	Params((int typcd,int indx));
novalue           findcases  Params((struct il_code *il, int has_dflt,
                               struct case_anlz *case_anlz));
novalue           fix_fncs   Params((struct c_fnc *fnc));
struct fentry    *flookup    Params((char *id));
novalue           gen_inlin  Params((struct il_code *il, struct val_loc *rslt,
                               struct code **scont_strt,
                               struct code **scont_fail, struct c_fnc *cont,
                               struct implement *impl, int nsyms,
                               struct op_symentry *symtab, nodeptr n,
                               int dcl_var, int n_va));
int	          getopr     Params((int ac, int *cc));
struct gentry    *glookup    Params((char *id));
novalue	          hsyserr    Params((char **av, char *file));
struct node      *i_str_leaf Params((int type,struct node *loc_model,char *c,
			       int d));
long              iconint    Params((char *image));
struct code      *il_copy    Params((struct il_c *dest, struct val_loc *src));
struct code      *il_cnv     Params((int typcd, struct il_code *src,
                               struct il_c *dflt, struct il_c *dest));
struct code      *il_dflt    Params((int typcd, struct il_code *src,
                               struct il_c *dflt, struct il_c *dest));
novalue           implproto  Params((struct implement *ip));
novalue           init       Params((noargs));
novalue           init_proc  Params((char *name));
novalue           init_rec   Params((char *name));
novalue           init_src   Params((noargs));
novalue           install    Params((char *name,int flag));
struct gentry    *instl_p    Params((char *name, int flag));
struct node      *int_leaf   Params((int type,struct node *loc_model,int c));
struct val_loc   *itmp_loc   Params((int n));
struct node      *invk_main  Params((struct pentry *main_proc));
struct node      *invk_nd    Params((struct node *loc_model, struct node *proc,
                              struct node *args));
novalue           invoc_grp  Params((char *grp));
novalue           invocbl    Params((nodeptr op, int arity));
struct node      *key_leaf   Params((nodeptr loc_model, char *keyname));
novalue           liveness   Params((nodeptr n, nodeptr resumer,
                              nodeptr *failer, int *gen));
struct node      *list_nd    Params((nodeptr loc_model, nodeptr args));
novalue           lnkdcl     Params((char *name));
novalue	          readdb     Params((char *db_name));
struct val_loc   *loc_cpy    Params((struct val_loc *loc, int mod_access));
novalue           mark_recs Params((struct fentry *fp, unsigned int *typ,
                              int *num_offsets, int *offset, int *bad_recs));
struct code      *mk_goto    Params((struct code *label));
struct node      *multiunary Params((char *op, nodeptr loc_model,
                               nodeptr oprnd));
struct sig_act   *new_sgact  Params((struct code *sig, struct code *cd,
                               struct sig_act *next));
int               nextchar   Params((noargs));
novalue           nfatal     Params((struct node *n, char *s1, char *s2));
int               n_arg_sym  Params((struct implement *ip));
novalue           outerfnc   Params((struct c_fnc *fnc));
int               past_prms  Params((struct node *n));
novalue           proccode   Params((struct pentry *proc));
novalue           prt_fnc    Params((struct c_fnc *fnc));
novalue           prt_frame  Params((char *prefix, int ntend, int n_itmp,
				int i, int j, int k));
struct centry    *putlit     Params((char *image,int littype,int len));
struct lentry    *putloc     Params((char *id,int id_type));
novalue	          quit       Params((char *msg));
novalue	          quitf      Params((char *msg,char *arg));
novalue           recconstr  Params((struct rentry *r));
novalue           resolve    Params((struct pentry *proc));
unsigned int      round2     Params((unsigned int n));
struct code      *sig_cd     Params((struct code *fail, struct c_fnc *fnc));
novalue           src_file   Params((char *name));
struct node      *sect_nd    Params((nodeptr op, nodeptr arg1, nodeptr arg2,
                               nodeptr arg3));
novalue           tfatal     Params((char *s1,char *s2));
struct node      *to_nd      Params((nodeptr loc_model, nodeptr arg1,
                               nodeptr arg2));
struct node      *toby_nd    Params((nodeptr loc_model, nodeptr arg1,
                               nodeptr arg2, nodeptr arg3));
int               trans      Params((noargs));
struct node      *tree1      Params((int type));
struct node      *tree2      Params((int type,struct node *loc_model));
struct node      *tree3      Params((int type,struct node *loc_model,
                               struct node *c));
struct node      *tree4      Params((int type, struct node *loc_model,
                               struct node *c, struct node *d));
struct node      *tree5      Params((int type, struct node *loc_model,
			       struct node *c, struct node *d,
			       struct node *e));
struct node      *tree6      Params((int type,struct node *loc_model,
			       struct node *c, struct node *d,
			       struct node *e, struct node *f));
novalue	          tsyserr    Params((char *s));
novalue	          twarn      Params((char *s1,char *s2));
struct code      *typ_chk    Params((struct il_code *var, int typcd));
int               type_case  Params((struct il_code *il, int (*fnc)(),
                               struct case_anlz *case_anlz));
novalue           typeinfer  Params((noargs));
struct node      *unary_nd   Params((nodeptr op, nodeptr arg));
novalue           var_dcls   Params((noargs));
int               varsubtyp  Params((unsigned int *typ,
                               struct lentry **single));
novalue	          writecheck Params((int rc));
novalue	          yyerror    Params((int tok,struct node *lval,int state));
int               yylex      Params((noargs));
int               yyparse    Params((noargs));
pointer		xmalloc	Params((long n));

#ifdef DeBug
novalue symdump Params((noargs));
novalue ldump   Params((struct lentry **lhash));
novalue gdump   Params((noargs));
novalue cdump   Params((noargs));
novalue fdump   Params((noargs));
novalue rdump   Params((noargs));
#endif					/* DeBug */

