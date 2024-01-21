extern void sc_clearent (struct ent *);
extern void sc_efree (register struct enode *);
extern void editv (int, int);
extern void editexp(int, int);
extern void edits (int, int);
extern void sc_eval_all (void);
extern char *sc_coltoa(int);
extern struct enode *sc_new_enode (int, struct enode *, struct enode *);
extern void sc_let (struct ent *, struct enode *);
extern void sc_slet (struct ent *, struct enode *, int);
extern void sc_hide_row (int);
extern void sc_hide_col (int);
extern void sc_copy (struct ent *, struct ent *, struct ent *, struct ent *);
extern void sc_eraser(struct ent *, struct ent *);
extern void sc_fill (struct ent *, struct ent *, double, double);
extern void sc_moveto(int, int);
extern void sc_num_search (double);
extern void sc_go_last (void);
extern void sc_set_iterations (int);
extern void sc_str_search (char *);










