extern void sc_sync_ranges (void);
extern void sc_write_range (FILE *);
extern struct range *sc_find_range(char *, int, struct ent *, struct ent *);
extern int are_ranges (void);
extern void list_range(FILE *);
extern void clean_range (void);
extern void sc_add_range(char *, struct ent_ptr, struct ent_ptr, int);
extern void sc_del_range(struct ent *, struct ent *);




