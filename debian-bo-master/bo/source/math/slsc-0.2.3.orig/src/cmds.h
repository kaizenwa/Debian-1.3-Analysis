extern void duprow (void);
extern void dupcol(void);
extern void insertrow (int);
extern void deleterow(int);
extern void insertcol(int);
extern void deletecol(int);
extern void rowvalueize (int);
extern void colvalueize(int);
extern void erase_area(int, int, int, int);
extern void valueize_area(int, int, int, int);
extern void pullcells (int);
extern void colshow_op(void);
extern void rowshow_op (void);
extern int get_rcqual (int);
extern void closerow (int);
extern void closerow (int);
extern void closecol (int);
extern void doend (int, int);
extern void doformat (int, int, int, int);
extern void printfile (char *, int, int, int, int);
extern void tblprintfile (char *, int, int, int, int);
extern void sync_refs (void);
extern void syncref(struct enode *);
extern void hiderow(int);
extern void hidecol (int);
extern void showrow(int, int);
extern void showcol (int c1, int c2);
extern FILE *openout (char *, int *);
extern void closeout(FILE *, int);
extern void copyent(register struct ent *, register struct ent *, int, int);
extern void write_fd (FILE *, int, int, int, int);
extern int writefile (char *, int, int, int, int);
extern void readfile (char *, int);
extern void erasedb (void);
extern void backcol (int);
extern void backrow (int);
extern void forwcol(int);
extern void forwrow (int);
void showstring (char *,	       /* to display */
		 int,		       /* or rightflush */
		 int,		       /* is there a numeric value? */
		 int, int,	       /* spreadsheet location */
		 int *,		       /* value returned through it */
		 int,		       /* last column displayed? */
		 int *,		       /* value returned through it */
		 int, int);	       /* screen row and column */
extern int sc_etype (register struct enode *);


extern void sc_erase_block (int, int, int, int);
extern void sc_adjust_maxrow_col (void);
extern int Sc_File_Line_Number;
