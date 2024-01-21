/* This is part of tmview, a dvi previewer. (c) 1995 Thomas Moor         */
/*                                                                       */
/* This program may be used without any warranty. It may be modified and */
/* distributed without any restrictions.                                 */

void htex_init(void);
void htex_kill(void);

int checkndoHyperTeX(char*,int,int);
void htex_recordbits(long x, long y, long w, long h); 
void htex_beginpage(pagelistelement*);
void htex_endpage(void);
void htex_scalebox(double,double);
void htex_findondonepage(char* href, int* ip, int* ia);
int  htex_findonpage(char* href, pagelistelement* page);
int  searchhref(char* href, int* spage, int* xpxl, int* ypxl);
int htex_clickmouse(pagelistelement*,int x, int y, char** res);
int htex_findnearanchor(pagelistelement* page,int *x, int *y);
int htex_findnumberanchor(pagelistelement* page,int* num);



