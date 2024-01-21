/* This is part of tmview, a dvi previewer. (c) 1995 Thomas Moor         */
/*                                                                       */
/* This program may be used without any warranty. It may be modified and */
/* distributed without any restrictions.                                 */

void initfontdatabase(void);
void killfontdatabase(void);
void initdvi(void);
void killdvi(void);
int readpage(short,float,uchar,int);
short gotocccpage(double*,short);
const char* searchdvi(char* sstr, int* spage, long* spos);
