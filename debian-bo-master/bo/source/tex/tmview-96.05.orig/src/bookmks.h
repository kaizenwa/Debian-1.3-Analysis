/* This is part of tmview, a dvi previewer. (c) 1995 Thomas Moor         */
/*                                                                       */
/* This program may be used without any warranty. It may be modified and */
/* distributed without any restrictions.                                 */


void filemkskill(void);
void filemksinit(void);
void rolldownfilemk(void);
void rollupfilemk(void);
void updatefilemk(void);
void freefilemkname(char*);             
void allocnewfilemk(char*);
void addbookmk(bookmarklist* bookmks, int doname);
void setbookmk(bookmark* thebmk, int doname);
int existsbookmkqm(bookmarklist* bookmks);
int amiatbookmkqm(bookmarklist* bookmks,int i);
void allocnewbookmk(bookmarklist* bookmks);
void freebookmknumber(bookmarklist* bookmks,int i);
void freebookmk(bookmarklist* bookmks);
int checkbookmk(bookmarklist* bookmks,int name);
int findrolldownbookmk(bookmarklist* bookmks,int name);




