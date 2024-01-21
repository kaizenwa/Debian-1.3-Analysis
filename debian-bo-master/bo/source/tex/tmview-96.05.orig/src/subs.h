/* This is part of tmview, a dvi previewer. (c) 1995 Thomas Moor         */
/*                                                                       */
/* This program may be used without any warranty. It may be modified and */
/* distributed without any restrictions.                                 */


void pfprot(char*, ...);
void pfverb(char*, ...);
void allocmem(void*,long int);
void freemem(void*);
void alloclrumem(void*,long int);
void freelrumem(void*);
void touchlrumem(void* );
char *stralloccpy(char**, char*);
long scaled(long,long);
void alloc_bitmapbw(bitmap*);
void alloc_bitmapgs(bitmap*);
void clear_bitmap(bitmap*);
void print_bitmap(bitmap*);
void draw_frame(bitmap*);
int msearch(char*, char*, char*);
int completefile(char*, int);
int dvistandard(char**,char*);
void setworkdir(void);



