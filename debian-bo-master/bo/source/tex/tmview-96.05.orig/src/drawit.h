/* This is part of tmview, a dvi previewer. (c) 1995 Thomas Moor         */
/*                                                                       */
/* This program may be used without any warranty. It may be modified and */
/* distributed without any restrictions.                                 */

drawlistdata* drawlistadd(selfdrawfunction* sdf);
void drawlistclear(void);
void updateall(void);
void updatelist(void);
void drawpage(int,int,int,int);
void drawupdatepage(void);     
void drawshowallmarks(void);
void drawshowfixedmarksonly(void);     
void drawhideallmarks(void); 
void drawhidefixedmarksonly(void);
void drawshowrect(void); 
void drawhiderect(void); 
     
void drawstatusline(void);
void drawstatuspage(void);
void drawstatusmarks(void);
void drawstatusarg(void);
void drawstatuszoom(void);

void drawsr(const char*, int,int,int);
void drawsl(const char*, int,int,int);
void drawsn(const char*, int,int);
void drawcharbmp(drawlistdata* arg);
void drawcharbmp2bw(drawlistdata* arg);  
void drawcharbmp2gs(drawlistdata* arg);
void drawrulebg(drawlistdata* arg);  
void drawruleor(drawlistdata* arg);

