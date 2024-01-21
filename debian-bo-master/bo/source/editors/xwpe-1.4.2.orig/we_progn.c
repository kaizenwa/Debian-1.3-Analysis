/* we_progn.c                                             */
/* Copyright (C) 1993 Fred Kruse                          */
/* This is free software; you can redistribute it and/or  */
/* modify it under the terms of the                       */
/* GNU General Public License, see the file COPYING.      */

#include "edit.h"

#undef TESTSDEF

#ifdef PROG
#include "makro.h"
#include "xkeys.h"
#undef exit

extern struct dirfile *e_p_get_var();
extern char *e_p_msg[];

unsigned char *e_resv_c[] = {  "auto" , "break", "case", "char",
		      "const", "continue", "default", "do", "double",
		      "else", "enum", "extern", "float", "for",
		      "goto", "if", "int", "long", "register",
		      "return", "short",
		      "signed", "sizeof", "static", "struct", "switch",
		      "this", "typedef", "union", "unsigned",
		      "void", "volatile", "while", NULL
		   } ;
unsigned char *e_resv_cc[] = {  "auto" , "break", "case", "cerr", "char", "cin",
		       "class", "const", "continue", "cout",
		       "default", "delete", "do", "double",
		       "else", "enum", "extern", "float", "for", "friend",
		       "goto",
		       "if", "inline", "int", "long", "new", "operator",
		       "private", "protected", "public", "register",
		       "return", "short",
		       "signed", "sizeof", "static", "struct", "switch",
		       "template", "this", "typedef", "union", "unsigned",
		       "virtual", "void", "volatile", "while", NULL
		    } ;

unsigned char *e_opr_f77[] = {  ".AND.", ".EQ.", ".EQV.", ".FALSE.", ".GE.",
		       ".GT.", ".LE.", ".LT.", ".NE.", ".NEQV.",
		       ".NOT.", ".OR.", ".TRUE.", NULL
		    };
unsigned char *e_resv_f77[] = {  "ACCESS" , "ASSIGN", "BACKSPACE", "BLANK",
			"BLOCKDATA", "CALL", "CHARACTER", "CLOSE",
			"COMMON", "COMPLEX", "CONTINUE", "DATA",
			"DIMENSION", "DIRECT", "DO", "DOUBLE PECISION",
			"ELSE", "END", "ENDFILE", "ENTRY", "EQUIVALENCE",
			"ERR", "EXIST", "EXTERNAL", "FILE", "FMT", "FORM",
			"FORMAT", "FORMATTED", "FUNCTION", "GOTO", "IF",
			"IMPLICIT", "INQUIRE", "INTEGER", "INTRINSIC",
			"IOSTAT", "LOGICAL", "NAME", "NAMED", "NEXTREC",
			"NUMBER", "OPEN", "OPENED", "PARAMETER", "PAUSE",
			"PRINT", "PROGRAM", "READ", "REAL", "REC", "RECL",
			"RETURN", "REWIND", "SAVE", "SEQUENTIAL", "STATUS",
			"STOP", "SUBROUTINE", "TO", "UNFORMATED", "UNIT",
			"WRITE", NULL
		     } ;

struct col_sw c_col_st = {  e_resv_c, NULL, "()[]{}<>+-/*%=|&!.?:,;",
			    "/*", "*/", "", "",
			    '\"', '\'', '#', '\\', '\\', '\0', '\0', '\0',
				-1, -1, 1000
			 };
struct col_sw cc_col_st = {  e_resv_cc, NULL, "()[]{}<>+-/*%=|&!.?:,;",
			     "/*", "*/", "//", "",
			     '\"', '\'', '#', '\\', '\\', '\0', '\0', '\0',
				-1, -1, 1000
			  };
struct col_sw f_col_st = {  e_resv_f77, e_opr_f77, "()+-/*=$.:,",
			    "", "", "!", "C*",
			    '\'', '\0', '\0', '\0', '\0', '\0', '\0', 1,
			    0, 5, 72
			 };

typedef struct {  char *ext;  COLSW *cs;  }  ALLCSW;
ALLCSW **e_p_acs = NULL;
int e_p_acn = 4, b_dif = 3;

#define FRB1 s->fb->cr.fb  /*  Schluesselworte etc  */
#define FRB2 s->fb->ck.fb  /*  Konstanten           */
#define FRB3 s->fb->cp.fb  /*  Pre-Prozessor A.     */
#define FRB4 s->fb->cc.fb  /*  Kommentare           */
#define FRB5 s->fb->ct.fb  /*  Text                 */

#define iscase(s) ( (!strncmp(s, "case", 4) && !isalnum1(*(s+4)))	      \
		 || (!strncmp(s, "default", 7) && !isalnum1(*(s+7))) )
#define isstatus(s) ( !strncmp(s, "private:", 8) || !strncmp(s, "public:", 7) \
                 || !strncmp(s, "protected:", 10) )
#define ispif(s) ( !strncmp(s, "#if", 3) || !strncmp(s, "#ifdef", 6)  \
			|| !strncmp(s, "#ifndef", 7) )
#define ispelse(s) ( !strncmp(s, "#else", 5) || !strncmp(s, "#elif", 5) )
#define ispendif(s) ( !strncmp(s, "#endif", 6) )
#define iscop(c) ( c == '<' || c == '>' || c == '+' || c == '-' ||    \
		   c == '/' || c == '*' || c == '%' || c == '=' ||    \
		   c == '|' || c == '&' || c == '!' || c == ':' ||    \
		   c == '?' || c == '.' )

#define e_mk_col(str, n, frb, cs, n_nd, n_bg)				\
{									\
   if(n == cs->i_cm) 							\
   {  int ii;								\
      for(ii = 0; cs->cm_ch[ii] && 					\
		cs->cm_ch[ii] != toupper(str[n]); ii++);		\
      if(cs->cm_ch[ii]) mcsw = 7;					\
   }									\
   if(mcsw == 6 && (isalnum(str[n]) || str[n] == '_')) frb = FRB1;	\
   else if(mcsw == 7) frb = FRB4;					\
   else if(mcsw == 3 && (isalnum(str[n]) || str[n] == '.')) frb = FRB2;	\
   else if(mcsw == 5)							\
   {	if(str[n] == cs->cm_bg[0])					\
	{  int jj;							\
	   for(jj = 1; cs->cm_bg[jj]					\
			&& str[n+jj] == cs->cm_bg[jj]; jj++);		\
	   if(!cs->cm_bg[jj]) 						\
           {  mcsw = 4;  n_bg = n+jj-1;  frb = FRB4;  }			\
	}								\
	if(mcsw == 5 && str[n] == cs->cm_te[0])				\
	{  int jj;							\
	   for(jj = 1; cs->cm_te[jj]					\
			&& str[n+jj] == cs->cm_te[jj]; jj++);		\
	   if(!cs->cm_te[jj]) {  mcsw = 7;  frb = FRB4;  }		\
	}								\
	if(mcsw == 5) frb = FRB3;					\
   }									\
   else if(mcsw == 1)							\
   {  	if(str[n] == cs->c_str && !bssw) mcsw = 0;			\
	frb = FRB2;							\
   }									\
   else if(mcsw == 2)							\
   {  	if(str[n] == cs->c_c && !bssw) mcsw = 0;  			\
	frb = FRB2;							\
   }									\
   else if(mcsw == 4)							\
   {	if(n_nd < n-n_bg && str[n] == cs->cm_nd[n_nd])			\
	{  int jj;							\
	   for(jj = 1; jj <= n_nd && n-jj >= 0				\
			&& str[n-jj] == cs->cm_nd[n_nd-jj]; jj++);	\
	   if(jj > n_nd) mcsw = svmsw;					\
	}								\
	frb = FRB4;							\
   }									\
   else									\
   {    if(n >= cs->i_af) {  mcsw = 7;  frb = FRB4;  }			\
	else if(isdigit(str[n]))					\
	{  if(n == 0 || (!isalnum(str[n-1]) && str[n-1] != '_'))	\
	   {  mcsw = 3;  frb = FRB2;  }					\
	   else frb = FRB5;						\
	}								\
	else if(isalpha(str[n]))					\
        {  if(cs->to_u && (n == 0 					\
			|| (!isalnum(str[n-1]) && str[n-1] != '_')))	\
	   {  int ii, jj;						\
	      for(ii = 0; cs->res_wrd[ii] 				\
			&& cs->res_wrd[ii][0] < toupper(str[n]); ii++); \
	      for( ; cs->res_wrd[ii]	 				\
			&& cs->res_wrd[ii][0] == toupper(str[n]); ii++)	\
	      {  for(jj = 0; cs->res_wrd[ii][jj]			\
		   && cs->res_wrd[ii][jj] == toupper(str[n+jj]); jj++);	\
	           if(!cs->res_wrd[ii][jj] && !isalnum(str[n+jj]) 	\
			&& str[n+jj] != '_')				\
	           {  mcsw = 6;  frb = FRB1;  break;  }			\
	      }								\
	   }								\
	   else if(!cs->to_u && (n == 0 					\
			|| (!isalnum(str[n-1]) && str[n-1] != '_')))	\
	   {  int ii, jj;						\
	      for(ii = 0; cs->res_wrd[ii] 				\
			&& cs->res_wrd[ii][0] < str[n]; ii++);		\
	      for( ; cs->res_wrd[ii]	 				\
			&& cs->res_wrd[ii][0] == str[n]; ii++)		\
	      {  for(jj = 0; cs->res_wrd[ii][jj]			\
		   && cs->res_wrd[ii][jj] == str[n+jj]; jj++);		\
	           if(!cs->res_wrd[ii][jj] && !isalnum(str[n+jj]) 	\
			&& str[n+jj] != '_')				\
	           {  mcsw = 6;  frb = FRB1;  break;  }			\
	      }								\
	   }								\
	   if(!mcsw) frb = FRB5;					\
	}								\
	else if(isspace(str[n]))					\
	{  mcsw = 0;  frb = FRB5;  }					\
	else 								\
	{  if(str[n] == cs->c_str)					\
	   {  mcsw = 1;  frb = FRB2;  }					\
	   else if(str[n] == cs->c_c)					\
   	   {  mcsw = 2;  frb = FRB2;  }					\
	   else if(str[n] == cs->c_pr)					\
	   {  svmsw = mcsw = 5;  frb = FRB3;  }				\
	   else								\
	   {	mcsw = 0;						\
		if(str[n] == cs->cm_bg[0])				\
	   	{  int jj;						\
	   	   for(jj = 1; cs->cm_bg[jj]				\
			&& str[n+jj] == cs->cm_bg[jj]; jj++);		\
	   	   if(!cs->cm_bg[jj]) 					\
		   {  mcsw = 4;  n_bg = n+jj-1;  frb = FRB4;  }		\
	   	}							\
	   	if(!mcsw && str[n] == cs->cm_te[0])			\
	   	{  int jj;						\
		   for(jj = 1; cs->cm_te[jj]				\
			&& str[n+jj] == cs->cm_te[jj]; jj++);		\
		   if(!cs->cm_te[jj]) {  mcsw = 7;  frb = FRB4;  }	\
		}							\
	   	if(cs->opr && !mcsw)					\
                { if(cs->to_u)						\
	          { int ii, jj;						\
	            for(ii = 0; cs->opr[ii] 				\
			&& cs->opr[ii][0] < str[n]; ii++);		\
	            for( ; cs->opr[ii]		 			\
			&& cs->opr[ii][0] == str[n]; ii++)		\
	            { for(jj = 0; cs->opr[ii][jj]			\
		       && cs->opr[ii][jj] == toupper(str[n+jj]); jj++);	\
	               if(!cs->opr[ii][jj])			 	\
	               {  mcsw = 6;  frb = FRB1;  break;  }		\
	      	    }							\
	          }							\
	   	  else							\
	          { int ii, jj;						\
	            for(ii = 0; cs->opr[ii] 				\
			&& cs->opr[ii][0] < str[n]; ii++);		\
	            for( ; cs->opr[ii]		 			\
			&& cs->opr[ii][0] == str[n]; ii++)		\
	            { for(jj = 0; cs->opr[ii][jj]			\
		       && cs->opr[ii][jj] == str[n+jj]; jj++);		\
	               if(!cs->opr[ii][jj])			 	\
	               {  mcsw = 6;  frb = FRB1;  break;  }		\
	      	    }							\
	          }							\
		}							\
	   	if(!mcsw)						\
		{  int jj;						\
	 	   for(jj = 0; cs->no_alnum[jj]				\
			&& str[n] != cs->no_alnum[jj]; jj++);		\
	  	   if(!cs->no_alnum[jj]) frb = FRB5;			\
	  	   else frb = FRB1;					\
		}							\
	   }								\
	}								\
   }									\
   if(str[n] == cs->c_nc) bssw = !bssw;					\
   else bssw = 0;							\
}

/*
	else if((str[n] == '-' || str[n] == '+') && isdigit(str[n+1]))	\
	{  mcsw = 3;  frb = FRB2;  }					\
*/

int e_scfbol(n, mcsw, str, cs)
     int n;
     int mcsw;
     unsigned char *str;
     COLSW *cs;
{
   int bssw = 0, svmsw = 0, nla = 0, i, j;
   for(i = 0; i < n && isspace(str[i]); i++);
   if(mcsw == 5) svmsw = 5;
   else if(mcsw == 0 && str[i] == cs->c_pr) {  svmsw = mcsw = 5;  i++;  }
   for(; i < n; i++)
   {  if(str[i] == cs->c_na) nla = 1;
      if(i == cs->i_cm) return(0);
      else if(mcsw == 4)
      {  if(str[i] == cs->cm_nd[0])
	 {  for(j = 1; cs->cm_nd[j] && str[i+j] == cs->cm_nd[j]; j++);
	    if(!cs->cm_nd[j]) {  i += (j-1);  mcsw = svmsw;  }
	 }
      }
      else if(mcsw == 1)
      {  if(str[i] == cs->c_str && !bssw) mcsw = svmsw;  }
      else if(str[i] == cs->c_str && !bssw) mcsw = 1;
      else
      {  if(str[i] == cs->cm_te[0])
	 {  for(j = 1; cs->cm_te[j] && str[i+j] == cs->cm_te[j]; j++);
	    if(!cs->cm_te[j]) return(0);
	 }
	 if(str[i] == cs->cm_bg[0])
	 {  for(j = 1; cs->cm_bg[j] && str[i+j] == cs->cm_bg[j]; j++);
	    if(!cs->cm_bg[j]) mcsw = 4;
	    i += (j-1);
	 }
      }
      if(str[i] == cs->c_nc) bssw = !bssw;
      else bssw = 0;
   }
   return((mcsw == 4 || (bssw && n > 0 && str[n-1] == cs->c_nl)
		     || nla ||  cs->i_nl >= 0 ) ? mcsw : 0);
}

int e_sc_all(f, sw)
     FENSTER *f;
     int sw;
{
   int i;
   if((e_we_sw & 3) != 3) return(0);
   if(!sw)
   {  for(i = 0; i <= f->ed->mxedt; i++)
      if(f->ed->f[i]->c_sw)
      {  FREE(f->ed->f[i]->c_sw);
	 f->ed->f[i]->c_sw = NULL;
      }
   }
   else
   {  for(i = 0; i <= f->ed->mxedt; i++)
      {  if(f->ed->f[i]->c_sw) FREE(f->ed->f[i]->c_sw);
	 e_add_synt_tl(f->ed->f[i]->datnam, f->ed->f[i]);
	 if(f->ed->f[i]->c_st)
	 {  if(f->ed->f[i]->c_sw) FREE(f->ed->f[i]->c_sw);
	    f->ed->f[i]->c_sw = e_sc_txt(NULL, f->ed->f[i]->b);
	 }
      }
   }
   e_rep_win_tree(f->ed);
   return(0);
}

int e_program_opt(f)
     FENSTER *f;
{
   int ret, sw = f->ed->edopt & 8 ? 1 : 0;
   W_OPTSTR *o = e_init_opt_kst(f);
   if(!o) return(-1);
   o->xa = 17;  o->ya = 4;  o->xe = 59;  o->ye = 18;
   o->bgsw = AltO;
   o->name = "Programming-Options";
   o->crsw = AltO;
   e_add_txtstr(21, 2, "Screen:", o);
   e_add_txtstr(4, 2, "Stop at", o);
   e_add_sswstr(5, 3, 0, AltE, f->ed->edopt & 16 ? 1 : 0, "Errors  ", o);
   e_add_sswstr(5, 4, 0, AltM, f->ed->edopt & 32 ? 1 : 0, "Messages", o);
   e_add_sswstr(22, 3, 0, AltD, f->ed->edopt & 8 ? 1 : 0, "Diff. Colors", o);
   e_add_wrstr(4, 6, 4, 7, 35, 128, 1, AltX, "EXecution-Directory:",
						e_prog.exedir, NULL, o);
   e_add_wrstr(4, 9, 4, 10, 35, 128, 0, AltS, "System-Include-Path:",
					      e_prog.sys_include, NULL, o);
   e_add_bttstr(10, 12, 1, AltO, " Ok ", NULL, o);
   e_add_bttstr(26, 12, -1, ESC, "Cancel", NULL, o);
   ret = e_opt_kst(o);
   if(ret != ESC)
   {  e_prog.exedir = e_make_string(e_prog.exedir, o->wstr[0]->txt);
      e_prog.sys_include = e_make_string(e_prog.sys_include, o->wstr[1]->txt);
      f->ed->edopt = (f->ed->edopt & ~56) + (o->sstr[0]->num ? 16 : 0)
	+ (o->sstr[1]->num ? 32 : 0) + (o->sstr[2]->num ? 8 : 0);
      if(sw != o->sstr[2]->num) e_sc_all(f, o->sstr[2]->num);
   }
   freeostr(o);
   return(0);
}

int e_sc_nw_txt(y, b, sw)
     int y;
     BUFFER *b;
     int sw;
{
   int i, out;
   if(sw < 0)
   {  out = b->f->c_sw[y+1];
      for(i = y+1; i < b->mxlines; i++) b->f->c_sw[i] = b->f->c_sw[i+1];
      if(out == b->f->c_sw[y]) return(0);
   }
   else if(sw > 0)
   for(i = b->mxlines-1; i > y; i--) b->f->c_sw[i] = b->f->c_sw[i-1];
   
   if(b->f->c_st->i_nl < 0)
   {  for(i = y; i < b->mxlines-1; i++)
      {  if((out = e_scfbol(b->bf[i].len, b->f->c_sw[i],
			b->bf[i].s, b->f->c_st)) == b->f->c_sw[i+1]) break;
	 else b->f->c_sw[i+1] = out;
      }
   }
   else
   {  b->f->c_sw[y] = isspace(b->bf[y].s[b->f->c_st->i_nl]) ? 0 :
	 e_scfbol(b->bf[y-1].len, b->f->c_sw[y-1], b->bf[y-1].s, b->f->c_st);
      for(i = y; i < b->mxlines-1; i++)
      {  out = isspace(b->bf[i+1].s[b->f->c_st->i_nl]) ? 0 :
	     e_scfbol(b->bf[i].len, b->f->c_sw[i], b->bf[i].s, b->f->c_st);
	 if(out == b->f->c_sw[i+1]) break;
	 else b->f->c_sw[i+1] = out;
      }
   }
   return(0);
}

int *e_sc_txt(c_sw, b)
     int *c_sw;
     BUFFER *b;
{
   int i;
   if(!c_sw) c_sw = MALLOC(b->mx.y * sizeof(int));
   c_sw[0] = 0;
   if(b->f->c_st->i_nl < 0)
   {  for(i = 0; i < b->mxlines-1; i++)
      c_sw[i+1] = e_scfbol(b->bf[i].len, c_sw[i], b->bf[i].s, b->f->c_st);
   }
   else
   {  for(i = 0; i < b->mxlines-1; i++)
      {  c_sw[i+1] = isspace(b->bf[i+1].s[b->f->c_st->i_nl]) ? 0 :
		e_scfbol(b->bf[i].len, c_sw[i], b->bf[i].s, b->f->c_st);
      }
   }
   return(c_sw);
}

/*
       Schreiben einer Zeile (Schirminhalt)      */

void e_pr_c_line(y, f)
     int y;
     FENSTER *f;
{
   BUFFER *b = f->b;
   SCHIRM *s = f->s;
   int i, j, k, frb = 0;
   int mcsw = f->c_sw[y], svmsw = f->c_sw[y] == 5 ? 5 : 0, bssw = 0;
   int n_bg = -1, n_nd = strlen(f->c_st->cm_nd)-1;
#ifdef DEBUGGER
   int fsw = 0;
#endif
   for(i = j = 0; j < s->c.x; j++, i++)
   {  if(*(b->bf[y].s + i) == TAB) j += (f->ed->tabn - j % f->ed->tabn - 1);
#ifdef UNIX
      else if(((unsigned char) *(b->bf[y].s + i)) > 126)
      {  j++;
	 if(((unsigned char) *(b->bf[y].s + i)) < 128 + ' ') j++;
      }
      else if(*(b->bf[y].s + i) < ' ') j++;
#endif
   }
   if(j > s->c.x) i--;
   for(k = 0; k < i; k++) e_mk_col(b->bf[y].s, k, frb, f->c_st, n_nd, n_bg);
#ifdef DEBUGGER
   for (j = 1; j <= s->brp[0]; j++)
   if(s->brp[j] == y) {  fsw = 1;  break;  }
   for(j = s->c.x; i < b->bf[y].len && j < f->e.x - f->a.x + s->c.x - 1; i++, j++)
   {  e_mk_col(b->bf[y].s, i, frb, f->c_st, n_nd, n_bg);
      if( y == s->da.y && i >= s->da.x && i < s->de.x )
      frb = s->fb->dy.fb;
      else if(fsw) frb = s->fb->db.fb;
      else if( y == s->fa.y && i >= s->fa.x && i < s->fe.x )
      frb = s->fb->ek.fb;
#else
   for(j = s->c.x; i < b->bf[y].len && j < f->e.x - f->a.x + s->c.x - 1; i++, j++)
   {  e_mk_col(b->bf[y].s, i, frb, f->c_st, n_nd, n_bg);
      if( y == s->fa.y && i >= s->fa.x && i < s->fe.x )
      frb = s->fb->ek.fb;
#endif
      else if( (y < s->ke.y && ( y > s->ka.y ||
               (y == s->ka.y && i >= s->ka.x) ) ) ||
            (y == s->ke.y && i < s->ke.x && ( y > s->ka.y ||
               (y == s->ka.y && i >= s->ka.x) ) ) )
      frb = s->fb->ez.fb;
      else if(f->dtmd == 'b' && *(b->bf[y].s + i) == 0)
      frb = s->fb->et.fb - 16;
      else if(f->dtmd == 'b' && (unsigned char) *(b->bf[y].s + i) == 0xff)
      frb = s->fb->et.fb + 16;
/*        else
            e_mk_col(b->bf[y].s, i, frb, f->c_st, n_nd, n_bg);
*/
      if(*(b->bf[y].s + i) == TAB)
      for(k = f->ed->tabn - j % f->ed->tabn; k > 1 &&
                         j < f->e.x - f->a.x + s->c.x - 2; k--, j++)
      e_pr_char(f->a.x - s->c.x + j + 1, y - s->c.y + f->a.y + 1,
                                                                 ' ', frb);
#ifdef UNIX
      else if(!(e_we_sw & 1) && ((unsigned char)*(b->bf[y].s + i)) > 126)
      {  e_pr_char(f->a.x - s->c.x + j +1, y - s->c.y + f->a.y + 1,
                                                                 '@', frb);
	 if(++j >= f->e.x - f->a.x + s->c.x - 1) return;
	 if(((unsigned char)*(b->bf[y].s + i)) < 128 + ' '
                                   && j < f->e.x - f->a.x + s->c.x - 1)
	 {  e_pr_char(f->a.x - s->c.x + j +1, y - s->c.y + f->a.y + 1,
                                                                 '^', frb);
	    if(++j >= f->e.x - f->a.x + s->c.x - 1) return;
	 }
      }
      else if(*(b->bf[y].s + i) < ' ')
      {  e_pr_char(f->a.x - s->c.x + j +1, y - s->c.y + f->a.y + 1,
                                                                 '^', frb);
	 if(++j >= f->e.x - f->a.x + s->c.x - 1) return;
      }
#endif
      if(*(b->bf[y].s + i) == TAB)
      e_pr_char(f->a.x - s->c.x + j + 1, y - s->c.y + f->a.y + 1,                  ' ', frb);
#ifdef UNIX
      else if(!(e_we_sw & 1) && ((unsigned char)*(b->bf[y].s + i)) > 126
                                   && j < f->e.x - f->a.x + s->c.x - 1)
      {  if(((unsigned char)*(b->bf[y].s + i)) < 128 + ' ')
	 e_pr_char(f->a.x - s->c.x + j + 1, y - s->c.y + f->a.y + 1,
                      ((unsigned char) *(b->bf[y].s + i)) + 'A' - 129, frb);
	 else
	 e_pr_char(f->a.x - s->c.x + j + 1, y - s->c.y + f->a.y + 1,
			((unsigned char) *(b->bf[y].s + i)) - 128, frb);
      }
      else if(*(b->bf[y].s + i) < ' ' && j < f->e.x - f->a.x + s->c.x - 1)
      e_pr_char(f->a.x - s->c.x + j + 1, y - s->c.y + f->a.y + 1,
                                          *(b->bf[y].s + i) + 'A' - 1, frb);
#endif
      else
      e_pr_char(f->a.x - s->c.x + j + 1, y - s->c.y + f->a.y + 1,
                                                    *(b->bf[y].s + i), frb);
   }
   
   e_mk_col(b->bf[y].s, i, frb, f->c_st, n_nd, n_bg);
   
   if (i == b->bf[y].len && f->dtmd == 's' && j < f->e.x - f->a.x + s->c.x - 1)
   {  if( (y < s->ke.y && ( y > s->ka.y ||
               (y == s->ka.y && i >= s->ka.x) ) ) ||
            (y == s->ke.y && i < s->ke.x && ( y > s->ka.y ||
               (y == s->ka.y && i >= s->ka.x) ) ) )
      {  if( *(b->bf[y].s + i) == WR)
	 e_pr_char(f->a.x - s->c.x + j + 1, y - s->c.y + f->a.y + 1,
                                              PWR, s->fb->ez.fb);
	 else
	 e_pr_char(f->a.x - s->c.x + j + 1, y - s->c.y + f->a.y + 1,
                                              PNL, s->fb->ez.fb);
      }
      else
      {  if( *(b->bf[y].s + i) == WR)
	 e_pr_char(f->a.x - s->c.x + j + 1, y - s->c.y + f->a.y + 1,
                                              PWR, frb);
	 else
	 e_pr_char(f->a.x - s->c.x + j + 1, y - s->c.y + f->a.y + 1,
                                              PNL, frb);
      }
      j++;
   }
   for(; j < f->e.x - f->a.x + s->c.x - 1; j++)
   e_pr_char(f->a.x - s->c.x + j + 1, y - s->c.y + f->a.y + 1,
                                              ' ', frb);
}

char *e_dup_word(str)
     char *str;
{
   char *w = MALLOC((strlen(str)+1)*sizeof(char));
   if(w) strcpy(w, str);
   return(w);
}

int e_read_pr_opt(cn)
     ECNT *cn;
{
   FILE *fp;
   char **pc, tmp[128];
   unsigned char *pi;
   int ret, num, i, j;
   e_p_acs = MALLOC(e_p_acn*sizeof(ALLCSW *));
   for(i = 0; i < e_p_acn; i++) e_p_acs[i] = MALLOC(sizeof(ALLCSW));
   e_p_acs[0]->ext = ".c";   e_p_acs[0]->cs = &c_col_st;
   e_p_acs[1]->ext = ".cc";  e_p_acs[1]->cs = &cc_col_st;
   e_p_acs[2]->ext = ".h";  e_p_acs[2]->cs = &cc_col_st;
   e_p_acs[3]->ext = ".f";   e_p_acs[3]->cs = &f_col_st;
#ifdef DJGPP
   sprintf(tmp, "./%s", SYNFILE);
#else
   sprintf(tmp, "%s/%s/%s", getenv("HOME"), HOME_WPE_DIR, SYNFILE);
#endif
   if(!(fp = fopen(tmp, "r")))
   {  sprintf(tmp, "%s/%s", cn->libdrct, SYNFILE);
      if(!(fp = fopen(tmp, "r"))) return(0);
   }
   do
   {  if(!(ret = fscanf(fp, "%s", tmp)) || ret == EOF) break;
      for(i = 0; i < e_p_acn && strcmp(tmp, e_p_acs[i]->ext); i++);
      if(i == e_p_acn)
      {  e_p_acn++;
	 e_p_acs = REALLOC(e_p_acs, e_p_acn*sizeof(ALLCSW *));
	 e_p_acs[i] = MALLOC(sizeof(ALLCSW));
	 e_p_acs[i]->cs  = MALLOC(sizeof(COLSW));
	 e_p_acs[i]->ext = e_dup_word(tmp);
      }
      if(!(ret = fscanf(fp, "%d", &num)) || ret == EOF)
      return(e_error("Syntax in .wpe_pr_opt Wrong!", 0, cn->fb));
      pc = MALLOC((num+1)*sizeof(char *));
      for(j = 0; j < num; j++)
      {  if(!(ret = fscanf(fp, "%s", tmp)) || ret == EOF)
	 return(e_error("Syntax in .wpe_pr_opt Wrong!", 0, cn->fb));
	 pc[j] = e_dup_word(tmp);
      }
      pc[j] = NULL;
      e_p_acs[i]->cs->res_wrd = (unsigned char **) pc;
      if(!(ret = fscanf(fp, "%d", &num)) || ret == EOF)
      return(e_error("Syntax in .wpe_pr_opt Wrong!", 0, cn->fb));
      if(!num) pc = NULL;
      else
      {  pc = MALLOC((num+1)*sizeof(char *));
	 for(j = 0; j < num; j++)
	 {  if(!(ret = fscanf(fp, "%s", tmp)) || ret == EOF)
	    return(e_error("Syntax in .wpe_pr_opt Wrong!", 0, cn->fb));
	    pc[j] = e_dup_word(tmp);
	 }
	 pc[j] = NULL;
      }
      e_p_acs[i]->cs->opr = (unsigned char **) pc;
      if(!(ret = fscanf(fp, "%s", tmp)) || ret == EOF)
      return(e_error("Syntax in .wpe_pr_opt Wrong!", 0, cn->fb));
      e_p_acs[i]->cs->no_alnum = e_dup_word(strcmp(tmp, "NULL") ? tmp : "");
      if(!(ret = fscanf(fp, "%s", tmp)) || ret == EOF)
      return(e_error("Syntax in .wpe_pr_opt Wrong!", 0, cn->fb));
      e_p_acs[i]->cs->cm_bg = e_dup_word(strcmp(tmp, "NULL") ? tmp : "");
      if(!(ret = fscanf(fp, "%s", tmp)) || ret == EOF)
      return(e_error("Syntax in .wpe_pr_opt Wrong!", 0, cn->fb));
      e_p_acs[i]->cs->cm_nd = e_dup_word(strcmp(tmp, "NULL") ? tmp : "");
      if(!(ret = fscanf(fp, "%s", tmp)) || ret == EOF)
      return(e_error("Syntax in .wpe_pr_opt Wrong!", 0, cn->fb));
      e_p_acs[i]->cs->cm_te = e_dup_word(strcmp(tmp, "NULL") ? tmp : "");
      if(!(ret = fscanf(fp, "%s", tmp)) || ret == EOF)
      return(e_error("Syntax in .wpe_pr_opt Wrong!", 0, cn->fb));
      e_p_acs[i]->cs->cm_ch = e_dup_word(strcmp(tmp, "NULL") ? tmp : "");
      ret = fscanf(fp, " %c%c%c%c%c%c%c%c",
		&e_p_acs[i]->cs->c_str, &e_p_acs[i]->cs->c_c,
		&e_p_acs[i]->cs->c_pr, &e_p_acs[i]->cs->c_nc, &e_p_acs[i]->cs->c_nl,
		&e_p_acs[i]->cs->c_ew, &e_p_acs[i]->cs->c_na, &e_p_acs[i]->cs->to_u);
      if(!ret || ret == EOF)
      return(e_error("Syntax in .wpe_pr_opt Wrong!", 0, cn->fb));
      for(pi = &(e_p_acs[i]->cs->c_str); pi <= &(e_p_acs[i]->cs->to_u); pi++)
      if(*pi == ' ') *pi = '\0';
      fscanf(fp, "%d %d %d", &e_p_acs[i]->cs->i_cm,
		&e_p_acs[i]->cs->i_nl, &e_p_acs[i]->cs->i_af);
      if(!ret || ret == EOF)
      return(e_error("Syntax in .wpe_pr_opt Wrong!", 0, cn->fb));
   } while(ret != EOF);
   return(0);
}

int e_add_synt_tl(filename, f)
     char *filename;
     FENSTER *f;
{
   int i;
   f->c_st = NULL;
   f->c_sw = NULL;
   if(!e_p_acs || !filename || !filename[0]) return(0);
   for(i = strlen(filename); i >= 0 && filename[i] != '.'; i--);
   filename += i;
   for(i = 0; i < e_p_acn; i++)
   {  if(!strcmp(filename, e_p_acs[i]->ext))
      {  f->c_st = e_p_acs[i]->cs;
	 if(f->ed->edopt & 8) f->c_sw = MALLOC(f->b->mx.y*sizeof(int));
      }
   }
   return(0);
}

/*  Browser   */

E_AFILE *e_aopen(name, path, mode)
     char *name;
     char *path;
     int mode;
{
   extern struct EXT h_error;
   ECNT *cn = h_error.cn;
   E_AFILE *ep = MALLOC(sizeof(E_AFILE));
   char str[256];
   int i, j;
   ep->b = NULL;
   ep->fp = NULL;
   if(mode & 1)
   {  for(i = 1; i <= cn->mxedt && strcmp(cn->f[i]->datnam, name); i++);
      if(i <= cn->mxedt)
      {  ep->b = cn->f[i]->b;
	 ep->p.x = ep->p.y = 0;
      }
   }
   if((mode & 2) && !ep->b && !access(name, R_OK))
   ep->fp = fopen(name, "r");
   if((mode & 4) && !ep->b && !ep->fp)
   {  for(i = 0; path[i] && !ep->fp; i++)
      {  for(j = 0; (str[j] = path[i]) && path[i] != ':'; j++, i++);
	 if(!path[i]) i--;
	 str[j] = '/';
	 str[j+1] = '\0';
	 strcat(str, name);
	 if(!access(str, R_OK)) ep->fp = fopen(str, "r");
	 if(ep->fp) strcpy(name, str);
      }
   }
   if(!ep->b && !ep->fp) {  FREE(ep);  return(NULL);  }
   return(ep);
}

int e_aclose(ep)
     E_AFILE *ep;
{
   int ret = 0;
   if(ep->fp) ret = fclose(ep->fp);
   FREE(ep);
   return(ret);
}

char *e_agets(str, n, ep)
     char *str;
     int n;
     E_AFILE *ep;
{
   int i, j;
   if(ep->fp) return(fgets(str, n, ep->fp));
   if(ep->p.y >= ep->b->mxlines
	|| (ep->p.y == ep->b->mxlines-1 && ep->p.x > ep->b->bf[ep->p.y].len))
   return(NULL);
   for(i = 0; ep->p.y < ep->b->mxlines; (ep->p.y)++)
   {  for(j = ep->p.x; i < n-1 && j <= ep->b->bf[ep->p.y].len; i++, j++)
      str[i] = ep->b->bf[ep->p.y].s[j];
      
      if(str[i-1] == '\n')
      {  ep->p.x = 0;  (ep->p.y)++;  break;  }
      else if(i == n-1)
      {  ep->p.x = j;  break;  }
      else
      {  ep->p.x = 0;  (ep->p.y)++;  }
   }
   str[i] = '\0';
   return(str);
}

char *e_sh_spl1(sp, str, fp, n)
     char *sp;
     char *str;
     E_AFILE *fp;
     int *n;
{   
    while(1)
    { while(isspace(*++sp));
      if(sp[0] == '\\')
      { (*n)++;
	if(!e_agets((sp = str), 256, fp)) return(NULL);
        --sp;
      }
      else if(sp[0] == '/' && sp[1] == '*')
      { while(!(sp = strstr(sp, "*/")))
        { (*n)++; if(!e_agets((sp = str), 256, fp)) return(NULL); }
	sp++;
      }
      else break;
    }
    return(sp);
}

char *e_sh_spl2(sp, str, fp, n)
     char *sp;
     char *str;
     E_AFILE *fp;
     int *n;
{   
    while(1)
    { while(isspace(*++sp));
      if(!sp[0] || sp[0] == '\n' || sp[0] == '\\')
      {  (*n)++;  if(!e_agets((sp = str), 256, fp)) return(NULL);
	 --sp;
      }
      else if(sp[0] == '/' && sp[1] == '*')
      { while(!(sp = strstr(sp, "*/")))
        { (*n)++; if(!e_agets((sp = str), 256, fp)) return(NULL); }
	sp++;
      }
      else break;
    }
    return(sp);
}

char *e_sh_spl3(sp, str, fp, n)
     char *sp;
     char *str;
     E_AFILE *fp;
     int *n;
{   
    int brk = 1;
    while(*++sp != '}' || brk > 1)
    { if(!sp[0] || sp[0] == '\n')
      { (*n)++; if(!e_agets((sp = str), 256, fp)) return(NULL);
	if(isspace(*sp)) while(isspace(*++sp));
	if(sp[0] == '#' && ispelse(sp))
	{ do
	  {  (*n)++; if(!e_agets((sp = str), 256, fp)) return(NULL);
	     if(isspace(*sp)) while(isspace(*++sp));
	  } while(sp[0] != '#' || !ispendif(sp));
	}
	sp--;
      }
      else if(sp[0] == '/' && sp[1] == '*')
      { while(!(sp = strstr(sp, "*/")))
        { (*n)++; if(!e_agets((sp = str), 256, fp)) return(NULL);  }
	sp++;
      }
      else if(sp[0] == '{') brk++;
      else if(sp[0] == '}') brk--;
      else if(sp[0] == '\'')
      { int bsl = 0;
	while(*++sp && (sp[0] != '\'' || bsl))
	{  bsl = *sp == '\\' ? !bsl : 0;  }
	if(!*sp) sp--;
      }
      else if(sp[0] == '\"')
      { int bsl = 0;
	while(*++sp && ( *sp != '\"' || bsl))
	{  bsl = *sp == '\\' ? !bsl : 0;  }
	if(!*sp) sp--;
      }
    }
    return(sp);
}

char *e_sh_spl5(sp, str, fp, n)
     char *sp;
     char *str;
     E_AFILE *fp;
     int *n;
{   
    int brk = 1;
    while(*++sp != ')' || brk > 1)
    { if(!sp[0] || sp[0] == '\n')
      { (*n)++; if(!e_agets((sp = str), 256, fp)) return(NULL);
	if(isspace(*sp)) while(isspace(*++sp));
	if(sp[0] == '#' && ispelse(sp))
	{ do
	  {  (*n)++; if(!e_agets((sp = str), 256, fp)) return(NULL);
	     if(isspace(*sp)) while(isspace(*++sp));
	  } while(sp[0] != '#' || !ispendif(sp));
	}
	sp--;
      }
      else if(sp[0] == '/' && sp[1] == '*')
      { while(!(sp = strstr(sp, "*/")))
        { (*n)++; if(!e_agets((sp = str), 256, fp)) return(NULL);  }
	sp++;
      }
      else if(sp[0] == '(') brk++;
      else if(sp[0] == ')') brk--;
      else if(sp[0] == '\'')
      { int bsl = 0;
	while(*++sp && (sp[0] != '\'' || bsl))
	{  bsl = *sp == '\\' ? !bsl : 0;  }
	if(!*sp) sp--;
      }
      else if(sp[0] == '\"')
      { int bsl = 0;
	while(*++sp && (*sp != '\"' || bsl))
	{  bsl = *sp == '\\' ? !bsl : 0;  }
	if(!*sp) sp--;
      }
    }
    return(sp);
}

char *e_sh_spl4(sp, str, fp, n)
     char *sp;
     char *str;
     E_AFILE *fp;
     int *n;
{   
    int brk = 0;
    while((*++sp != ',' && *sp != ';' && *sp != '(') || brk)
    { if(!sp[0] || sp[0] == '\n')
      {  (*n)++;  if(!e_agets((sp = str), 256, fp)) return(NULL);
	 --sp;
      }
      else if(sp[0] == '/' && sp[1] == '*')
      { while(!(sp = strstr(sp, "*/")))
        { (*n)++; if(!e_agets((sp = str), 256, fp)) return(NULL);  }
	sp++;
      }
      else if(sp[0] == '\"')
      { int bsl = 0;
	while(*++sp && (sp[0] != '\"' || bsl))
	{  bsl = *sp == '\\' ? !bsl : 0;  }
	if(!*sp) sp--;
      }
      else if(sp[0] == '\'')
      { int bsl = 0;
	while(*++sp && (sp[0] != '\'' || bsl))
	{  bsl = *sp == '\\' ? !bsl : 0;  }
	if(!*sp) sp--;
      }
      else if(sp[0] == '{') brk++;
      else if(sp[0] == '}') brk--;
    }
    return(sp);
}

struct dirfile *e_c_add_df(str, df)
     char *str;
     struct dirfile *df;
{
   if(df == NULL)
   {  df = MALLOC(sizeof(struct dirfile));
      df->anz = 0;
      df->name = MALLOC(sizeof(char*));
   }
   df->anz++;
   df->name = REALLOC(df->name, df->anz * sizeof(char*));
   df->name[df->anz-1] = MALLOC((strlen(str)+1) * sizeof(char));
   strcpy(df->name[df->anz-1], str);
   return(df);
}

int e_find_def(name, startfile, mode, file, num, xn, nold, oldfile, df, first)
     char *name;
     char *startfile;
     int mode;
     char *file;
     int *num;
     int *xn;
     int nold;
     char *oldfile;
     struct dirfile **df;
     int *first;
{
   E_AFILE *fp = NULL;
   char *sp = NULL, str[256], *w, word[256];
   int i, n, com = 0, ret = 1;
   int len = strlen(name);
   if(*df)
   {  for(i = 0; i < (*df)->anz; i++)
      if(!strcmp((*df)->name[i], startfile)) return(-2);
   }
   *df = e_c_add_df(startfile, *df);
   if(!fp) fp = e_aopen(startfile, e_prog.sys_include, mode);
   if(!fp) return(-1);
   for(n = 0; com == 2 || e_agets((sp = str), 256, fp); n++)
   {  if(com)
      {  if(com == 1 && !(sp = strstr(sp, "*/"))) continue;
	 else com = 0;
      }
      if(isspace(*sp) && !(sp = e_sh_spl2(sp, str, fp, &n))) goto b_end;
      if(sp[0] == '/' && sp[1] == '*')
      {  if(!(sp = strstr(sp, "*/"))) com = 1;
	 else {  n--;  com = 2;  sp += 2;  }
	 continue;
      }
      else if(*sp == '#')
      {  if(!(sp = e_sh_spl1(sp, str, fp, &n))) goto b_end;
	 if(!strncmp(sp, "define", 6))
	 {  while(isalpha(*++sp));
	    if(isspace(*sp) && !(sp = e_sh_spl1(sp, str, fp, &n))) goto b_end;
	    if(!strncmp(sp, name, len) && !isalnum1(sp[len]))
	    {  if(*first)
	       {  e_aclose(fp);  strcpy(file, startfile);
		  *num = n;  *xn = ((int)(sp - str)) + len;  return(0);
	       }
	       else if(n == nold && !strcmp(startfile, oldfile)) *first = 1;
	    }
	 }
#ifndef TESTSDEF
	 else if(!strncmp(sp, "include", 7))
	 {  while(isalpha(*++sp));
	    if(isspace(*sp) && !(sp = e_sh_spl1(sp, str, fp, &n))) goto b_end;
	    for(i = 1; (word[i-1] = sp[i])
			&& sp[i] != '\"' && sp[i] != '>'; i++);
	    word[i-1] = '\0';
	    if(!e_find_def(name, word, sp[i] == '>' ? 4 : 7,
				file, num, xn, nold, oldfile, df, first))
	    {  e_aclose(fp);  return(0);  }
	 }
#endif
	 while(sp[i = (strlen(sp) - 1)] != '\n' || sp[i-1] == '\\')
	 {  n++; if(!e_agets((sp = str), 256, fp)) goto b_end;  }
      }
      else if(*sp == '{')
      {  if(!(sp = e_sh_spl3(sp, str, fp, &n))) goto b_end;
	 sp++;  com = 2;  n--;
      }
      else if(!strncmp(sp, "extern", 6)) continue;
      else if(!strncmp(sp, "typedef", 7))
      {  while(!isspace(*++sp));
	 if(!(sp = e_sh_spl2(sp, str, fp, &n))) goto b_end;
	 if(!strncmp(sp, "struct", 6) || !strncmp(sp, "class", 5) ||
		!strncmp(sp, "union", 5))
	 {  while(!isspace(*++sp));
	    if(!(sp = e_sh_spl2(sp, str, fp, &n))) goto b_end;
	    if(*sp == ';') {  sp++;  com = 2;  n--;  continue;  }
	    if(!strncmp(sp, name, len) && !isalnum1(sp[len]))
	    {  while(!isspace(*++sp));
	       if(!(sp = e_sh_spl2(sp, str, fp, &n))) goto b_end;
	       if(*sp == '{')
	       {  if(*first)
		  {  e_aclose(fp);  strcpy(file, startfile);
		     *num = n;  *xn = (int)(sp - str);  return(0);
		  }
		  else if(n == nold && !strcmp(startfile, oldfile)) *first = 1;
	       }
	    }
	 }
	 while(1)
	 {  if(isalpha1(*sp))
	    {  for(w = word; isalnum1(*w = *sp); w++, sp++);
	       *w = '\0';
	       if(isspace(*sp) && !(sp = e_sh_spl2(sp, str, fp, &n))) goto b_end;
	       if(*sp == ';')
	       {  if(!strncmp(word, name, len))
		  {  if(*first)
		     {  e_aclose(fp);  strcpy(file, startfile);
			*num = n;  *xn = (int)(sp - str);  return(0);
		     }
		     else if(n == nold && !strcmp(startfile, oldfile)) *first = 1;
		  }
		  sp++;  com = 2;  n--;  break;
	       }
	    }
	    else if(*sp == '{')
	    {  if(!(sp = e_sh_spl3(sp, str, fp, &n))) goto b_end;
	       if(!(sp = e_sh_spl2(sp, str, fp, &n))) goto b_end;
	       if(*sp == ';') {  sp++;  com = 2;  n--;  break;  }
	    }
	    else if(!(sp = e_sh_spl2(sp, str, fp, &n))) goto b_end;
	 }
      }
      else if(!strncmp(sp, "struct", 6) || !strncmp(sp, "class", 5) ||
		!strncmp(sp, "union", 5))
      {  while(!isspace(*++sp));
	 if(!(sp = e_sh_spl2(sp, str, fp, &n))) goto b_end;
	 if(*sp == ';') {  sp++;  com = 2;  n--;  continue;  }
	 if(!strncmp(sp, name, len) && !isalnum1(sp[len]))
	 {  while(!isspace(*++sp));
	    if(!(sp = e_sh_spl2(sp, str, fp, &n))) goto b_end;
	    if(*sp == '{')
	    {  if(*first)
	       {  e_aclose(fp);  strcpy(file, startfile);
		  *num = n;  *xn = (int)(sp - str);  return(0);
	       }
	       else if(n == nold && !strcmp(startfile, oldfile)) *first = 1;
	    }
	 }
	 else if(*sp != '{')
	 {  while(!isspace(*++sp));
	    if(!(sp = e_sh_spl2(sp, str, fp, &n))) goto b_end;
	 }
	 if(*sp == ';') {  sp++;  com = 2;  n--;  continue;  }
	 if(*sp == '{')
	 {  if(!(sp = e_sh_spl3(sp, str, fp, &n))) goto b_end;
	    if(!(sp = e_sh_spl2(sp, str, fp, &n))) goto b_end;
	    if(*sp == ';') {  sp++;  com = 2;  n--;  continue;  }
	 }
	 while(1)
	 {  while (*sp == '*') sp++;
	    if(isspace(*sp) && !(sp = e_sh_spl2(sp, str, fp, &n))) goto b_end;
	    if(!strncmp(sp, name, len) && !isalnum1(sp[len]))
	    {  while(isalnum1(*sp)) sp++;
	       if(isspace(*sp) && !(sp = e_sh_spl2(sp, str, fp, &n))) goto b_end;
	       if((*sp == ';' || *sp == ',' || *sp == '='
					 || *sp == '[' || *sp == ')'))
	       {  if(*first)
		  {  e_aclose(fp);  strcpy(file, startfile);
		     *num = n;  *xn = (int)(sp - str);  return(0);
		  }
		  else if(n == nold && !strcmp(startfile, oldfile))
		  {  *first = 1;
		     if(*sp == ';') {  sp++;  com = 2;  n--;  break;  }
		  }
	       }
	       else if(*sp == '(')
	       {  if(!(sp = e_sh_spl5(sp, str, fp, &n))) goto b_end;
		  if(!(sp = e_sh_spl2(sp, str, fp, &n))) goto b_end;
		  if(*sp == '{')
		  {  if(*first)
		     {  e_aclose(fp);  strcpy(file, startfile);
			*num = n;  *xn = (int)(sp - str);  return(0);
		     }
		     else if(n == nold && !strcmp(startfile, oldfile))
		     {  *first = 1;  break;  }
		  }
		  else break;
	       }
	    }
	    else if(*sp == '(') sp++;
	    else if(*sp == '*') {  while (*sp == '*') sp++;  continue;  }
	    else if(*sp == ';') {  sp++;  com = 2;  n--;  break;  }
	    else if(*sp == '{')
	    {  if(!(sp = e_sh_spl3(sp, str, fp, &n))) goto b_end;
	       sp++;  com = 2;  n--;
	       break;
	    }
	    else
	    {  if(!(sp = e_sh_spl4(sp, str, fp, &n))) goto b_end;
	       if(*sp == '(') break;
	       else if(*sp == ';') {  sp++;  com = 2;  n--;  break;  }
	       if(!(sp = e_sh_spl2(sp, str, fp, &n))) goto b_end;
	    }
	    if(*sp == ';') {  sp++;  com = 2;  n--;  break;  }
	 }
      }
      else if(isalnum1(*sp))
      {  while(isalnum1(*sp)) sp++;
	 while(1)
	 {  while (*sp == '*') sp++;
	    if(isspace(*sp) && !(sp = e_sh_spl2(sp, str, fp, &n))) goto b_end;
	    if(!strncmp(sp, name, len) && !isalnum1(sp[len]))
	    {  while(isalnum1(*sp)) sp++;
	       if(isspace(*sp) && !(sp = e_sh_spl2(sp, str, fp, &n))) goto b_end;
	       if(*sp == ';' || *sp == ',' || *sp == '='
					 || *sp == '[' || *sp == ')')
	       {  if(*first)
		  {  e_aclose(fp);  strcpy(file, startfile);
		     *num = n;  *xn = (int)(sp - str);  return(0);
		  }
		  else if(n == nold && !strcmp(startfile, oldfile))
		  {  *first = 1;  break;  }
	       }
	       else if(*sp == '(')
	       {  if(!(sp = e_sh_spl5(sp, str, fp, &n))) goto b_end;
		  if(!(sp = e_sh_spl2(sp, str, fp, &n))) goto b_end;
		  if(*sp == '{')
		  {  if(*first)
		     {  e_aclose(fp);  strcpy(file, startfile);
			*num = n;  *xn = (int)(sp - str);  return(0);
		     }
		     else if(n == nold && !strcmp(startfile, oldfile))
		     {  *first = 1;  break;  }
		  }
		  else break;
	       }
	    }
	    else if(*sp == '(') sp++;
	    else if(*sp == '*') 
            {  while (*sp == '*') sp++;  continue;  }
	    else if(*sp == ';') {  sp++;  com = 2;  n--;  break;  }
	    else if(*sp == '{')
	    {  if(!(sp = e_sh_spl3(sp, str, fp, &n))) goto b_end;
	       sp++;  com = 2;  n--;
	       break;
	    }
	    else
	    {  if(!(sp = e_sh_spl4(sp, str, fp, &n))) goto b_end;
	       if(*sp == '(')
	       {  if(!(sp = e_sh_spl5(sp, str, fp, &n))) goto b_end;
		  if(!(sp = e_sh_spl2(sp, str, fp, &n))) goto b_end;
		  if(*sp == '{' && !(sp = e_sh_spl3(sp, str, fp, &n))) goto b_end;
		  sp++;  com = 2;  n--;  break;
	       }
	       else if(*sp == ';') {  sp++;  com = 2;  n--;  break;  }
	       if(!(sp = e_sh_spl2(sp, str, fp, &n))) goto b_end;
	    }
	    if(*sp == ';') {  sp++;  com = 2;  n--;  break;  }
	 }
      }
   }
   b_end:
    e_aclose(fp);
   return(ret);
}

int e_show_nm_f(name, f, oldn, oldname)
     char *name;
     FENSTER *f;
     int oldn;
     char **oldname;
{
   int i, j, len, ret, num, x, first = oldn < 0 ? 1 : 0;
   char str[128], file[128], *filename;
   struct dirfile *df, *fdf = NULL;
#ifndef TESTSDEF
   if(!access(e_prog.project, 0))
   {
#ifdef XWINDOW
      if(e_we_sw & 1) fk_pointer(WTCUR);
#endif
      if(e_read_var(f)) ret = -1;
      else
      {  df = e_p_get_var("FILES");
	 if(!df)
	 {  e_error(e_p_msg[8], 0, f->fb);
#ifdef XWINDOW
	    if(e_we_sw & 1) fk_pointer(LASTCUR);
#endif
	    return(-1);
	 }
	 for(i = 0, ret = 1; i < df->anz && ret; i++)
	 {  strcpy(str, df->name[i]);
	    ret = e_find_def(name, str, 7, file, &num, &x, oldn,
						*oldname, &fdf, &first);
	 }
	 freedf(df);
      }
   }
   else
#endif
   {  if(f->dtmd <= 'Z') return(-1);
#ifdef XWINDOW
      if(e_we_sw & 1) fk_pointer(WTCUR);
#endif
      strcpy(str, f->datnam);
      ret = e_find_def(name, str, 7, file, &num, &x, oldn,
						*oldname, &fdf, &first);
   }
   freedf(fdf);
   if(ret)
   {  sprintf(str, "%s not found!", name);
      e_error(str, 0, f->fb);
#ifdef XWINDOW
      if(e_we_sw & 1) fk_pointer(LASTCUR);
#endif
      return(-1);
   }
   if(*oldname) FREE(*oldname);
   *oldname = MALLOC((strlen(file)+1) * sizeof(char));
   strcpy(*oldname, file);
   for(i = strlen(file)-1; i >= 0 && file[i] != '/'; i--);
   for(j = f->ed->mxedt; j > 0; j--)
   {  if(i < 0) filename = f->ed->f[j]->datnam;
      else filename = e_mkfilename(f->ed->f[j]->dirct, f->ed->f[j]->datnam);
      if(!strcmp(filename, file)) break;
   }
   if(j > 0) e_switch_window(f->ed->edt[j], f->ed->f[f->ed->mxedt]);
   else e_edit(f->ed, file);
   f = f->ed->f[f->ed->mxedt];
   for(i = num, j = x+1-(len = strlen(name)); i >= 0; )
   {  for(len = strlen(name); j >= 0
			&& strncmp(name, f->b->bf[i].s+j, len); j--);
      if(j < 0 && i >= 0)
      {  i--;  j = f->b->bf[i].len-len+1;  }
      else break;
   }
   if(i >= 0) {  num = i;  x = j;  }
   else len = 0;
   f->s->fa.y = f->s->fe.y = f->b->b.y = num;
   f->s->fe.x = f->b->b.x = x + len;
   f->s->fa.x = x;
   e_cursor(f, 1);
   f->s->fa.y = num;
   e_schirm(f, 1);
#ifdef XWINDOW
   if(e_we_sw & 1) fk_pointer(LASTCUR);
#endif
   return(num);
}


struct {  int num;
	  char *str, *file;
       }  sh_df = {  -1, NULL  };

int e_sh_def(f)
     FENSTER *f;
{
   char str[80];
   if(f->ed->shdf && f->ed->shdf->anz > 0) strcpy(str, f->ed->shdf->name[0]);
   else str[0] = '\0';
   if(e_add_arguments(str, "Show Definition", f, 0 , AltB, &f->ed->shdf))
   {  if(sh_df.str) FREE(sh_df.str);
      sh_df.str = MALLOC((strlen(str)+1)*sizeof(char));
      strcpy(sh_df.str, str);
      if(sh_df.file) {  FREE(sh_df.file);  sh_df.file = NULL;  }
      f->ed->shdf = e_add_df(str, f->ed->shdf);
      sh_df.num = e_show_nm_f(str, f, -1, &sh_df.file);
   }
   return(0);
}

int e_sh_nxt_def(f)
     FENSTER *f;
{
   if(sh_df.num >= 0 && sh_df.str && sh_df.file)
   {
      sh_df.num = e_show_nm_f(sh_df.str, f, sh_df.num, &sh_df.file);
   }
   return(0);
}

int e_nxt_brk(f)
     FENSTER *f;
{
   int c = f->b->bf[f->b->b.y].s[f->b->b.x];
   int i, j, ob, cb, bsp, brk, nif;
   if(c == '{' || c == '(' || c == '[')
   {  if(c == '{') {  ob = '{';  cb = '}';  }
      else if(c == '(') {  ob = '(';  cb = ')';  }
      else {  ob = '[';  cb = ']';  }
      for(brk = 1, i = f->b->b.y; i < f->b->mxlines; i++)
      for(j = i == f->b->b.y ? f->b->b.x+1 : 0; j < f->b->bf[i].len; j++)
      {  if(f->b->bf[i].s[j] == '\"')
	 {  for(bsp = 0, j++; j < f->b->bf[i].len
			&& (f->b->bf[i].s[j] != '\"' || bsp); j++)
	    {  if(f->b->bf[i].s[j] == '\\') bsp = !bsp;
	       else bsp = 0;
	       if(j == f->b->bf[i].len - 1 && bsp && i < f->b->mxlines-1)
	       {  i++;  j = -1;  bsp = 0;  }
	    }
	 }
	 else if(f->b->bf[i].s[j] == '\'')
	 {  for(bsp = 0, j++; j < f->b->bf[i].len
			&& (f->b->bf[i].s[j] != '\'' || bsp); j++)
	    {  if(f->b->bf[i].s[j] == '\\') bsp = !bsp;
	       else bsp = 0;
	       if(j == f->b->bf[i].len - 1 && bsp && i < f->b->mxlines-1)
	       {  i++;  j = -1;  bsp = 0;  }
	    }
	 }
	 else if(f->b->bf[i].s[j] == '/' && f->b->bf[i].s[j+1] == '*')
	 {  for(j += 2; f->b->bf[i].s[j] != '*'
			|| f->b->bf[i].s[j+1] != '/'; j++)
	    {  if(j >= f->b->bf[i].len - 1)
	       {  if(i < f->b->mxlines-1) {  i++;  j = -1;  }
		  else break;
	       }
	    }
	 }
	 else if(f->b->bf[i].s[j] == '/' && f->b->bf[i].s[j+1] == '/') break;
	 else if(f->b->bf[i].s[j] == '#' && ispelse(f->b->bf[i].s))
	 {  for(nif = 1, i++; i < f->b->mxlines-1; i++)
	    {  for(j = 0; isspace(f->b->bf[i].s[j]); j++);
	       if(ispendif(f->b->bf[i].s+j)) nif--;
	       else if(ispif(f->b->bf[i].s+j)) nif++;
               if(!nif) break;
	    }
	    continue;
	 }
	 else if(f->b->bf[i].s[j] == ob) brk++;
	 else if(f->b->bf[i].s[j] == cb)
	 {  brk--;
	    if(!brk) {  f->b->b.y = i;  f->b->b.x = j;  return(0);  }
	 }
      }
      return(e_error("No Matching Bracket!", 0, f->ed->fb));
   }
   else
   {  if(c == '}') {  ob = '{';  cb = '}';  }
      else if(c == ')') {  ob = '(';  cb = ')';  }
      else if(c == ']') {  ob = '[';  cb = ']';  }
      else {  ob = 0;  cb = 0;  }
      for(brk = -1, i = f->b->b.y; i >= 0; i--)
      {  if(i == f->b->b.y)
	 for(j = 0; j < f->b->b.x && (f->b->bf[i].s[j] != '/'
            || f->b->bf[i].s[j+1] != '/'); j++);
	 else
	 for(j = 0; j < f->b->bf[i].len && (f->b->bf[i].s[j] != '/'
            || f->b->bf[i].s[j+1] != '/'); j++);
	 for(j--; j >= 0; j--)
	 {  if(f->b->bf[i].s[j] == '\"')
	    {  for(j--; j >= 0; j--)
	       {  if(f->b->bf[i].s[j] == '\"')
		  {  for(bsp = 0, j--; j >= 0 && f->b->bf[i].s[j] == '\\'; j--)
		     bsp = !bsp;
		     j++;
		     if(!bsp) break;
		  }
		  if(j == 0 && i > 0 && f->b->bf[i-1].s[f->b->bf[i-1].len] == '\\')
		  {  i--;  j = f->b->bf[i].len;  }
	       }
	    }
	    else if(f->b->bf[i].s[j] == '\'')
	    {  for(j--; j >= 0; j--)
	       {  if(f->b->bf[i].s[j] == '\'')
		  {  for(bsp = 0, j--; j >= 0 && f->b->bf[i].s[j] == '\\'; j--)
		     bsp = !bsp;
		     j++;
		     if(!bsp) break;
		  }
		  if(j == 0 && i > 0 && f->b->bf[i-1].s[f->b->bf[i-1].len] == '\\')
		  {  i--;  j = f->b->bf[i].len;  }
	       }
	    }
	    else if(f->b->bf[i].s[j] == '/' && f->b->bf[i].s[j-1] == '*')
	    {  for(j -= 2; f->b->bf[i].s[j] != '*'
			|| f->b->bf[i].s[j-1] != '/'; j--)
	       {  if(j <= 0)
		  {  if(i > 0) {  i--;  j = f->b->bf[i].len;  }
		     else break;
		  }
	       }
	    }
	    else if(f->b->bf[i].s[j] == '#' && ispelse(f->b->bf[i].s))
	    {  for(nif = 1, i--; i > 0; i--)
	       {  for(j = 0; isspace(f->b->bf[i].s[j]); j++);
		  if(ispendif(f->b->bf[i].s+j)) nif++;
		  else if(ispif(f->b->bf[i].s+j)) nif--;
		  if(!nif) break;
	       }
	       continue;
	    }
	    else if(!ob)
	    {  if(f->b->bf[i].s[j] == '{' || f->b->bf[i].s[j] == '('
			|| f->b->bf[i].s[j] == '[')
	       {  brk++;
		  if(!brk) {  f->b->b.y = i;  f->b->b.x = j;  return(0);  }
	       }
	       else if(f->b->bf[i].s[j] == '}' || f->b->bf[i].s[j] == ')'
			|| f->b->bf[i].s[j] == ']') brk--;
	       else if(i == 0 && j == 0)
	       {  f->b->b.y = i;  f->b->b.x = j;  return(0);  }
	    }
	    else
	    {  if(f->b->bf[i].s[j] == ob)
	       {  brk++;
		  if(!brk) {  f->b->b.y = i;  f->b->b.x = j;  return(0);  }
	       }
	       else if(f->b->bf[i].s[j] == cb) brk--;
	    }
	 }
      }
      return(e_error("No Matching Bracket!", 0, f->ed->fb));
   }
   return(e_error("No Matching Bracket!", 0, f->ed->fb));
}

char *e_mbt_mk_sp(str, n, sw, m)
     char *str;
     int n;
     int sw;
     int *m;
{
   int k;
   if(!sw) *m = n;
   else *m = n / sw + n % sw;
   str = REALLOC(str, (*m+1)*sizeof(char));
   if(!sw) k = 0;
   else for(k = 0; k < n / sw; k++) str[k] = '\t';
   for(; k < *m; k++) str[k] = ' ';
   str[*m] = '\0';
   return(str);
}

int e_mbt_str(b, ii, jj, c, n, sw, cmnd)
     BUFFER *b;
     int *ii;
     int *jj;
     unsigned char c;
     int n;
     int sw;
     int *cmnd;
{
   int i = *ii, j = *jj + 1, bsp;
   if(*cmnd != 2) *cmnd = 0;
   for(bsp = 0; j < b->bf[i].len && (b->bf[i].s[j] != c || bsp); j++)
   {  if(b->bf[i].s[j] == '\\') bsp = !bsp;
      else bsp = 0;
      if(j == b->bf[i].len - 1 && bsp && i < b->mxlines-1)
      {  char *str = MALLOC(1);
	 int m;
         i++;  bsp = 0;
	 for(j = 0, m = b->bf[i].len; j < m && isspace(b->bf[i].s[j]); j++);
	 if(j > 0) e_del_nchar(b, b->f->s, 0, i, j);
	 if(j < m)
	 {  str = e_mbt_mk_sp(str, n+b_dif, sw, &m);
	    e_ins_nchar(b, b->f->s, str, 0, i, m);
	 }
	 j = -1;
	 FREE(str);
      }
   }
   *ii = i;  *jj = j;
   return(0);  
}

int e_mbt_cnd(b, ii, jj, n, sw, cmnd)
     BUFFER *b;
     int *ii;
     int *jj;
     int n;
     int sw;
     int *cmnd;
{
   int i = *ii, j = *jj + 2;
   for(; b->bf[i].s[j] != '*'
			|| b->bf[i].s[j+1] != '/'; j++)
   {  if(j >= b->bf[i].len - 1)
      {  if(i < b->mxlines-1)
	 {  char *str = MALLOC(1);
	    int m;
	    i++;
	    for(j = 0, m = b->bf[i].len; j < m && isspace(b->bf[i].s[j]); j++);
	    if(j > 0) e_del_nchar(b, b->f->s, 0, i, j);
	    if(j < m && (b->bf[i].s[0] != '*' || b->bf[i].s[1] != '/'))
	    {  str = e_mbt_mk_sp(str, n+b_dif, sw, &m);
	       e_ins_nchar(b, b->f->s, str, 0, i, m);
	    }
	    j = -1;
	    FREE(str);
	    if(*cmnd == 2) *cmnd = 1;
	 }
	 else break;
      }
   }
   *ii = i;  *jj = j+1;
   return(0);  
}


int e_mk_beauty(sw, ndif, f)
     int sw;
     int ndif;
     FENSTER *f;
{
   BUFFER *b;
   SCHIRM *s;
   int bg, nd, m, n, i, j, k, brk, cbrk = 0, nif = 0, nic = 0;
   int nstrct = 0, cmnd, cm_sv;
   char *tstr = MALLOC(sizeof(char));
   char *bstr = MALLOC((ndif+1)*sizeof(char));
   int *nvek = MALLOC(sizeof(int));
   int *ifvekb = MALLOC(sizeof(int));
   int *ifvekr = MALLOC(sizeof(int));
   int *vkcs = MALLOC(sizeof(int));
   int *vkcb = MALLOC(sizeof(int));
   PUNKT sa, se, sb;
   for(i = f->ed->mxedt; i > 0 && f->ed->f[i]->dtmd <= 'Z'; i--);
   if(i <= 0)
   {  FREE(tstr);  FREE(bstr);  FREE(nvek);  FREE(ifvekb);
      FREE(ifvekr);  FREE(vkcs);  FREE(vkcb);
      return(0);
   }
   e_switch_window(f->ed->edt[i], f);
#ifdef XWINDOW
   if(e_we_sw & 1) fk_pointer(WTCUR);
#endif
   f = f->ed->f[f->ed->mxedt];
   b = f->b;
   s = f->s;
   sa = s->ka;  se = s->ke;  sb = b->b;
   if(sw & 1)
   {  if(sw & 2)  bg = i = b->b.x == 0 ? b->b.y : b->b.y + 1;
      else  bg = i = s->ka.x == 0 ? s->ka.y : s->ka.y + 1;
      nd = s->ke.x == 0 ? s->ke.y : s->ke.y + 1;
      if(nd > b->mxlines) nd = b->mxlines;
   }
   else
   {  if(sw & 2)  bg = i = b->b.x == 0 ? b->b.y : b->b.y + 1;
      else  bg = i = 0;
      nd = b->mxlines;
   }
   if(s->ka.y < 0 || (s->ka.y == 0 && s->ka.x <= 0)) n = 0;
   else
   {  for(n = j = 0; j < b->bf[i].len && isspace(b->bf[i].s[j]); j++)
      {  if(b->bf[i].s[j] == ' ') n++;
	 else if(b->bf[i].s[j] == '\t')
	 n += f->ed->tabn - (n % f->ed->tabn);
      }
   }
   tstr = e_mbt_mk_sp(tstr, n, (sw & 4) ? 0 : f->ed->tabn, &m);
   for(k = 0; k < ndif; k++) bstr[k] = ' ';
   bstr[ndif] = '\0';
   nvek[0] = n;
   for(cm_sv = 1, cmnd = 1, brk = 0; i < nd; i++)
   {  for(j = 0; j < b->bf[i].len && isspace(b->bf[i].s[j]); j++);
      if(i > bg)
      {  for(k = b->bf[i-1].len-1; k >= 0 && isspace(b->bf[i-1].s[k]); k--);
	 if(k >= 0) e_del_nchar(b, s, k + 1, i-1, b->bf[i-1].len-1-k);
	 e_del_nchar(b, s, 0, i, j);
         if(b->bf[i].len > 0 && b->bf[i].s[0] != '#' && 
            (b->bf[i].s[0] != '/' || b->bf[i].s[1] != '*'))
	 {  if((cmnd == 0 && (!cm_sv || b->bf[i].s[0] != '{')) ||
              (cmnd == 2 && b->bf[i-1].s[k] == '\\'))
	    {  tstr = e_mbt_mk_sp(tstr, !cmnd ? n+b_dif : b_dif,
                                	(sw & 4) ? 0 : f->ed->tabn, &m);
               e_ins_nchar(b, s, tstr, 0, i, m);
               j = m;
               tstr = e_mbt_mk_sp(tstr, n, (sw & 4) ? 0 : f->ed->tabn, &m);
	    }
            else
            {  e_ins_nchar(b, s, tstr, 0, i, m);
               j = m;
               if(cmnd == 0) cmnd = 1;
            }
	 }
      }
      if(cmnd == 0) cm_sv = cbrk ? 1 : 0;
      else cm_sv = cmnd;
      for(cmnd = 1; j < b->bf[i].len; j++)
      {  if(b->bf[i].s[j] == '\"')
           e_mbt_str(b, &i, &j, '\"', n, (sw & 4) ? 0 : f->ed->tabn, &cmnd);
	 else if(f->b->bf[i].s[j] == '\'')
           e_mbt_str(b, &i, &j, '\'', n, (sw & 4) ? 0 : f->ed->tabn, &cmnd);
	 else if(b->bf[i].s[j] == '/' && b->bf[i].s[j+1] == '*')
           e_mbt_cnd(b, &i, &j, n, (sw & 4) ? 0 : f->ed->tabn, &cmnd);
	 else if(b->bf[i].s[j] == '/' && b->bf[i].s[j+1] == '/') break;
	 else if(b->bf[i].s[j] == ';') {  cmnd = cbrk ? 0 : 1;  nstrct = 0;  }
	 else if(b->bf[i].s[j] == '#' && ispif(b->bf[i].s))
	 {  nif++;
	    ifvekb = REALLOC(ifvekb, (nif+1)*sizeof(int));
	    ifvekr = REALLOC(ifvekr, (nif+1)*sizeof(int));
	    ifvekb[nif] = brk;
	    ifvekr[nif] = cbrk;
	    cmnd = 2;
	 }
	 else if(b->bf[i].s[j] == '#' && nif > 0 && ispelse(b->bf[i].s))
	 {  brk = ifvekb[nif];
	    cbrk = ifvekr[nif];
	    n = nvek[brk];
	    tstr = e_mbt_mk_sp(tstr, n, (sw & 4) ? 0 : f->ed->tabn, &m);
	    cmnd = 2;
	 }
	 else if(b->bf[i].s[j] == '#' && ispendif(b->bf[i].s))
	 {  nif--;  cmnd = 2;  }
	 else if(b->bf[i].s[j] == '#')
	 {  cmnd = 2;
	    for(j++; j < b->bf[i].len; j++)
	    {  if(j == b->bf[i].len - 1 && i < b->mxlines-1
					&& b->bf[i].s[j] == '\\')
	       {  i++;  j = 0;  }
	       else if(b->bf[i].s[j] == '\"')
	         e_mbt_str(b, &i, &j, '\"', n, (sw & 4) ? 0 : f->ed->tabn, &cmnd);
	       else if(f->b->bf[i].s[j] == '\'')
	         e_mbt_str(b, &i, &j, '\'', n, (sw & 4) ? 0 : f->ed->tabn, &cmnd);
	       else if(b->bf[i].s[j] == '/' && b->bf[i].s[j+1] == '*')
	         e_mbt_cnd(b, &i, &j, n, (sw & 4) ? 0 : f->ed->tabn, &cmnd);
	       else if(b->bf[i].s[j] == '/' && b->bf[i].s[j+1] == '/') break;
	    }
            if(b->bf[i].s[j] == '/' && b->bf[i].s[j+1] == '/') break;
	 }
	 else if(b->bf[i].s[j] == '(') {  nstrct = 0;  cmnd = 0;  cbrk++;  }
	 else if(b->bf[i].s[j] == ')') {  nstrct = 0;  cmnd = 0;  cbrk--;  }
	 else if(b->bf[i].s[j] == '{')
	 {  brk++;
	    cmnd = 1;
	    for(k = j + 1; k < b->bf[i].len && isspace(b->bf[i].s[k]); k++);
	    if(k < b->bf[i].len
		&& (b->bf[i].s[k] != '/' 
                || (b->bf[i].s[k+1] != '*' && b->bf[i].s[k+1] != '/')))
	    {  e_del_nchar(b, s, j+1, i, k-j-1);
	       e_ins_nchar(b, s, bstr, j+1, i, ndif-1);
	    }
	    if(nstrct == 1)
	    {  if(k >= b->bf[i].len) n = nvek[brk-1];
               else
               {  for(k = j - 1; k >= 0 && isspace(b->bf[i].s[k]); k--);
                  if(k >= 0) nstrct++;
               }
	       nstrct++;
	       nic++;
	       vkcb = REALLOC(vkcb, (nic+1)*sizeof(int));
	       vkcs = REALLOC(vkcs, (nic+1)*sizeof(int));
	       vkcs[nic] = 0;
	       vkcb[nic] = brk-1;
	    }
            else
            {  for(n = k = 0; k < j; k++)
               {  if(b->bf[i].s[k] == '\t')
                  n += f->ed->tabn - (n % f->ed->tabn);
                  else n++;
               }
            }
	    n += ndif;
	    tstr = e_mbt_mk_sp(tstr, n, (sw & 4) ? 0 : f->ed->tabn, &m);
	    nvek = REALLOC(nvek, (brk+1)*sizeof(int));
	    nvek[brk] = n;
	 }
	 else if(f->b->bf[i].s[j] == '}')
	 {  brk--;
	    cmnd = 1;
	    nstrct = 0;
	    if(nic > 0 && vkcb[nic] == brk - 1)
	    {  n = nvek[brk];  nic--;  brk--;  }
	    if(brk < 0)
	    {  FREE(tstr);  FREE(bstr);  FREE(nvek);
	       FREE(ifvekb);  FREE(ifvekr);
	       FREE(vkcs);  FREE(vkcb);
	       b->b = sb;  s->ka = sa;  s->ke = se;
	       e_schirm(f, 1);
#ifdef XWINDOW
	       if(e_we_sw & 1) fk_pointer(LASTCUR);
#endif
	       return(0);
	    }
	    n -= ndif;
	    tstr = e_mbt_mk_sp(tstr, n, (sw & 4) ? 0 : f->ed->tabn, &m);
	    for(k = j - 1; k >= 0 && isspace(b->bf[i].s[k]); k--);
	    e_del_nchar(b, s, k+1, i, j-1-k);
	    if(k > 0)
	    {  e_ins_nchar(b, s, bstr, k+1, i, ndif-1);  j = k+ndif+1;  }
	    else
	    {  e_ins_nchar(b, s, tstr, k+1, i, m);  j = k+m+2;  }
	    n = nvek[brk];
	    tstr = e_mbt_mk_sp(tstr, n, (sw & 4) ? 0 : f->ed->tabn, &m);
	 }
         else if(((j == 0 || !isalnum1(b->bf[i].s[j-1]))
            && (iscase(b->bf[i].s+j) || isstatus(b->bf[i].s+j)))
	    || (nstrct > 1 && isalnum1(b->bf[i].s[j])))
	 {  if(vkcs[nic])
	    {  n = nvek[vkcb[nic]+1];
	       tstr = e_mbt_mk_sp(tstr, n, (sw & 4) ? 0 : f->ed->tabn, &m);
	       for(k = j - 1; k >= 0 && isspace(b->bf[i].s[k]); k--);
	       e_del_nchar(b, s, k+1, i, j-1-k);
	       if(k > 0)
	       {  e_ins_nchar(b, s, bstr, k+1, i, ndif-1);  j = k+ndif+1;  }
	       else
	       {  e_ins_nchar(b, s, tstr, k+1, i, m);  j = k+m+2;  }
	    }
	    else {  brk++;  vkcs[nic] = 1;  }
            if(nstrct < 2 || isstatus(b->bf[i].s+j))
	    {  for(j++; j < b->bf[i].len && b->bf[i].s[j] != ':'; j++);
	       for(k = j + 1; k < b->bf[i].len && isspace(b->bf[i].s[k]); k++);
	       if(k < b->bf[i].len
		&& (b->bf[i].s[k] != '/' 
                || (b->bf[i].s[k+1] != '*' && b->bf[i].s[k+1] != '/')))
	       {  e_del_nchar(b, s, j+1, i, k-j-1);
		  e_ins_nchar(b, s, bstr, j+1, i, ndif-1);
		  for(n = k = 0; k < j; k++)
		  {  if(b->bf[i].s[k] == '\t')
		     n += f->ed->tabn - (n % f->ed->tabn);
		     else n++;
		  }
	       }
            }
            else if(nstrct == 2) e_ins_nchar(b, s, bstr, j, i, ndif);
            else j--;
	    n += ndif;
	    tstr = e_mbt_mk_sp(tstr, n, (sw & 4) ? 0 : f->ed->tabn, &m);
	    nvek = REALLOC(nvek, (brk+1)*sizeof(int));
	    nvek[brk] = n;
	    cmnd = 3;
            nstrct = 0;
	 }
	 else if((j == 0 || !isalnum1(b->bf[i].s[j-1]))
	    && (!strncmp(b->bf[i].s+j, "switch", 6) 
               		&& !isalnum1(b->bf[i].s[j+6])))
	 {  nic++;
	    vkcb = REALLOC(vkcb, (nic+1)*sizeof(int));
	    vkcs = REALLOC(vkcs, (nic+1)*sizeof(int));
	    vkcs[nic] = 0;
	    vkcb[nic] = brk;
	 }
	 else if((j == 0 || !isalnum1(b->bf[i].s[j-1]))
            && ((!strncmp(b->bf[i].s+j, "class", 5) 
               		&& !isalnum1(b->bf[i].s[j+5]))
            ||  (!strncmp(b->bf[i].s+j, "struct", 6) 
               		&& !isalnum1(b->bf[i].s[j+6])))) nstrct = 1;
	 else if(cmnd == 1 && !isspace(f->b->bf[i].s[j])) cmnd = 0;
	 else if(cmnd == 3 && f->b->bf[i].s[j] == ':') cmnd = 1;
      }
   }
   FREE(nvek);  FREE(tstr);  FREE(bstr);
   FREE(ifvekb);  FREE(ifvekr);
   FREE(vkcs);  FREE(vkcb);
   s->ka = sa;  s->ke = se;
   b->b = sb;
   e_schirm(f, 1);
#ifdef XWINDOW
   if(e_we_sw & 1) fk_pointer(LASTCUR);
#endif
   return(0);
}

int e_p_beautify(f)
     FENSTER *f;
{
   static int b_sw = 0;
   int ret;
   W_OPTSTR *o = e_init_opt_kst(f);
   if(!o) return(-1);
   o->xa = 21;  o->ya = 3;  o->xe = 52;  o->ye = 19;
   o->bgsw = AltO;
   o->name = "Beautify";
   o->crsw = AltO;
   e_add_txtstr(4, 2, "Begin:", o);
   e_add_txtstr(4, 6, "Scope:", o);
   e_add_numstr(4, 12, 26, 12, 2, 100, 0, AltN, "Number of Columns:", b_dif, o);
   e_add_sswstr(5, 10, 3, AltT, (b_sw & 4) ? 1 : 0, "No Tabulators     ", o);
   e_add_pswstr(0, 5, 3, 0, AltG, 0, "Global            ", o);
   e_add_pswstr(0, 5, 4, 0, AltS, b_sw & 1, "Selected Text     ", o);
   e_add_pswstr(1, 5, 7, 0, AltE, 0, "Entire Scope      ", o);
   e_add_pswstr(1, 5, 8, 0, AltF, (b_sw & 2) ? 1 : 0, "From Cursor       ", o);
   e_add_bttstr(7, 14, 1, AltO, " Ok ", NULL, o);
   e_add_bttstr(18, 14, -1, ESC, "Cancel", NULL, o);
   ret = e_opt_kst(o);
   if(ret != ESC)
   {  b_sw = o->pstr[0]->num + (o->pstr[1]->num << 1)
		+ (o->sstr[0]->num << 2);
      b_dif = o->nstr[0]->num;
      e_mk_beauty(b_sw, b_dif, f);
   }
   freeostr(o);
   return(0);
}
#ifdef FREETEST
int sc_txt_2(FENSTER *f)
{
    if(f->c_sw)
    {	if(f->s->ka.y == f->s->ke.y) e_sc_nw_txt(f->s->ke.y, f->b, 0);
	else
	{  f->c_sw = REALLOC(f->c_sw, f->b->mx.y * sizeof(int));
	   f->c_sw = e_sc_txt(f->c_sw, f->b);
	}
    }
    return(0);
}
#endif

#endif


