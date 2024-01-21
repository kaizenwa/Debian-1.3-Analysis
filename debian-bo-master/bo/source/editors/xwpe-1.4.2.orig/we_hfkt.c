/* we_hfkt.c                                             */
/* Copyright (C) 1993 Fred Kruse                          */
/* This is free software; you can redistribute it and/or  */
/* modify it under the terms of the                       */
/* GNU General Public License, see the file COPYING.      */

#include "edit.h"

/*
            Buchstaben in Grossbuchstaben umwandeln (deutsch)  */

#if defined(DOS) || defined(DJGPP)
int e_toupper(int c)
{
   if(c >= 'a' && c <= 'z') c = c - 'a' + 'A';
   else if(c == 0x84) c = 0x8e;
   else if(c == 0x94) c = 0x99;
   else if(c == 0x81) c = 0x9a;
   return (c);
}
#else
int e_toupper(c)
     int c;
{
   if(c >= 'a' && c <= 'z') c = c - 'a' + 'A';
   else if(c >= 0xe0 && c <= 0xfe) c = c - 0x20;
   return (c);
}
#endif
int e_strcncmp(s1, s2, n)
     char *s1;
     char *s2;
     int n;
{
   for(; n > 0 && toupper(*s1) == toupper(*s2); n--, s1++, s2++);
   return(n > 0 ? toupper(*s1) - toupper(*s2) : 0);
}

int e_strccmp(s1, s2)
     char *s1;
     char *s2;
{
   while(*s1 && toupper(*s1) == toupper(*s2)) { s1++; s2++; }
   return(toupper(*s1) - toupper(*s2));
}

char *e_strcstr(s1, s2)
     char *s1;
     char *s2;
{
   int i, j;
   for(i = 0; s1[i]; i++)
   {  for(j = 0; s2[j] && toupper(s1[i+j]) == toupper(s2[j]); j++);
      if(!s2[j]) return(s1+i);
      if(!s1[j]) break;
   }
   return(NULL);
}

/*
        String in Zeile finden    */
int e_strstr(x, n, s, f)
     int x;
     int n;
     unsigned char *s;
     unsigned char *f;
{
   int i, j, nf = strlen(f);
   if(x > n)
   {  for(i = x - nf; i >= n; i--)
      {  for(j = 0; j < nf; j++)  if(s[i+j] != f[j]) break;
	 if(j == nf) return(i);
      }
   }
   else
   {  for(i = x >= 0 ? x : 0; i <= n - nf /* && s[i] != '\0' && s[i] != WR */; i++)
      {  for(j = 0; j < nf; j++)  if(s[i+j] != f[j]) break;
	 if(j == nf) return(i);
      }
   }
   return(-1);
}
/*
        String in Zeile finden (ohne Gross- und Kleinschreibung)   */
int e_ustrstr(x, n, s, f)
     int x;
     int n;
     unsigned char *s;
     unsigned char *f;
{
   int i, j, nf = strlen(f);
   if(x > n)
   {  for(i = x-nf; i >= n; i--)
      {  for(j = 0; j < nf; j++)
	 if(e_toupper(s[i+j]) != e_toupper(f[j])) break;
	 if(j == nf) return(i);
      }
   }
   else
   {  for(i = x < 0 ? 0 : x; i <= n - nf; i++)
      {  for(j = 0; j < nf; j++)
	 if(e_toupper(s[i+j]) != e_toupper(f[j])) break;
	 if(j == nf) return(i);
      }
   }
   return(-1);
}
/*
        String in Zeile finden (mit Steuerzeichen)
           (ohne Gross- und Kleinschreibung)                   */
int e_urstrstr(x, n, s, f, nn)
     int x;
     int n;
     unsigned char *s;
     unsigned char *f;
     int *nn;
{
   int i;
   unsigned char far *str;
   unsigned char far *ft = MALLOC((strlen(f)+1)*sizeof(unsigned char));
   if(x <= n)
   {  str = MALLOC((n+1)*sizeof(unsigned char));
      for(i = 0; i < n; i++) str[i] = e_toupper(s[i]);
      str[n] = '\0';
   }
   else
   {  str = MALLOC((x+1)*sizeof(unsigned char));
      for(i = 0; i < x; i++) str[i] = e_toupper(s[i]);
      str[x] = '\0';
   }
   for(i = 0; (ft[i] = e_toupper(f[i])) != '\0'; i++);
   
   i = e_rstrstr(x, n, str, ft, nn);
   FREE(str);
   FREE(ft);
   return(i);
}

/*
        String in Zeile finden (mit Steuerzeichen)     */
int e_rstrstr(x, n, s, f, nn)
     int x;
     int n;
     unsigned char *s;
     unsigned char *f;
     int *nn;
{
   int i, j, jb, k, l, kk, jj, ii, ret, nf;
   for(ii = 0; f[ii] != '\0';)
   {  f += ii;
      for(nf = 0, ret = 0; f[nf] != '\0' && (f[nf] != '|' || ret); nf++)
      {  if(f[nf] == '\\') ret = 1;
	 else if(ret) ret = 0;
      }
      ii = f[nf] ? nf + 1 : nf;
      jb = 0;
      if(f[0] == '^') jb++;
      else if(f[nf-1] == '$' && f[nf-2] != '\\') nf--;
      if(x <= n)
      {  i = x >= 0 ? x : 0;
	 for(; (jb == 0 || i == 0) && i < n; i++)
	 {  for(ret = 0, j = 0, k = 0; jb + j < nf && !ret; j++)
	    {  kk = 0;
	       jj = 0;
	       do
	       {  if(f[jb+j] == '\\')
		  {  if(!kk) jj++; if(s[i+k] != f[jb+j+1]) ret = 1;  }
		  else if(f[jb+j] == '.')
		  {  if((f[jb+j+1] == '*' || f[jb+j+1] == '?') && jb+j < nf-2
	     	    && (!s[i+k] || s[i+k] == f[jb+j+2])) ret = 1;
		     else if(kk && f[jb+j+1] == '+' && jb+j < nf-2
	     	    && (!s[i+k] || s[i+k] == f[jb+j+2])) ret = 1;
		  }
		  else if(f[jb+j] == '[')
		  {  if(f[jb+j+1] == '^')
		     {  for(jj = 2, ret = 0; f[jb+jj+j] != ']' && f[jb+jj+j] != '\0'; jj++)
			{  if(f[jb+jj+j] == '-')
			   {  for(l = f[jb+jj+j-1]; l <= f[jb+jj+j+1]; l++)
			      if(s[i+k] == (unsigned char) l) ret = 1;
			   }
			   else if(s[i+k] == f[jb+jj+j]) ret = 1;
			}
		     }
		     else
		     {  for(jj = 1, ret = 1; f[jb+jj+j] != ']' && f[jb+jj+j] != '\0'; jj++)
			{  if(f[jb+j+jj] == '-')
			   {  for(l = f[jb+jj+j-1]; l <= f[jb+jj+j+1]; l++)
			      if(s[i+k] == (unsigned char) l) ret = 0;
			   }
			   else if(s[i+k] == f[jb+jj+j]) ret = 0;
			}
		     }
		  }
		  else if(s[i+k] != f[jb+j]) ret = 1;
		  if(!ret) {  k++;  kk++;  }
	       }
	       while((f[jb+j+jj+1] == '*' || f[jb+j+jj+1] == '+'
	     	|| (f[jb+j+jj+1] == '?' && kk < 2)) && !ret);
	       j += jj;
	       if(f[jb+j+1] == '*' || (f[jb+j+1] == '+' && kk > 0)
	     			|| (f[jb+j+1] == '?' && kk < 2))
	       {  ret = 0;  j++;  }
	    }
	    if(!ret && jb + j == nf && (f[nf] != '$' || i+k == n))
	    {  *nn = k; return(i);  }
	 }
      }
      else
      {  for(i = !jb ? x-1 : 0; i >= n; i--)
	 {  for(ret = 0, j = 0, k = 0; jb + j < nf && !ret; j++)
	    {  kk = 0;
	       jj = 0;
	       do
	       {  if(f[jb+j] == '\\')
		  {  if(!kk) jj++; if(s[i+k] != f[jb+j+1]) ret = 1;  }
		  else if(f[jb+j] == '.')
		  {  if((f[jb+j+1] == '*' || f[jb+j+1] == '?') && jb+j < nf-2
	     	    && (!s[i+k] || s[i+k] == f[jb+j+2])) ret = 1;
		     else if(kk && f[jb+j+1] == '+' && jb+j < nf-2
	     	    && (!s[i+k] || s[i+k] == f[jb+j+2])) ret = 1;
		  }
		  else if(f[jb+j] == '[')
		  {  if(f[jb+j+1] == '^')
		     {  for(jj = 2, ret = 0; f[jb+jj+j] != ']' && f[jb+jj+j] != '\0'; jj++)
			{  if(f[jb+jj+j] == '-')
			   {  for(l = f[jb+jj+j-1]; l <= f[jb+jj+j+1]; l++)
			      if(s[i+k] == (unsigned char) l) ret = 1;
			   }
			   else if(s[i+k] == f[jb+jj+j]) ret = 1;
			}
		     }
		     else
		     {  for(jj = 1, ret = 1; f[jb+jj+j] != ']' && f[jb+jj+j] != '\0'; jj++)
			{  if(f[jb+j+jj] == '-')
			   {  for(l = f[jb+jj+j-1]; l <= f[jb+jj+j+1]; l++)
			      if(s[i+k] == (unsigned char) l) ret = 0;
			   }
			   else if(s[i+k] == f[jb+jj+j]) ret = 0;
			}
		     }
		  }
		  else if(s[i+k] != f[jb+j]) ret = 1;
		  if(!ret) {  k++;  kk++;  }
	       }
	       while((f[jb+j+jj+1] == '*' || f[jb+j+jj+1] == '+'
	     	|| (f[jb+j+jj+1] == '?' && kk < 2)) && !ret);
	       j += jj;
	       if(f[jb+j+1] == '*' || (f[jb+j+1] == '+' && kk > 0)
	    			|| (f[jb+j+1] == '?' && kk < 2))
	       {  ret = 0;  j++;  }
	    }
	    if(!ret && jb + j == nf && (f[nf] != '$' || i+k == x))
	    {  *nn = k; return(i);  }
	 }
      }
   }
   return(-1);
}
/*
        String in Zeile finden (mit Steuerzeichen)     */
/*
int e_rstrstr(int x, int n, unsigned char far *s, unsigned char *f, int *nn)
{
   int i = x >= 0 ? x : 0, j, jb = 0, k, l, sw = 0, nf = strlen(f);
   if(f[0] == '^') jb++;
   else if(f[nf-1] == '$' && f[nf-2] != '\\') { nf--; i = n - nf; }

   for(; (jb == 0 || i == 0) && i < n; i++)
   {   for(j = 0, k = 0; jb + j < nf; j++, k++)  
       {  if(j > 0 && f[jb+j-1] == '\\')
          {   if(s[i+k] != f[jb+j] && f[jb+j+1] != '*') break;
          }
          else if(f[jb+j] == '\\') k--;    
          else if(f[jb+j] == '.') 
	  {  if(f[jb+j+1] == '*' && f[jb+j+2]) 
	     {  for(; f[jb+j+2] != s[i+k]; k++); k--;  } 
	     else continue;
          else if(f[jb+j] == '[')
          {   if(f[jb+j+1] == '^')
              {   for(j += 2, sw = 1; f[jb+j] != ']' && f[jb+j] != '\0'; j++)
                  {  if(f[jb+j] == '-')
		     {  for(l = f[jb+j-1]; l <= f[jb+j+1]; l++)
				   if(s[i+k] == (char) l) sw = 0;
		     }
		     else if(s[i+k] == f[jb+j]) sw = 0;
		  }
		  if(sw == 0) break;
	      }
	      else
	      {   for(j++, sw = 0; f[jb+j] != ']' && f[jb+j] != '\0'; j++)
		  {  if(f[jb+j] == '-')
		     {  for(l = f[jb+j-1]; l <= f[jb+j+1]; l++)
				   if(s[i+k] == (char) l) sw = 1;
		     }
		     else if(s[i+k] == f[jb+j]) sw = 1;
                  }
                  if(sw == 0) break;
              }
          }
          else if(f[jb+j] == '+' || f[jb+j] == '*')
          {   for(; s[i+k] == f[jb+j-1]; k++);  k--;  }
          else if(s[i+k] != f[jb+j] && f[jb+j+1] != '*') break;
       }
       if(jb + j == nf) { *nn = k; return(i); }
   }
   return(-1);            
}
*/
/*
        String in Zeile finden (mit Steuerzeichen)
           (ohne Gross- und Kleinschreibung)                   
int e_urstrstr(int x, int n, unsigned char far *s, unsigned char *f, int *nn)
{
   int i = x >= 0 ? x : 0, j, jb = 0, k, l, sw = 0, nf = strlen(f);
   if(f[0] == '^') jb++;
   else if(f[nf-1] == '$' && f[nf-2] != '\\') { nf--; i = n - nf; }

   for(; (jb == 0 || i == 0) && i < n; i++)
   {   for(j = 0, k = 0; jb + j < nf; j++, k++)  
       {  if(j > 0 && f[jb+j-1] == '\\')
          {   if(e_toupper(s[i+k]) != e_toupper(f[jb+j]) && f[jb+j+1] != '*') 
                                                                    break;
          }
          else if(f[jb+j] == '\\') k--;    
          else if(f[jb+j] == '.') continue;
          else if(f[jb+j] == '[')
          {   if(f[jb+j+1] == '^')
              {   for(j += 2, sw = 1; f[jb+j] != ']' && f[jb+j] != '\0'; j++)
                  {  if(f[jb+j] == '-')
		     {  for(l = f[jb+j-1]; l <= f[jb+j+1]; l++)
				   if(e_toupper(s[i+k]) == e_toupper(l)) sw = 0;
		     }
		     else if(e_toupper(s[i+k]) == e_toupper(f[jb+j])) sw = 0;
		  }
		  if(sw == 0) break;
	      }
	      else
	      {   for(j++, sw = 0; f[jb+j] != ']' && f[jb+j] != '\0'; j++)
		  {  if(f[jb+j] == '-')
		     {  for(l = f[jb+j-1]; l <= f[jb+j+1]; l++)
				   if(e_toupper(s[i+k]) == e_toupper(l)) sw = 1;
		     }
		     else if(e_toupper(s[i+k]) == e_toupper(f[jb+j])) sw = 1;
                  }
                  if(sw == 0) break;
              }
          }
          else if(f[jb+j] == '+' || f[jb+j] == '*')
          {  if(f[jb+j] == '*') k--; 
             for(; e_toupper(s[i+k]) == e_toupper(f[jb+j-1]); k++);  k--;  }
          else if(e_toupper(s[i+k]) != e_toupper(f[jb+j]) && f[jb+j+1] != '*') break;
       }
       if(jb + j == nf) { *nn = k; return(i); }
   }
   return(-1);
}
*/
/*
         Zahlenkasten  (Zahleneingabe)     */

int e_num_kst(s, num, max, f, n, sw)
     char *s;
     int num;
     int max;
     FENSTER *f;
     int n;
     int sw;
{
   int ret, nz = e_anz_zif(max);
   char *tmp = MALLOC((strlen(s)+2) * sizeof(char));
   W_OPTSTR *o = e_init_opt_kst(f);
   if(!o || !tmp) return(-1);
   o->xa = 20;  o->ya = 4;  o->xe = 52;  o->ye = 10;
   o->bgsw = 0;
   o->name = s;
   o->crsw = AltO;
   sprintf(tmp, "%s:", s);
   e_add_numstr(3, 2, 29-nz, 2, nz, max, n, sw, tmp, num, o);
   FREE(tmp);
   e_add_bttstr(6, 4, 1, AltO, " Ok ", NULL, o);
   e_add_bttstr(21, 4, -1, ESC, "Cancel", NULL, o);
   ret = e_opt_kst(o);
   if(ret != ESC) num = o->nstr[0]->num;
   freeostr(o);
   return(num);
}
/*
{
#if  MOUSE
    extern struct mouse e_mouse;
#endif
    PIC *pic = NULL;
    int nz = e_anz_zif(max), c = 255, xt = num;
    int ns = strlen(s);
    int xa = 35, ya = 5, xe = xa +ns + nz + 4, ye = ya + 2;
    pic = e_std_kst(xa, ya, xe, ye, NULL, 1, f->fb->nr.fb, f->fb->ne.fb);
    if(pic == NULL) {  e_error(e_msg[0], 0, f->fb); return(-1); }
    e_pr_str(xa+2, ya+1, s, f->fb->nt.fb, 0, 0, 0, 0);
#if  MOUSE
    while(e_mshit() != 0);
    while ( c != CR && c != ESC && c != -2 &&
            (c != -1 || e_mouse.y != ya || e_mouse.x != xa+3) )
#else
    while ( c != CR && c != ESC)
#endif
      c =  e_schreib_zif(&xt, xe-nz-1, ya+1, nz, f->fb->ft.fb, f->fb->fz.fb);
    if( c != -2 && c != ESC ) num = xt;
#if  MOUSE
    while(e_mshit() != 0);
#endif
    e_close_view(pic, 1);
    return(num);
}
*/
/*
         Anzahl der Ziffern einer Zahl   */

int e_anz_zif(n)
     int n;
{
   int i = 0;
   if(n == 0) return(1);
   else if(n < 0) n = -n;
   while (n > 0) {  n /= 10; i++;  }
   return(i);
}
/*
         Zahl in Ziffern zerlegen    */

char *e_numtozif(z, n, s)
     int z;
     int n;
     char *s;
{
   int zf = z, i, nz = e_anz_zif(z);
   for (i = nz; i < n; i++) s[i-nz] = 32;
   while (nz > 0)
   {  for (zf = z, i = 1; i < nz; i++) zf /= 10;
      s[n-nz] = zf+'0';
      for (i = 1; i < nz; i++) zf *= 10;
      z -= zf;
      nz--;
   }
   s[n] = '\0';
   return(s);
}
/*
         Zahl aus Ziffern bilden    */

int e_ziftonum(s)
     char *s;
{
   int i, n = 0;
   for (i = 0; s[i] < '1' || s[i] > '9'; i++);
   for (; s[i] >= '0' && s[i] <= '9'; i++)
   n = n*10 + s[i] - '0';
   return(n);
}
/*
      L„nge eines Strings bestimmen    */
int e_str_len(s)
     unsigned char *s;
{
   int i;
   for(i = 0; *(s+i) != '\0' && *(s+i) != WR && *(s+i) != CR; i++);
   return (i);
}
/*
      Anzahl der Character eines Strings bestimmen    */
int e_str_nrc(s)
     unsigned char *s;
{
   int i;
   for(i = 0; *(s+i) != '\0'; i++);
   return (i);
}
/*
	     COLOR - Struktur mit Konstanten belegen           */

COLOR e_s_x_clr(f, b)
     int f;
     int b;
{
   COLOR c;
   c.f = f;
   c.b = b;
   c.fb = 16*b + f;
   return(c);
}

COLOR e_n_x_clr(fb)
     int fb;
{
   COLOR f;
   f.fb = fb;
   f.b = fb/16;
   f.f = fb%16;
   return(f);
}

#if !defined(DOS) && !defined(DJGPP)

COLOR e_s_t_clr(f, b)
     int f;
     int b;
{
   COLOR c;
   c.f = f;
   c.b = b;
   c.fb = f;
   return(c);
}

COLOR e_n_t_clr(fb)
     int fb;
{
   COLOR f;
   f.fb = fb;
   f.b = fb;
   f.f = fb;
   return(f);
}

#endif

/*
            PUNKT - Struktur mit Konstanten belegen            */

/*  include "edit.h"  */

PUNKT e_s_pkt(x, y)
     int x;
     int y;
{
   PUNKT p;
   p.x = x;
   p.y = y;
   return(p);
}

/*   String mit Blanks fuellen   */

char *e_blktostr(s, n)
     char *s;
     int n;
{
   int i;
   for(i = 0; i < n; i++) s[i] = ' ';
   s[n] = '\0';
   return(s);
}

int e_pr_uul(fb)
     FARBE *fb;
{
   extern WOPT *blst;
   extern int nblst;
   int i;
   e_blk(MAXSCOL, 0, MAXSLNS-1, fb->mt.fb);
   for (i = 0; i < nblst && blst[i].x < MAXSCOL; ++i)
   e_pr_str_scan(blst[i].x+1, MAXSLNS-1, blst[i].t, fb->mt.fb,
			blst[i].s, blst[i].n, fb->ms.fb, blst[i].x,
			i == nblst-1 ? MAXSCOL-1 : blst[i+1].x-1);
   return(i);
}

#ifndef UNIX
int e_pr_mtul(char *text, int n, int col)
{
   char tmp[80];
   sprintf(tmp, "%s %d", text, n);
   e_pr_nstr(1, MAXSLNS - 1, MAXSCOL - 1, tmp, col, col);
   return(0);
}

int e_pr_ul(FARBE *fb)
{
   extern WOPT *blst;
   extern int nblst;
   int i;
   for (i = 0; i < nblst && blst[i].x < MAXSCOL; ++i)
   e_pr_str(blst[i].x+1, MAXSLNS-1, blst[i].t, fb->mt.fb, blst[i].s,
                                              blst[i].n, fb->ms.fb, 0);
   return(i);
}
#endif

char *e_make_string(p, str)
     char *p;
     char *str;
{
   int len;
   if(p) FREE(p);
   if(str == NULL)
   {  if((p = MALLOC(2)) == NULL) return(NULL);
      p[0] = '\0';
   }
   else
   {  if((len = strlen(str)) < 2) len = 2;
      if((p = MALLOC(len+1)) == NULL) return(NULL);
      strcpy(p, str);
   }
   return(p);
}



