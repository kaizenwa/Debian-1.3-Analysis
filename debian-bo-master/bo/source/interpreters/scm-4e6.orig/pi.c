/* Copyright (C) 1991 1995 Free Software Foundation, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of GUILE.
 *
 * The exception is that, if you link the GUILE library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name GUILE.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * GUILE, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  
 */

/* "pi.c", program for computing digits of numerical value of PI.
   Author: Aubrey Jaffer

pi <n> <d> prints out <n> digits of pi in groups of <d> digits.

'Spigot' algorithm origionally due to Stanly Rabinowitz.
This algorithm takes time proportional to the square of <n>/<d>.
This fact can make comparisons of computational speed between systems
of vastly differring performances quicker and more accurate.

Try: pi 100 5
The digit size <d> will have to be reduced for larger <n> or an
error due to overflow will occur. */

short *calloc();
main(c,v)
int c;char **v;{
  int n=200,j=0,m,b=2,k=0,t,r=1,d=5;
  long q;
  short *a;
  if(c>1)n=atoi(v[1]);
  if(c>2)d=atoi(v[2]);
  while(k++<d)r=r*10;
  n=n/d+1;
  k=m=3.322*n*d;
  a=calloc(1+m,2);
  while(k)a[--k]=2;
  for(a[m]=4;j<n;b=q%r){
    q=0;
    for(k=m;k;){
      q+=a[k]*r;
      t=(2*k+1);
      a[k]=q%t;
      q=q/t;
      q*=k--;}
    printf("%0*d%s",d,b+q/r,++j%10?"  ":"\n");}
  puts("");}
