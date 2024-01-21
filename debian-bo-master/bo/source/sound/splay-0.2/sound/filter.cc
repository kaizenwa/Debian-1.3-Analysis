/* MPEG/WAVE Sound library

   (C) 1997 by Jung woo-jae */

// Filter.cc
// Subbandsynthesis routines from maplay 1.2 for Linux
// I've modified some macros for reducing source code.

#include "mpegsound.h"

static const REAL MY_PI = 3.14159265358979323846;

static const REAL cos1_64  = 1.0 / (2.0 * cos(MY_PI        / 64.0));
static const REAL cos3_64  = 1.0 / (2.0 * cos(MY_PI * 3.0  / 64.0));
static const REAL cos5_64  = 1.0 / (2.0 * cos(MY_PI * 5.0  / 64.0));
static const REAL cos7_64  = 1.0 / (2.0 * cos(MY_PI * 7.0  / 64.0));
static const REAL cos9_64  = 1.0 / (2.0 * cos(MY_PI * 9.0  / 64.0));
static const REAL cos11_64 = 1.0 / (2.0 * cos(MY_PI * 11.0 / 64.0));
static const REAL cos13_64 = 1.0 / (2.0 * cos(MY_PI * 13.0 / 64.0));
static const REAL cos15_64 = 1.0 / (2.0 * cos(MY_PI * 15.0 / 64.0));
static const REAL cos17_64 = 1.0 / (2.0 * cos(MY_PI * 17.0 / 64.0));
static const REAL cos19_64 = 1.0 / (2.0 * cos(MY_PI * 19.0 / 64.0));
static const REAL cos21_64 = 1.0 / (2.0 * cos(MY_PI * 21.0 / 64.0));
static const REAL cos23_64 = 1.0 / (2.0 * cos(MY_PI * 23.0 / 64.0));
static const REAL cos25_64 = 1.0 / (2.0 * cos(MY_PI * 25.0 / 64.0));
static const REAL cos27_64 = 1.0 / (2.0 * cos(MY_PI * 27.0 / 64.0));
static const REAL cos29_64 = 1.0 / (2.0 * cos(MY_PI * 29.0 / 64.0));
static const REAL cos31_64 = 1.0 / (2.0 * cos(MY_PI * 31.0 / 64.0));
static const REAL cos1_32  = 1.0 / (2.0 * cos(MY_PI        / 32.0));
static const REAL cos3_32  = 1.0 / (2.0 * cos(MY_PI * 3.0  / 32.0));
static const REAL cos5_32  = 1.0 / (2.0 * cos(MY_PI * 5.0  / 32.0));
static const REAL cos7_32  = 1.0 / (2.0 * cos(MY_PI * 7.0  / 32.0));
static const REAL cos9_32  = 1.0 / (2.0 * cos(MY_PI * 9.0  / 32.0));
static const REAL cos11_32 = 1.0 / (2.0 * cos(MY_PI * 11.0 / 32.0));
static const REAL cos13_32 = 1.0 / (2.0 * cos(MY_PI * 13.0 / 32.0));
static const REAL cos15_32 = 1.0 / (2.0 * cos(MY_PI * 15.0 / 32.0));
static const REAL cos1_16  = 1.0 / (2.0 * cos(MY_PI        / 16.0));
static const REAL cos3_16  = 1.0 / (2.0 * cos(MY_PI * 3.0  / 16.0));
static const REAL cos5_16  = 1.0 / (2.0 * cos(MY_PI * 5.0  / 16.0));
static const REAL cos7_16  = 1.0 / (2.0 * cos(MY_PI * 7.0  / 16.0));
static const REAL cos1_8   = 1.0 / (2.0 * cos(MY_PI        / 8.0));
static const REAL cos3_8   = 1.0 / (2.0 * cos(MY_PI * 3.0  / 8.0));
static const REAL cos1_4   = 1.0 / (2.0 * cos(MY_PI / 4.0));

void Mpegtoraw::computebuffer(REAL *fraction,REAL buffer[2][CALCBUFFERSIZE])
{
  REAL newv[32];
  register REAL *x1,*x2,tmp;
  REAL p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,pa,pb,pc,pd,pe,pf;
  REAL q0,q1,q2,q3,q4,q5,q6,q7,q8,q9,qa,qb,qc,qd,qe,qf;

  // compute new values via a fast cosine transform:
  x1=fraction;
  x2=fraction + 31;
  p0=*x1++ + *x2;   p1=*x1++ + *--x2; p2=*x1++ + *--x2; p3=*x1++ + *--x2;
  p4=*x1++ + *--x2; p5=*x1++ + *--x2; p6=*x1++ + *--x2; p7=*x1++ + *--x2;
  p8=*x1++ + *--x2; p9=*x1++ + *--x2; pa=*x1++ + *--x2; pb=*x1++ + *--x2;
  pc=*x1++ + *--x2; pd=*x1++ + *--x2; pe=*x1++ + *--x2; pf=*x1   + *--x2;

  q0=p0+pf;q1=p1+pe;q2=p2+pd;q3=p3+pc;
  q4=p4+pb;q5=p5+pa;q6=p6+p9;q7=p7+p8;
  q8=cos1_32 *(p0-pf);q9=cos3_32 *(p1-pe);
  qa=cos5_32 *(p2-pd);qb=cos7_32 *(p3-pc);
  qc=cos9_32 *(p4-pb);qd=cos11_32*(p5-pa);
  qe=cos13_32*(p6-p9);qf=cos15_32*(p7-p8);

  p0=q0+q7;p1=q1+q6;p2=q2+q5;p3=q3+q4;
  p4=cos1_16*(q0-q7);p5=cos3_16*(q1-q6);
  p6=cos5_16*(q2-q5);p7=cos7_16*(q3-q4);
  p8=q8+qf;p9=q9+qe;pa=qa+qd;pb=qb+qc;
  pc=cos1_16*(q8-qf);
  pd=cos3_16*(q9-qe);
  pe=cos5_16*(qa-qd);
  pf=cos7_16*(qb-qc);

  q0=p0+p3;q1=p1+p2;q2=cos1_8*(p0-p3);q3=cos3_8*(p1-p2);
  q4=p4+p7;q5=p5+p6;q6=cos1_8*(p4-p7);q7=cos3_8*(p5-p6);
  q8=p8+pb;q9=p9+pa;qa=cos1_8*(p8-pb);qb=cos3_8*(p9-pa);
  qc=pc+pf;qd=pd+pe;qe=cos1_8*(pc-pf);qf=cos3_8*(pd-pe);

  p0=q0+q1;p1=cos1_4*(q0-q1);p2=q2+q3;p3=cos1_4*(q2-q3);
  p4=q4+q5;p5=cos1_4*(q4-q5);p6=q6+q7;p7=cos1_4*(q6-q7);
  p8=q8+q9;p9=cos1_4*(q8-q9);pa=qa+qb;pb=cos1_4*(qa-qb);
  pc=qc+qd;pd=cos1_4*(qc-qd);pe=qe+qf;pf=cos1_4*(qe-qf);

  tmp=p6+p7;
  newv[36-17]=-(p5+tmp);
  newv[44-17]=-(p4+tmp);
  tmp=pb+pf;
  newv[10]   =tmp;
  newv[6]    =pd+tmp;
  tmp=pe+pf;
  newv[46-17]=-(p8+pc+tmp);
  newv[34-17]=-(p9+pd+tmp);
  tmp+=pa+pb;
  newv[38-17]=-(pd+tmp);
  newv[42-17]=-(pc+tmp);
  newv[2]    =p9+pd+pf;
  newv[4]    =p5+p7;
  newv[48-17]=-p0;
  newv[0]    =p1;
  newv[8]    =p3;
  newv[12]   =p7;
  newv[14]   =pf;
  newv[40-17]=-(p2+p3);

  x1=fraction;x2=fraction+31;
  p0=cos1_64 *(*x1++ - *x2  ); p1=cos3_64 *(*x1++ - *--x2);
  p2=cos5_64 *(*x1++ - *--x2); p3=cos7_64 *(*x1++ - *--x2);
  p4=cos9_64 *(*x1++ - *--x2); p5=cos11_64*(*x1++ - *--x2);
  p6=cos13_64*(*x1++ - *--x2); p7=cos15_64*(*x1++ - *--x2);
  p8=cos17_64*(*x1++ - *--x2); p9=cos19_64*(*x1++ - *--x2);
  pa=cos21_64*(*x1++ - *--x2); pb=cos23_64*(*x1++ - *--x2);
  pc=cos25_64*(*x1++ - *--x2); pd=cos27_64*(*x1++ - *--x2);
  pe=cos29_64*(*x1++ - *--x2); pf=cos31_64*(*x1   - *--x2);

  q0=p0+pf;q1=p1+pe;q2=p2+pd;q3=p3+pc;
  q4=p4+pb;q5=p5+pa;q6=p6+p9;q7=p7+p8;
  q8=cos1_32 *(p0-pf);q9=cos3_32 *(p1-pe);
  qa=cos5_32 *(p2-pd);qb=cos7_32 *(p3-pc);
  qc=cos9_32 *(p4-pb);qd=cos11_32*(p5-pa);
  qe=cos13_32*(p6-p9);qf=cos15_32*(p7-p8);

  p0=q0+q7;p1=q1+q6;p2=q2+q5;p3=q3+q4;
  p4=cos1_16*(q0-q7);p5=cos3_16*(q1-q6);
  p6=cos5_16*(q2-q5);p7=cos7_16*(q3-q4);
  p8=q8+qf;p9=q9+qe;pa=qa+qd;pb=qb+qc;
  pc=cos1_16*(q8-qf);
  pd=cos3_16*(q9-qe);
  pe=cos5_16*(qa-qd);
  pf=cos7_16*(qb-qc);

  q0=p0+p3;q1=p1+p2;q2=cos1_8*(p0-p3);q3=cos3_8*(p1-p2);
  q4=p4+p7;q5=p5+p6;q6=cos1_8*(p4-p7);q7=cos3_8*(p5-p6);
  q8=p8+pb;q9=p9+pa;qa=cos1_8*(p8-pb);qb=cos3_8*(p9-pa);
  qc=pc+pf;qd=pd+pe;qe=cos1_8*(pc-pf);qf=cos3_8*(pd-pe);

  p0=q0+q1;p1=cos1_4*(q0-q1);
  p2=q2+q3;p3=cos1_4*(q2-q3);
  p4=q4+q5;p5=cos1_4*(q4-q5);
  p6=q6+q7;p7=cos1_4*(q6-q7);
  p8=q8+q9;p9=cos1_4*(q8-q9);
  pa=qa+qb;pb=cos1_4*(qa-qb);
  pc=qc+qd;pd=cos1_4*(qc-qd);
  pe=qe+qf;pf=cos1_4*(qe-qf);

  tmp=pd+pf;
  newv[1]    =p1+p9+tmp;
  newv[5]    =p5+p7+pb+tmp;
  tmp+=p9;
  newv[33-17]=-(p1+pe+tmp);
  tmp+=p5+p7;
  newv[3]    =tmp;
  newv[35-17]=-(p6+pe+tmp);
  tmp=pa+pb+pc+pd+pe+pf;
  newv[39-17]=-(p2+p3+tmp-pc);
  newv[43-17]=-(p4+p6+p7+tmp-pd);
  newv[37-17]=-(p5+p6+p7+tmp-pc);
  newv[41-17]=-(p2+p3+tmp-pd);
  tmp=p8+pc+pe+pf;
  newv[47-17]=-(p0+tmp);
  newv[45-17]=-(p4+p6+p7+tmp);
  tmp=pb+pf;
  newv[11]   =p7+tmp;
  tmp+=p3;
  newv[9]    =tmp;
  newv[7]    =pd+tmp;
  newv[13]   =p7+pf;
  newv[15]   =pf;

  // insert V[0-15] (== newv[0-15]) into actual v:
  x1=newv;x2=buffer[currentcalcbuffer]+calcbufferoffset;
  *x2=*x1++;x2+=16; *x2=*x1++;x2+=16; *x2=*x1++; x2+=16; *x2=*x1++; x2+=16;
  *x2=*x1++;x2+=16; *x2=*x1++;x2+=16; *x2=*x1++; x2+=16; *x2=*x1++; x2+=16;
  *x2=*x1++;x2+=16; *x2=*x1++;x2+=16; *x2=*x1++; x2+=16; *x2=*x1++; x2+=16;
  *x2=*x1++;x2+=16; *x2=*x1++;x2+=16; *x2=*x1++; x2+=16; *x2=*x1;   x2+=16;
  // V[16] is always 0.0:
  *x2=0.0;x2+=16;
  // insert V[17-31] (== -newv[15-1]) into actual v:
  *x2=-*x1;  x2+=16; *x2=-*--x1;x2+=16; *x2=-*--x1;x2+=16; *x2=-*--x1;x2+=16;
  *x2=-*--x1;x2+=16; *x2=-*--x1;x2+=16; *x2=-*--x1;x2+=16; *x2=-*--x1;x2+=16;
  *x2=-*--x1;x2+=16; *x2=-*--x1;x2+=16; *x2=-*--x1;x2+=16; *x2=-*--x1;x2+=16;
  *x2=-*--x1;x2+=16; *x2=-*--x1;x2+=16; *x2=-*--x1;

  // insert V[32] (== -newv[0]) into other v:
  //  x2 = (actual_v == v1 ? v2 : v1) + actual_write_pos;
  x2=buffer[currentcalcbuffer^1]+calcbufferoffset;
  *x2=-*--x1;x2+=16;
  // insert V[33-48] (== newv[16-31]) into other v:
  x1=newv+16;
  *x2=*x1++;x2+=16; *x2=*x1++;x2+=16; *x2=*x1++;x2+=16; *x2=*x1++;x2+=16;
  *x2=*x1++;x2+=16; *x2=*x1++;x2+=16; *x2=*x1++;x2+=16; *x2=*x1++;x2+=16;
  *x2=*x1++;x2+=16; *x2=*x1++;x2+=16; *x2=*x1++;x2+=16; *x2=*x1++;x2+=16;
  *x2=*x1++;x2+=16; *x2=*x1++;x2+=16; *x2=*x1++;x2+=16; *x2=*x1;  x2+=16;
  // insert V[49-63] (== newv[30-16]) into other v:
  *x2=*--x1;x2+=16; *x2=*--x1;x2+=16; *x2=*--x1;x2+=16; *x2=*--x1;x2+=16;
  *x2=*--x1;x2+=16; *x2=*--x1;x2+=16; *x2=*--x1;x2+=16; *x2=*--x1;x2+=16;
  *x2=*--x1;x2+=16; *x2=*--x1;x2+=16; *x2=*--x1;x2+=16; *x2=*--x1;x2+=16;
  *x2=*--x1;x2+=16; *x2=*--x1;x2+=16; *x2=*--x1;
}


#define SAVE \
        raw=(int)(r*scalefactor); \
        if(raw>MAXSCALE)raw=MAXSCALE;else if(raw<MINSCALE)raw=MINSCALE; \
	putraw(raw);
#define OS  r=*vp * *dp++
#define XX  vp+=15;r+=*vp * *dp++
#define OP  r+=*--vp * *dp++

inline void Mpegtoraw::generatesingle(void)
{
  int i;
  register REAL r, *vp;
  register const REAL *dp;
  int raw;

  i=0;
  dp=filter;
  vp=calcbufferL[currentcalcbuffer]+calcbufferoffset; 
// actual_v+actual_write_pos;

  switch (calcbufferoffset)
  {
    case  0:for(;i<32;i++,vp+=15){
              OS;XX;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;
	      SAVE;}break;
    case  1:for(;i<32;i++,vp+=15){
	      OS;OP;XX;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;
	      SAVE;}break;
    case  2:for(;i<32;i++,vp+=15){
              OS;OP;OP;XX;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;
	      SAVE;}break;
    case  3:for(;i<32;i++,vp+=15){
	      OS;OP;OP;OP;XX;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;
	      SAVE;}break;
    case  4:for(;i<32;i++,vp+=15){
              OS;OP;OP;OP;OP;XX;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;
	      SAVE;}break;
    case  5:for(;i<32;i++,vp+=15){
	      OS;OP;OP;OP;OP;OP;XX;OP;OP;OP;OP;OP;OP;OP;OP;OP;
	      SAVE;}break;
    case  6:for(;i<32;i++,vp+=15){
	      OS;OP;OP;OP;OP;OP;OP;XX;OP;OP;OP;OP;OP;OP;OP;OP;
	      SAVE;}break;
    case  7:for(;i<32;i++,vp+=15){
	      OS;OP;OP;OP;OP;OP;OP;OP;XX;OP;OP;OP;OP;OP;OP;OP;
	      SAVE;}break;
    case  8:for(;i<32;i++,vp+=15){
              OS;OP;OP;OP;OP;OP;OP;OP;OP;XX;OP;OP;OP;OP;OP;OP;
	      SAVE;}break;
    case  9:for(;i<32;i++,vp+=15){
	      OS;OP;OP;OP;OP;OP;OP;OP;OP;OP;XX;OP;OP;OP;OP;OP;
	      SAVE;}break;
    case 10:for(;i<32;i++,vp+=15){
              OS;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;XX;OP;OP;OP;OP;
	      SAVE;}break;
    case 11:for(;i<32;i++,vp+=15){
              OS;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;XX;OP;OP;OP;
	      SAVE;}break;
    case 12:for(;i<32;i++,vp+=15){
	      OS;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;XX;OP;OP;
	      SAVE;}break;
    case 13:for(;i<32;i++,vp+=15){
	      OS;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;XX;OP;
	      SAVE;}break;
    case 14:for(;i<32;i++,vp+=15){
	      OS;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;XX;
	      SAVE;}break;
    case 15:for(;i<32;i++,vp+=31){
              OS;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;
	      SAVE;}break;
  }
}

#undef OS
#undef XX
#undef OP
#undef SAVE

#define SAVE \
        raw=(int)(r1*scalefactor);  \
        if(raw>MAXSCALE)raw=MAXSCALE;else if(raw<MINSCALE)raw=MINSCALE; \
	putraw(raw);  \
        raw=(int)(r2*scalefactor);  \
        if(raw>MAXSCALE)raw=MAXSCALE;else if(raw<MINSCALE)raw=MINSCALE; \
	putraw(raw);
#define OS r1=*vp1 * *dp; \
           r2=*vp2 * *dp++ 
#define XX vp1+=15;r1+=*vp1 * *dp; \
	   vp2+=15;r2+=*vp2 * *dp++
#define OP r1+=*--vp1 * *dp; \
	   r2+=*--vp2 * *dp++


inline void Mpegtoraw::generate(void)
{
  int i;
  REAL r1,r2;
  register REAL *vp1,*vp2;
  register const REAL *dp;
  int raw;

  i=0;
  dp=filter;
  vp1=calcbufferL[currentcalcbuffer]+calcbufferoffset; 
  vp2=calcbufferR[currentcalcbuffer]+calcbufferoffset; 
// actual_v+actual_write_pos;

  switch (calcbufferoffset)
  {
    case  0:for(;i<32;i++,vp1+=15,vp2+=15){
              OS;XX;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;
	      SAVE;}break;
    case  1:for(;i<32;i++,vp1+=15,vp2+=15){
	      OS;OP;XX;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;
	      SAVE;}break;
    case  2:for(;i<32;i++,vp1+=15,vp2+=15){
              OS;OP;OP;XX;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;
	      SAVE;}break;
    case  3:for(;i<32;i++,vp1+=15,vp2+=15){
	      OS;OP;OP;OP;XX;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;
	      SAVE;}break;
    case  4:for(;i<32;i++,vp1+=15,vp2+=15){
              OS;OP;OP;OP;OP;XX;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;
	      SAVE;}break;
    case  5:for(;i<32;i++,vp1+=15,vp2+=15){
	      OS;OP;OP;OP;OP;OP;XX;OP;OP;OP;OP;OP;OP;OP;OP;OP;
	      SAVE;}break;
    case  6:for(;i<32;i++,vp1+=15,vp2+=15){
	      OS;OP;OP;OP;OP;OP;OP;XX;OP;OP;OP;OP;OP;OP;OP;OP;
	      SAVE;}break;
    case  7:for(;i<32;i++,vp1+=15,vp2+=15){
	      OS;OP;OP;OP;OP;OP;OP;OP;XX;OP;OP;OP;OP;OP;OP;OP;
	      SAVE;}break;
    case  8:for(;i<32;i++,vp1+=15,vp2+=15){
              OS;OP;OP;OP;OP;OP;OP;OP;OP;XX;OP;OP;OP;OP;OP;OP;
	      SAVE;}break;
    case  9:for(;i<32;i++,vp1+=15,vp2+=15){
	      OS;OP;OP;OP;OP;OP;OP;OP;OP;OP;XX;OP;OP;OP;OP;OP;
	      SAVE;}break;
    case 10:for(;i<32;i++,vp1+=15,vp2+=15){
              OS;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;XX;OP;OP;OP;OP;
	      SAVE;}break;
    case 11:for(;i<32;i++,vp1+=15,vp2+=15){
              OS;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;XX;OP;OP;OP;
	      SAVE;}break;
    case 12:for(;i<32;i++,vp1+=15,vp2+=15){
	      OS;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;XX;OP;OP;
	      SAVE;}break;
    case 13:for(;i<32;i++,vp1+=15,vp2+=15){
	      OS;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;XX;OP;
	      SAVE;}break;
    case 14:for(;i<32;i++,vp1+=15,vp2+=15){
	      OS;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;XX;
	      SAVE;}break;
    case 15:for(;i<32;i++,vp1+=31,vp2+=31){
              OS;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;OP;
	      SAVE;}break;
  }
}


void Mpegtoraw::subbandsynthesis(REAL *fractionL,REAL *fractionR)
{
  computebuffer(fractionL,calcbufferL);
  if(!outputstereo)generatesingle();
  else
  {
    computebuffer(fractionR,calcbufferR);
    generate();
  }

  if(calcbufferoffset<15)calcbufferoffset++;
  else calcbufferoffset=0;

  currentcalcbuffer^=1;
}
