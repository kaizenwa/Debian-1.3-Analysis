/* MPEG/WAVE Sound library

   (C) 1997 by Jung woo-jae */

// Mpeglayer3.cc
// It's for MPEG Layer 3
// I've made array for superior functions for speed.
// Extend TO_FOUR_THIRDS to negative.
// Bug fix : maplay 1.2+ have wrong TO_FOUR_THIRDS ranges.
// Force to mono!!

#include <math.h>
#include <stdlib.h>

#include "mpegsound.h"

#define REAL0 0

#define ARRAYSIZE (SBLIMIT*SSLIMIT)
#define REALSIZE (sizeof(REAL))

#ifdef PI
#undef PI
#endif
#define PI   3.141593
#define PI12 0.2617994
#define PI18 0.17453293
#define PI24 0.1308997
#define PI36 0.08726646
#define PI72 0.04363323

typedef struct
{
  int l[23];
  int s[14];
}SFBANDINDEX;

// This is using inline assembly for intel x86 machines.
#define NATIVE_ASSEMBLY

#ifdef NATIVE_ASSEMBLY
inline void long_memset(void * s,unsigned int c,int count)
{
__asm__ __volatile__(
  "cld\n\t"
  "rep ; stosl\n\t"
  : /* no output */
  :"a" (c), "c" (count/4), "D" ((long) s)
  :"cx","di","memory");
}

#endif

#define FOURTHIRDSTABLENUMBER (1<<13)

static bool initializedlayer3=false;
static REAL two_to_negative_half_pow[40];
static REAL TO_FOUR_THIRDSTABLE[FOURTHIRDSTABLENUMBER*2];
static REAL POW2[256];
static REAL POW2_1[8][2][16];
static REAL ca[8],cs[8];
static REAL win[4][36];
static REAL NATIVECOS[9*8];

static REAL TAN12[16]=
{ 0.0,        0.26794919, 0.57735027  , 1.0,
  1.73205081, 3.73205081, 9.9999999e10,-3.73205081,
 -1.73205081,-1.0,       -0.57735027,  -0.26794919,
  0.0,        0.26794919, 0.57735027,   1.0};
static REAL Ci[8]=
{-0.6f,-0.535f,-0.33f,-0.185f,-0.095f,-0.041f,-0.0142f,-0.0037f};

void Mpegtoraw::layer3initialize(void)
{
  layer3framestart=0;
  
  {
    int i,j,k;
  
    for(i=0;i<2;i++)
      for(j=0;j<SBLIMIT;j++)
	for(k=0;k<SSLIMIT;k++)
	  prevblck[i][j][k]=0.0f;
  }

  bitwindow.initialize();

  if(initializedlayer3)return;

  for(int i=0;i<40;i++)
    two_to_negative_half_pow[i]=(REAL)pow(2.0,-0.5*(double)i);
  {
    REAL *TO_FOUR_THIRDS=TO_FOUR_THIRDSTABLE+FOURTHIRDSTABLENUMBER;

    for(int i=0;i<FOURTHIRDSTABLENUMBER;i++)
      TO_FOUR_THIRDS[-i]=
	-(TO_FOUR_THIRDS[i]=(REAL)pow((double)i,4.0/3.0));
  }
  for(int i=0;i<256;i++)POW2[i]=(REAL)pow(2.0,(0.25*(i-210.0)));
  for(int i=0,j,k;i<8;i++)
    for(j=0;j<2;j++)
      for(k=0;k<16;k++)POW2_1[i][j][k]=pow(2.0,(-2.0*i)-(0.5*(1.0+j)*k));
  {
    REAL sq;
    for(int i=0;i<8;i++)
    {
      sq=sqrt(1.0f+Ci[i]*Ci[i]);
      cs[i]=1.0f/sq;
      ca[i]=Ci[i]*cs[i];
    }
  }
  {
    register int i;

    /* type 0 */
    for(i=0;i<36 ;i++)win[0][i]=(REAL)sin(PI36*(i+0.5));
    /* type 1*/
    for(i=0 ;i<18;i++)win[1][i]=(REAL)sin(PI36*(i+0.5));
    for(i=18;i<24;i++)win[1][i]=1.0f;
    for(i=24;i<30;i++)win[1][i]=(REAL)sin(PI12*(i+0.5-18));
    for(i=30;i<36;i++)win[1][i]=0.0f;
    /* type 2 (not needed anymore) */
    /* for(i=0;i<12;i++) win[2][i]=REAL(sin(PI12*(i+0.5)));
       for(i=12;i<36;i++)win[2][i]=0.0f;*/
    /* type 3*/
    for(i= 0;i<6 ;i++)win[3][i]=0.0f;
    for(i= 6;i<12;i++)win[3][i]=(REAL)sin(PI12*(i+0.5-6.0));
    for(i=12;i<18;i++)win[3][i]=1.0f;
    for(i=18;i<36;i++)win[3][i]=(REAL)sin(PI36*(i+0.5));

    for(i=0;i<4;i++)
    {
      win[i][ 0]*= 0.740093616f ;win[i][ 1]*= 0.821339815f;
      win[i][ 2]*= 0.930579498f ;win[i][ 3]*= 1.082840285f;
      win[i][ 4]*= 1.306562965f ;win[i][ 5]*= 1.662754762f;
      win[i][ 6]*= 2.310113158f ;win[i][ 7]*= 3.830648788f;
      win[i][ 8]*=11.46279281f  ;
                                 win[i][ 9]*=-11.46279281f ;
      win[i][10]*= -3.830648788f;win[i][11]*= -2.310113158f;
      win[i][12]*= -1.662754762f;win[i][13]*= -1.306562965f;
      win[i][14]*= -1.082840285f;win[i][15]*= -0.930579498f;
      win[i][16]*= -0.821339815f;win[i][17]*= -0.740093616f;
      win[i][18]*= -0.678170852f;win[i][19]*= -0.630236207f;
      win[i][20]*= -0.592844523f;win[i][21]*= -0.563690973f;
      win[i][22]*= -0.541196100f;win[i][23]*= -0.524264562f;
      win[i][24]*= -0.512139757f;win[i][25]*= -0.504314480f;
      win[i][26]*= -0.500476342f;win[i][27]*= -0.500476342f;
      win[i][28]*= -0.504314480f;win[i][29]*= -0.512139757f;
      win[i][30]*= -0.524264562f;win[i][31]*= -0.541196100f;
      win[i][32]*= -0.563690973f;win[i][33]*= -0.592844523f;
      win[i][34]*= -0.630236207f;win[i][35]*= -0.678170852f;
    }
  }    
  for(int i=0;i<9;i++)
    for(int j=0,t=(i<<1)+1;j<8;j++)
      NATIVECOS[(i<<3)+j]=(REAL)cos(PI18*t*(j+1));

  initializedlayer3=true;
}

inline bool Mpegtoraw::layer3getsideinfo(void)
{
  sideinfo.main_data_begin=getbits(9);

  if(!inputstereo)sideinfo.private_bits=getbits(5);
  else sideinfo.private_bits=getbits(3);
  
    sideinfo.ch[LS].scfsi[0]=getbit();
    sideinfo.ch[LS].scfsi[1]=getbit();
    sideinfo.ch[LS].scfsi[2]=getbit();
    sideinfo.ch[LS].scfsi[3]=getbit();
  if(inputstereo)
  {
    sideinfo.ch[RS].scfsi[0]=getbit();
    sideinfo.ch[RS].scfsi[1]=getbit();
    sideinfo.ch[RS].scfsi[2]=getbit();
    sideinfo.ch[RS].scfsi[3]=getbit();
  }

  for(int gr=0,ch;gr<2;gr++)
    for(ch=0;;ch++)
    {
      layer3grinfo *gi=&(sideinfo.ch[ch].gr[gr]);

      gi->part2_3_length       =getbits(12);
      gi->big_values           =getbits(9);
      gi->global_gain          =getbits(8);
      gi->scalefac_compress    =getbits(4);
      gi->window_switching_flag=getbit();
      if(gi->window_switching_flag)
      {
	gi->block_type      =getbits(2);
	gi->mixed_block_flag=getbit();
	
	gi->table_select[0] =getbits(5);
	gi->table_select[1] =getbits(5);
	
	gi->subblock_gain[0]=getbits(3);
	gi->subblock_gain[1]=getbits(3);
	gi->subblock_gain[2]=getbits(3);
	
	/* Set region_count parameters since they are implicit in this case. */
	if(gi->block_type==0)
	{
	  /* printf("Side info bad: block_type == 0 in split block.\n");
	     exit(0); */
	  return false;
	}
	else if (gi->block_type==2 && gi->mixed_block_flag==0)
	     gi->region0_count=8; /* MI 9; */
	else gi->region0_count=7; /* MI 8; */
	gi->region1_count=20-(gi->region0_count);
      }
      else
      {
	gi->table_select[0] =getbits(5);
	gi->table_select[1] =getbits(5);
	gi->table_select[2] =getbits(5);
	gi->region0_count   =getbits(4);
	gi->region1_count   =getbits(3);
	gi->block_type      =0;
      }
      gi->preflag           =getbit();
      gi->scalefac_scale    =getbit();
      gi->count1table_select=getbit();

      gi->generalflag=gi->window_switching_flag && (gi->block_type==2);

      if(!inputstereo || ch)break;
    }

  return true;
}

static int slen[2][16]={{0, 0, 0, 0, 3, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4},
			{0, 1, 2, 3, 0, 1, 2, 3, 1, 2, 3, 1, 2, 3, 2, 3}};

void Mpegtoraw::layer3getscalefactors(int ch,int gr)
{
  layer3grinfo *gi=&(sideinfo.ch[ch].gr[gr]);
  register layer3scalefactor *sf=(&scalefactors[ch]);
  int l0,l1;
  {
    int scale_comp=gi->scalefac_compress;

    l0=slen[0][scale_comp];
    l1=slen[1][scale_comp];
  }  
  if(gi->generalflag)
  {
    if(gi->mixed_block_flag)
    {                                 /* MIXED */ /* NEW-ag 11/25 */
      sf->l[0]=wgetbits(l0);sf->l[1]=wgetbits(l0);
      sf->l[2]=wgetbits(l0);sf->l[3]=wgetbits(l0);
      sf->l[4]=wgetbits(l0);sf->l[5]=wgetbits(l0);
      sf->l[6]=wgetbits(l0);sf->l[7]=wgetbits(l0);

      sf->s[0][ 3]=wgetbits(l0);sf->s[1][ 3]=wgetbits(l0);
      sf->s[2][ 3]=wgetbits(l0);
      sf->s[0][ 4]=wgetbits(l0);sf->s[1][ 4]=wgetbits(l0);
      sf->s[2][ 4]=wgetbits(l0);
      sf->s[0][ 5]=wgetbits(l0);sf->s[1][ 5]=wgetbits(l0);
      sf->s[2][ 5]=wgetbits(l0);

      sf->s[0][ 6]=wgetbits(l1);sf->s[1][ 6]=wgetbits(l1);
      sf->s[2][ 6]=wgetbits(l1);
      sf->s[0][ 7]=wgetbits(l1);sf->s[1][ 7]=wgetbits(l1);
      sf->s[2][ 7]=wgetbits(l1);
      sf->s[0][ 8]=wgetbits(l1);sf->s[1][ 8]=wgetbits(l1);
      sf->s[2][ 8]=wgetbits(l1);
      sf->s[0][ 9]=wgetbits(l1);sf->s[1][ 9]=wgetbits(l1);
      sf->s[2][ 9]=wgetbits(l1);
      sf->s[0][10]=wgetbits(l1);sf->s[1][10]=wgetbits(l1);
      sf->s[2][10]=wgetbits(l1);
      sf->s[0][11]=wgetbits(l1);sf->s[1][11]=wgetbits(l1);
      sf->s[2][11]=wgetbits(l1);

      sf->s[0][12]=sf->s[1][12]=sf->s[2][12]=0;
    }
    else 
    {  /* SHORT*/
      sf->s[0][ 0]=wgetbits(l0);sf->s[1][ 0]=wgetbits(l0);
      sf->s[2][ 0]=wgetbits(l0);
      sf->s[0][ 1]=wgetbits(l0);sf->s[1][ 1]=wgetbits(l0);
      sf->s[2][ 1]=wgetbits(l0);
      sf->s[0][ 2]=wgetbits(l0);sf->s[1][ 2]=wgetbits(l0);
      sf->s[2][ 2]=wgetbits(l0);
      sf->s[0][ 3]=wgetbits(l0);sf->s[1][ 3]=wgetbits(l0);
      sf->s[2][ 3]=wgetbits(l0);
      sf->s[0][ 4]=wgetbits(l0);sf->s[1][ 4]=wgetbits(l0);
      sf->s[2][ 4]=wgetbits(l0);
      sf->s[0][ 5]=wgetbits(l0);sf->s[1][ 5]=wgetbits(l0);
      sf->s[2][ 5]=wgetbits(l0);

      sf->s[0][ 6]=wgetbits(l1);sf->s[1][ 6]=wgetbits(l1);
      sf->s[2][ 6]=wgetbits(l1);
      sf->s[0][ 7]=wgetbits(l1);sf->s[1][ 7]=wgetbits(l1);
      sf->s[2][ 7]=wgetbits(l1);
      sf->s[0][ 8]=wgetbits(l1);sf->s[1][ 8]=wgetbits(l1);
      sf->s[2][ 8]=wgetbits(l1);
      sf->s[0][ 9]=wgetbits(l1);sf->s[1][ 9]=wgetbits(l1);
      sf->s[2][ 9]=wgetbits(l1);
      sf->s[0][10]=wgetbits(l1);sf->s[1][10]=wgetbits(l1);
      sf->s[2][10]=wgetbits(l1);
      sf->s[0][11]=wgetbits(l1);sf->s[1][11]=wgetbits(l1);
      sf->s[2][11]=wgetbits(l1);

      sf->s[0][12]=sf->s[1][12]=sf->s[2][12]=0;
    }
  }
  else
  {   /* LONG types 0,1,3 */
    if((sideinfo.ch[ch].scfsi[0]==0) || (gr==0))
    {
      sf->l[ 0]=wgetbits(l0);sf->l[ 1]=wgetbits(l0);
      sf->l[ 2]=wgetbits(l0);sf->l[ 3]=wgetbits(l0);
      sf->l[ 4]=wgetbits(l0);sf->l[ 5]=wgetbits(l0);
    }
    if((sideinfo.ch[ch].scfsi[1]==0) || (gr==0))
    {
      sf->l[ 6]=wgetbits(l0);sf->l[ 7]=wgetbits(l0);
      sf->l[ 8]=wgetbits(l0);sf->l[ 9]=wgetbits(l0);
      sf->l[10]=wgetbits(l0);
    }
    if((sideinfo.ch[ch].scfsi[2]==0) || (gr==0))
    {
      sf->l[11]=wgetbits(l1);sf->l[12]=wgetbits(l1);
      sf->l[13]=wgetbits(l1);sf->l[14]=wgetbits(l1);
      sf->l[15]=wgetbits(l1);
    }
    if((sideinfo.ch[ch].scfsi[3]==0) || (gr==0))
    {
      sf->l[16]=wgetbits(l1);sf->l[17]=wgetbits(l1);
      sf->l[18]=wgetbits(l1);sf->l[19]=wgetbits(l1);
      sf->l[20]=wgetbits(l1);
    }
    sf->l[21]=sf->l[22]=0;
  }
}

static SFBANDINDEX sfBandIndextable[3]=
{
{{0,4,8,12,16,20,24,30,36,44,52,62,74,90,110,134,162,196,238,288,342,418,576},
 {0,4,8,12,16,22,30,40,52,66,84,106,136,192}},
{{0,4,8,12,16,20,24,30,36,42,50,60,72,88,106,128,156,190,230,276,330,384,576},
  {0,4,8,12,16,22,28,38,50,64,80,100,126,192}},
{{0,4,8,12,16,20,24,30,36,44,54,66,82,102,126,156,194,240,296,364,448,550,576},
  {0,4,8,12,16,22,30,42,58,78,104,138,180,192}}
};


void Mpegtoraw::layer3huffmandecode(int ch,int gr,int out[SBLIMIT][SSLIMIT])
{
  layer3grinfo *gi=&(sideinfo.ch[ch].gr[gr]);
  int part2_3_end=layer3part2start+(gi->part2_3_length);
  int region1Start,region2Start;
  int i,e=gi->big_values<<1;

  /* Find region boundary for short block case. */
  if(gi->generalflag)
  {
    /* Region2. */
    region1Start=36; /* sfb[9/3]*3=36 */
    region2Start=576;/* No Region2 for short block case. */
  }
  else
  {          /* Find region boundary for long block case. */
    region1Start=sfBandIndextable[frequency].l[gi->region0_count+1];/* MI */
    region2Start=sfBandIndextable[frequency].l[gi->region0_count+
                                             gi->region1_count+2];/* MI */
  }

  //  huffmanframe++;
  /* Read bigvalues area. */
  for(i=0;i<e;)
  {
    int x,y,g;
    const HUFFMANCODETABLE *h;
      
    if     (i<region1Start)h=&ht[gi->table_select[0]];
    else if(i<region2Start)h=&ht[gi->table_select[1]];
    else                   h=&ht[gi->table_select[2]];
    
    huffmandecoder(h,&x,&y,&g,&g);

    out[0][i]=x;i++;
    out[0][i]=y;i++;
  }

  /* Read count1 area. */
  const HUFFMANCODETABLE *h=&ht[gi->count1table_select+32];
  while(bitwindow.gettotalbit()<part2_3_end)
  {
    int x,y,v,w;

    huffmandecoder(h,&x,&y,&v,&w);
    out[0][i]=v;i++;out[0][i]=w;i++;
    out[0][i]=x;i++;out[0][i]=y;i++;
    if(i>=ARRAYSIZE)
    {
      bitwindow.rewind(bitwindow.gettotalbit()-part2_3_end);
      return;
    }
  }
  

#ifdef NATIVE_ASSEMBLY
  long_memset(&out[0][i],0,
              (ARRAYSIZE-i)*sizeof(int));
#else
  for(;i<ARRAYSIZE;i++)out[0][i]=0;
#endif
  bitwindow.rewind(bitwindow.gettotalbit()-part2_3_end);
}


static int pretab[22]={0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,2,2,3,3,3,2,0};

inline REAL Mpegtoraw::layer3twopow2(int scale,int preflag,
				     int pretab_offset,int l)
{
  int index=l;
  
  if(preflag)index+=pretab_offset;
  return(two_to_negative_half_pow[index<<scale]);
}

inline REAL Mpegtoraw::layer3twopow2_1(int a,int b,int c)
{
  return POW2_1[a][b][c];
}

void Mpegtoraw::layer3dequantizesample(int ch,int gr,
				       int   in[SBLIMIT][SSLIMIT],
				       REAL out[SBLIMIT][SSLIMIT])
{
  layer3grinfo *gi=&(sideinfo.ch[ch].gr[gr]);
  SFBANDINDEX *sfBandIndex=&(sfBandIndextable[frequency]);
  
  /* Compute overall (global) scaling. */
  {
    REAL *TO_FOUR_THIRDS=TO_FOUR_THIRDSTABLE+FOURTHIRDSTABLENUMBER;
    REAL temp=POW2[gi->global_gain];

    for(int sb=0;sb<SBLIMIT;sb++)
    {
      int *i=in[sb];
      REAL *o=out[sb];

      o[ 0]=temp*TO_FOUR_THIRDS[i[ 0]];o[ 1]=temp*TO_FOUR_THIRDS[i[ 1]];
      o[ 2]=temp*TO_FOUR_THIRDS[i[ 2]];o[ 3]=temp*TO_FOUR_THIRDS[i[ 3]];
      o[ 4]=temp*TO_FOUR_THIRDS[i[ 4]];o[ 5]=temp*TO_FOUR_THIRDS[i[ 5]];
      o[ 6]=temp*TO_FOUR_THIRDS[i[ 6]];o[ 7]=temp*TO_FOUR_THIRDS[i[ 7]];
      o[ 8]=temp*TO_FOUR_THIRDS[i[ 8]];o[ 9]=temp*TO_FOUR_THIRDS[i[ 9]];
      o[10]=temp*TO_FOUR_THIRDS[i[10]];o[11]=temp*TO_FOUR_THIRDS[i[11]];
      o[12]=temp*TO_FOUR_THIRDS[i[12]];o[13]=temp*TO_FOUR_THIRDS[i[13]];
      o[14]=temp*TO_FOUR_THIRDS[i[14]];o[15]=temp*TO_FOUR_THIRDS[i[15]];
      o[16]=temp*TO_FOUR_THIRDS[i[16]];o[17]=temp*TO_FOUR_THIRDS[i[17]];
    }
  }

  /* choose correct scalefactor band per block type, initalize boundary */
  /* and apply formula per block type */
  
  if(!gi->generalflag)
  {
    int next_cb_boundary=sfBandIndex->l[1]; /* LONG blocks: 0,1,3 */
    int cb=0;
  
    for(int index=0;index<ARRAYSIZE;index++)
    {
      if(index==next_cb_boundary) /* Adjust critical band boundary */
	next_cb_boundary=sfBandIndex->l[(++cb)+1];

      /* LONG block types 0,1,3 & 1st 2 subbands of switched blocks */
      out[0][index]*=layer3twopow2(gi->scalefac_scale,gi->preflag,
				   pretab[cb],scalefactors[ch].l[cb]);
    }
  }
  else if(!gi->mixed_block_flag)
  {
    int cb=0,cb_begin=0;
    int cb_width=sfBandIndex->s[1];
    int next_cb_boundary=(cb_width<<2)-cb_width;

    for(int index=0;index<ARRAYSIZE;index++)
    {
      if(index==next_cb_boundary)
      { /* Adjust critical band boundary */
	next_cb_boundary=sfBandIndex->s[(++cb)+1];
	next_cb_boundary=(next_cb_boundary<<2)-next_cb_boundary;
	cb_begin=sfBandIndex->s[cb];
	cb_width=sfBandIndex->s[cb+1]-cb_begin;
	cb_begin=(cb_begin<<2)-cb_begin;
      }
      /* Do long/short dependent scaling operations. */
      {
	int t_index=(index-cb_begin)/cb_width;
	out[0][index]*=layer3twopow2_1(gi->subblock_gain[t_index],
				       gi->scalefac_scale,
				       scalefactors[ch].s[t_index][cb]);
      }
    }
  }
  else
  {
    int cb_begin=0,cb_width=0;
    int cb=0;
    int next_cb_boundary=sfBandIndex->l[1]; /* LONG blocks: 0,1,3 */
    int index;

    for(index=0;index<SSLIMIT*2;index++)
    {
      if(index==next_cb_boundary)
      {
	if(index==sfBandIndex->l[8])
	{
	  next_cb_boundary=sfBandIndex->s[4];
	  next_cb_boundary=(next_cb_boundary<<2)-next_cb_boundary;
	  cb=3;
	  cb_width=sfBandIndex->s[4]-sfBandIndex->s[3];
	  cb_begin=sfBandIndex->s[3];
	  cb_begin=(cb_begin<<2)-cb_begin;
	}
	else if(index<sfBandIndex->l[8])
	  next_cb_boundary=sfBandIndex->l[(++cb)+1];
	else 
	{
	  next_cb_boundary=sfBandIndex->s[(++cb)+1];
	  next_cb_boundary=(next_cb_boundary<<2)-next_cb_boundary;
	  cb_begin=sfBandIndex->s[cb];
	  cb_width=sfBandIndex->s[cb+1]-cb_begin;
	  cb_begin=(cb_begin<<2)-cb_begin;
	}
      }
      /* LONG block types 0,1,3 & 1st 2 subbands of switched blocks */
      out[0][index]*=layer3twopow2(gi->scalefac_scale,gi->preflag,
				   pretab[cb],scalefactors[ch].l[cb]);
    }
    for(;index<ARRAYSIZE;index++)
    { 
      if(index==next_cb_boundary)
      {
	if(index==sfBandIndex->l[8])
	{
	  next_cb_boundary=sfBandIndex->s[4];
	  next_cb_boundary=(next_cb_boundary<<2)-next_cb_boundary;
	  cb=3;
	  cb_width=sfBandIndex->s[4]-sfBandIndex->s[3];
	  cb_begin=sfBandIndex->s[3];
	  cb_begin=(cb_begin<<2)-cb_begin;
	}
	else if(index<sfBandIndex->l[8])
	  next_cb_boundary=sfBandIndex->l[(++cb)+1];
	else 
	{
	  next_cb_boundary=sfBandIndex->s[(++cb)+1];
	  next_cb_boundary=(next_cb_boundary<<2)-next_cb_boundary;
	  cb_begin=sfBandIndex->s[cb];
	  cb_width=sfBandIndex->s[cb+1]-cb_begin;
	  cb_begin=(cb_begin<<2)-cb_begin;
	}
      }
      {
	int t_index=(index-cb_begin)/cb_width;
	out[0][index]*=layer3twopow2_1(gi->subblock_gain[t_index],
				       gi->scalefac_scale,
				       scalefactors[ch].s[t_index][cb]);
      }
    }
  }
}

inline \
void Mpegtoraw::layer3fixtostereo(int gr,REAL  in[2][SBLIMIT][SSLIMIT],
				         REAL out[2][SBLIMIT][SSLIMIT])
{
  layer3grinfo *gi=&(sideinfo.ch[0].gr[gr]);
  SFBANDINDEX *sfBandIndex=&(sfBandIndextable[frequency]);
  
  int ms_stereo=(mode==joint) && (extendedmode & 0x2);
  int i_stereo =(mode==joint) && (extendedmode & 0x1);
  int sfb;
  int i,j,sb,ss,is_pos[576];
  int lines,temp,temp2;
  REAL is_ratio[576];

  if(!inputstereo)
  { /* mono , bypass xr[0][][] to lr[0][][]*/
    memcpy(out[0][0],in[0][0],ARRAYSIZE*REALSIZE);
    /*for(sb=0;sb<SBLIMIT;sb++)
        for(ss=0;ss<SSLIMIT;ss++)out[0][sb][ss]=in[0][sb][ss]; */
    return;
  } 

  {
    /* initialization */
#ifdef NATIVE_ASSEMBLY
    long_memset(is_pos,7,576*sizeof(int));
#else
    for(i=0;i<576;i+=8)
      is_pos[i]  =is_pos[i+1]=is_pos[i+2]=is_pos[i+3]=
      is_pos[i+4]=is_pos[i+5]=is_pos[i+6]=is_pos[i+7]=7;
#endif
    
    if(i_stereo)
    {
      if(gi->generalflag)
      {
	if(gi->mixed_block_flag)
	{
	  int max_sfb=0;
	  for(j=0;j<3;j++)
	  {
	    int sfbcnt=2;
	    for(sfb=12;sfb>=3;sfb--)
	    {
	      i=sfBandIndex->s[sfb];
	      lines=sfBandIndex->s[sfb+1]-i;
	      i=(i<<2)-i+(j+1)*lines-1;
	      while(lines>0)
	      {
		if(in[1][0][i]!=0.0f)
		{
		  sfbcnt=sfb;
		  sfb=-10;
		  lines=-10;
		}
		lines--;
		i--;
	      }
	    }
	    sfb=sfbcnt+1;
	    
	    if(sfb>max_sfb)max_sfb=sfb;
	    
	    for(;sfb<12;sfb++)
	    {
	      temp=sfBandIndex->s[sfb];
	      sb  =sfBandIndex->s[sfb+1]-temp;
	      i   =(temp<<2)-temp+j*sb;
	      
	      for(;sb>0;sb--)
	      {
		is_pos[i]=scalefactors[1].s[j][sfb];
		if(is_pos[i]!=7)is_ratio[i]=TAN12[is_pos[i]];
		i++;
	      }
	    }
	    sfb=sfBandIndex->s[10];
	    sb =sfBandIndex->s[11]-sfb;
	    sfb=(sfb<<2)-sfb+j*sb;
	    temp =sfBandIndex->s[11];
	    sb=sfBandIndex->s[12]-temp;
	    i=(temp<<2)-temp+j*sb;
	    
	    for(;sb>0;sb--)
	    {
	      is_pos[i]=is_pos[sfb];
	      is_ratio[i]=is_ratio[sfb];
	      i++;
	    }
	  }
	  if(max_sfb<=3)
	  {
	    ss=17;sb=-1;
	    for(i=2;i>=0;)
	      if(in[1][i][ss]!=0.0f)
	      {
		sb=(i<<4)+(i<<1)+ss;
		i=-1;
	      }
	      else
	      {
		ss--;
		if(ss<0)
		{
		  i--;
		  ss=17;
		}
	      }

	    for(i=0;sfBandIndex->l[i]<=sb;i++);
	    sfb=i;i=sfBandIndex->l[i];
	    for(;sfb<8;sfb++)
	      for(sb=sfBandIndex->l[sfb+1]-sfBandIndex->l[sfb];sb>0;sb--)
	      {
		is_pos[i]=scalefactors[1].l[sfb];
		if(is_pos[i]!=7)is_ratio[i]=TAN12[is_pos[i]];
		i++;
	      }
	  }
	}
	else
	{
	  for(j=0;j<3;j++)
	  {
	    int sfbcnt=-1;
	    for(sfb=12;sfb>=0;sfb--)
	    {
	      temp=sfBandIndex->s[sfb];
	      lines=sfBandIndex->s[sfb+1]-temp;
	      i=(temp<<2)-temp+(j+1)*lines-1;
		  
	      for(;lines>0;lines--)
	      {
		if(in[1][0][i]!=0.0f)
		{
		  sfbcnt=sfb;
		  sfb=lines=-10;
		}
		i--;
	      }
	    }

	    for(sfb=sfbcnt+1;sfb<12;sfb++)
	    {
	      temp=sfBandIndex->s[sfb];
	      sb  =sfBandIndex->s[sfb+1]-temp;
	      i   =(temp<<2)-temp+j*sb;
	      for(;sb>0;sb--)
	      {
		is_pos[i]=scalefactors[1].s[j][sfb];
		if(is_pos[i]!=7)is_ratio[i]=TAN12[is_pos[i]];
		    
		i++;
	      }
	    }
	      
	    temp =sfBandIndex->s[10];
	    temp2=sfBandIndex->s[11];

	    sb  =temp2-temp;
	    sfb =(temp<<2)-temp+j*sb;
	    sb  =sfBandIndex->s[12]-temp2;
	    i   =(temp2<<2)-temp2+j*sb;
	      
	    for(;sb>0;sb--)
	    {
	      is_pos[i]=is_pos[sfb];
	      is_ratio[i]=is_ratio[sfb];
	      i++;
	    }
	  }
	}
      }
      else // ms-stereo
      {
	ss=17;sb=0;
	for(i=31;i>=0;)
	  if(in[1][i][ss]!=0.0f)
	  {
	    sb=(i<<4)+(i<<1)+ss;
	    i=-1;
	  }
	  else
	  {
	    ss--;
	    if(ss<0)
	    {
	      i--;
	      ss=17;
	    }
	  }
	for(i=0;sfBandIndex->l[i]<=sb;i++);

	sfb=i;
	i=sfBandIndex->l[i];
	for(;sfb<21;sfb++)
	{
	  sb=sfBandIndex->l[sfb+1]-sfBandIndex->l[sfb];
	  for(;sb>0;sb--)
	  {
	    is_pos[i]=scalefactors[1].l[sfb];
	    if(is_pos[i]!=7)is_ratio[i]=TAN12[is_pos[i]];
	      
	    i++;
	  }
	}
	sfb=sfBandIndex->l[20];
	for(sb=576-sfBandIndex->l[21];sb>0;sb--)
	{
	  is_pos[i]=is_pos[sfb];
	  is_ratio[i]=is_ratio[sfb];
	  i++;
	}
      }
    }
    
    for(i=0;i<ARRAYSIZE;i++)
      if(is_pos[i]==7)
      {
	if(ms_stereo)
	{
	  out[LS][0][i]=(in[LS][0][i]+in[RS][0][i])*0.7071068f;
	  out[RS][0][i]=(in[LS][0][i]-in[RS][0][i])*0.7071068f;
	}
	else
	{
	  out[LS][0][i]=in[LS][0][i];
	  out[RS][0][i]=in[RS][0][i];
	}
      }
      else if(i_stereo)
      {
	out[RS][0][i]= in[LS][0][i]/REAL(1+is_ratio[i]);
	out[LS][0][i]=out[RS][0][i]*is_ratio[i];
      }
  } // channels==2
}

void Mpegtoraw::layer3reorder(int ch,int gr,REAL  in[SBLIMIT][SSLIMIT],
			                    REAL out[SBLIMIT][SSLIMIT])
{
  layer3grinfo *gi=&(sideinfo.ch[ch].gr[gr]);
  SFBANDINDEX *sfBandIndex=&(sfBandIndextable[frequency]);
  int sfb,sfb_start,sfb_lines;
  
  //  if(gi->generalflag)            // Since layer3reorderandantialias
  {
    if(gi->mixed_block_flag)
    {
#ifdef NATIVE_ASSEMBLY
      long_memset(out[2],REAL0,(SBLIMIT-2)*SSLIMIT*REALSIZE);
#else
      for(int sb=0;sb<SBLIMIT;sb++)
	out[sb][0] =out[sb][1] =out[sb][2] =out[sb][3] =out[sb][4] =
	out[sb][5] =out[sb][6] =out[sb][7] =out[sb][8] =out[sb][9] =
	out[sb][10]=out[sb][11]=out[sb][12]=out[sb][13]=out[sb][14]=
	out[sb][15]=out[sb][16]=out[sb][17]=0.0f;
#endif
      /* NO REORDER FOR LOW 2 SUBBANDS */
      memcpy(out[0],in[0],2*SSLIMIT*REALSIZE);

      /* REORDERING FOR REST SWITCHED SHORT */
      for(sfb=3,sfb_start=sfBandIndex->s[3],
	    sfb_lines=sfBandIndex->s[4]-sfb_start;
	  sfb<13;
	  sfb++,sfb_start=sfBandIndex->s[sfb],
	    (sfb_lines=sfBandIndex->s[sfb+1]-sfb_start))
      {
	for(int freq=0;freq<sfb_lines;freq++)
	{
	  int src_line=sfb_start+(sfb_start<<1)+freq;
	  int des_line=src_line+(freq<<1);
	  out[0][des_line]=in[0][src_line];src_line+=sfb_lines;des_line++;
	  out[0][des_line]=in[0][src_line];src_line+=sfb_lines;des_line++;
	  out[0][des_line]=in[0][src_line];
	}
      }
    }
    else
    {  /* pure short */
#ifdef NATIVE_ASSEMBLY
      long_memset(out[0],REAL0,ARRAYSIZE*REALSIZE);
#else
      for(int sb=0;sb<SBLIMIT;sb++)
        out[sb][0] =out[sb][1] =out[sb][2] =out[sb][3] =out[sb][4] =
	out[sb][5] =out[sb][6] =out[sb][7] =out[sb][8] =out[sb][9] =
        out[sb][10]=out[sb][11]=out[sb][12]=out[sb][13]=out[sb][14]=
        out[sb][15]=out[sb][16]=out[sb][17]=0.0f;
#endif
      for(sfb=0,sfb_start=0,sfb_lines=sfBandIndex->s[1];
	  sfb<13;sfb++,sfb_start=sfBandIndex->s[sfb],
	    (sfb_lines=sfBandIndex->s[sfb+1]-sfb_start))
      {
	for(int freq=0;freq<sfb_lines;freq++)
	{
	  int src_line=sfb_start+(sfb_start<<1)+freq;
	  int des_line=src_line+(freq<<1);

	  out[0][des_line]=in[0][src_line];src_line+=sfb_lines;des_line++;
	  out[0][des_line]=in[0][src_line];src_line+=sfb_lines;des_line++;
	  out[0][des_line]=in[0][src_line];
	}
      }
    }
  }
  //  else    /*long blocks */ 
  //    memcpy(out[0],in[0],ARRAYSIZE*REALSIZE);
}

void Mpegtoraw::layer3antialias(int ch,int gr,REAL  in[SBLIMIT][SSLIMIT],
				              REAL out[SBLIMIT][SSLIMIT])
{
  layer3grinfo *gi=&(sideinfo.ch[ch].gr[gr]);
  
  /* 31 alias-reduction operations between each pair of sub-bands */
  /* with 8 butterflies between each pair                         */
  if(gi->generalflag)
  {
    //    if(!gi->mixed_block_flag)      // Since layer3reorderandantialias
    //      memcpy(out[0],in[0],ARRAYSIZE*REALSIZE);
    //    else
    {
      memcpy(out[0],in[0],10*REALSIZE);

      for(int ss=0;ss<8;ss++)
      {
	REAL bu,bd; /* upper and lower butterfly inputs */

	bu=in[0][17-ss];bd=in[1][ss];
	out[0][17-ss]=(bu*cs[ss])-(bd*ca[ss]);
	out[1][ss]   =(bd*cs[ss])+(bu*ca[ss]);
      }

      memcpy(&out[1][8],&in[1][8],(10+(SBLIMIT-2)*SSLIMIT)*REALSIZE);
    }
  }
  else
  {
    memcpy(out[0],in[0],8*REALSIZE);
    for(int sb=0;sb<31;sb++)
    {
      for(int ss=0;ss<8;ss++)
      {
	REAL bu,bd; /* upper and lower butterfly inputs */

	bu=in[sb][17-ss];bd=in[sb+1][ss];
	out[sb][17-ss]=(bu*cs[ss])-(bd*ca[ss]);
	out[sb+1][ss] =(bd*cs[ss])+(bu*ca[ss]);
      }
      out[sb][8]=in[sb][8];out[sb][9]=in[sb][9];
    }
    memcpy(&out[31][8],&in[31][8],10*REALSIZE);
  }
}

void Mpegtoraw::layer3reorderandantialias(int ch,int gr,
					  REAL  in[SBLIMIT][SSLIMIT],
					  REAL out[SBLIMIT][SSLIMIT])
{
  REAL tmp[SBLIMIT][SSLIMIT];
  register layer3grinfo *gi=&(sideinfo.ch[ch].gr[gr]);

  if(gi->generalflag)
  {
    if(!gi->mixed_block_flag)
    {
      layer3reorder(ch,gr,in,out);
    }
    else
    {
      layer3reorder  (ch,gr, in,tmp);
      layer3antialias(ch,gr,tmp,out);
    }
  }
  else
    layer3antialias(ch,gr, in,out);
}




inline void Mpegtoraw::layer3inv_mdct(REAL *in,REAL *out,int block_type)
{
  REAL tmp[18];
  
  if(block_type==2)
  {
#ifdef NATIVE_ASSEMBLY
    long_memset(&out[0],REAL0,36*REALSIZE);
#else    
    out[ 0]=out[ 1]=out[ 2]=out[ 3]=out[ 4]=out[ 5]=out[ 6]=out[ 7]=out[ 8]=
    out[ 9]=out[10]=out[11]=out[12]=out[13]=out[14]=out[15]=out[16]=out[17]=
    out[18]=out[19]=out[20]=out[21]=out[22]=out[23]=out[24]=out[25]=out[26]=
    out[27]=out[28]=out[29]=out[30]=out[31]=out[32]=out[33]=out[34]=out[35]=
      0.0f;
#endif

    for(register int i=0,six_i=6;i<3;i++)
    {
      // 12 point IMDCT
      // Begin 12 point IDCT
	
      // Input aliasing for 12 pt IDCT
      in[15+i]+=in[12+i];in[12+i]+=in[9+i];in[9+i] += in[6+i];
      in[6+i] +=in[3+i]; in[3+i] +=in[0+i];
	
      // Input aliasing on odd indices (for 6 point IDCT)
      in[15+i]+=in[9+i]; in[9+i] +=in[3+i];
	
      {
	REAL pp1,sum;

	// 3 point IDCT on even indices
	pp1=in[ 6+i]         *0.866025403f;
	sum=in[ 0+i]+in[12+i]*0.500000000f;
	tmp[1]=in[0+i]-in[12+i];
	tmp[0]=sum+pp1         ;
	tmp[2]=sum-pp1         ;
	// End 3 point IDCT on even indices
	
	// 3 point IDCT on odd indices (for 6 point IDCT)
	pp1=in[ 9+i]         *0.866025403f;
	sum=in[ 3+i]+in[15+i]*0.500000000f;
	tmp[4]=in[3+i]-in[15+i];
	tmp[5]=sum+pp1         ;
	tmp[3]=sum-pp1         ;
	// End 3 point IDCT on odd indices
      }

      // Twiddle factors on odd indices (for 6 point IDCT)
      tmp[3]*=1.931851653f;
      tmp[4]*=0.707106781f;
      tmp[5]*=0.517638090f;
	
      {
	REAL save;

	// Output butterflies on 2 3 point IDCT's (for 6 point IDCT)
	save=tmp[0];tmp[0]=tmp[0]+tmp[5];tmp[5]=save-tmp[5];
	save=tmp[1];tmp[1]=tmp[1]+tmp[4];tmp[4]=save-tmp[4];
	save=tmp[2];tmp[2]=tmp[2]+tmp[3];tmp[3]=save-tmp[3];
	// End 6 point IDCT
      }
	
      // Twiddle factors on indices (for 12 point IDCT)
      tmp[0]*=0.504314480f;
      tmp[1]*=0.541196100f;
      tmp[2]*=0.630236207f;
      tmp[3]*=0.821339815f;
      tmp[4]*=1.306562965f;
      tmp[5]*=3.830648788f;
      // End 12 point IDCT
	
      // Shift to 12 point modified IDCT, multiply by window type 2
      tmp[ 8]=-tmp[0]*0.793353340f;
      tmp[ 9]=-tmp[0]*0.608761429f;
      tmp[ 7]=-tmp[1]*0.923879532f;
      tmp[10]=-tmp[1]*0.382683432f;
      tmp[ 6]=-tmp[2]*0.991444861f;
      tmp[11]=-tmp[2]*0.130526192f;
      
      tmp[0]= tmp[3];
      tmp[1]= tmp[4]*0.382683432f;
      tmp[2]= tmp[5]*0.608761429f;
      tmp[3]=-tmp[5]*0.793353340f;
      tmp[4]=-tmp[4]*0.923879532f;
      tmp[5]=-tmp[0]*0.991444861f;
      tmp[0]*=0.130526192f;
	
      out[six_i]+=tmp[ 0];six_i++;out[six_i]+=tmp[ 1];six_i++;
      out[six_i]+=tmp[ 2];six_i++;out[six_i]+=tmp[ 3];six_i++;
      out[six_i]+=tmp[ 4];six_i++;out[six_i]+=tmp[ 5];six_i++;

      out[six_i  ]+=tmp[ 6];out[six_i+1]+=tmp[ 7];
      out[six_i+2]+=tmp[ 8];out[six_i+3]+=tmp[ 9];
      out[six_i+4]+=tmp[10];out[six_i+5]+=tmp[11];
    }
  }
  else
  {
    // 36 point IDCT
    
    // input aliasing for 36 point IDCT
    in[17]+=in[16];in[16]+=in[15];in[15]+=in[14];in[14]+=in[13];
    in[13]+=in[12];in[12]+=in[11];in[11]+=in[10];in[10]+=in[ 9];
    in[ 9]+=in[ 8];in[ 8]+=in[7]; in[ 7]+=in[ 6];in[ 6]+=in[ 5];
    in[ 5]+=in[ 4];in[ 4]+=in[3]; in[ 3]+=in[ 2];in[ 2]+=in[ 1];
    in[ 1]+=in[ 0];

    // 18 point IDCT for odd indices

    // input aliasing for 18 point IDCT
    in[17]+=in[15];in[15]+=in[13];in[13]+=in[11];in[11]+=in[9];
    in[ 9]+=in[ 7];in[ 7]+=in[ 5];in[ 5]+=in[ 3];in[ 3]+=in[1];
    
    {
      register REAL *nc=NATIVECOS;

      for(int i=0;i<9;i++)
      {
        register REAL sum0=in[0],sum1=in[ 1];

        sum0+=in[ 2]*nc[0];       sum1+=in[ 3]*nc[0];         nc++;
        sum0+=in[ 4]*nc[0];       sum1+=in[ 5]*nc[0];         nc++;
        sum0+=in[ 6]*nc[0];       sum1+=in[ 7]*nc[0];         nc++;
        sum0+=in[ 8]*nc[0];       sum1+=in[ 9]*nc[0];         nc++;
        sum0+=in[10]*nc[0];       sum1+=in[11]*nc[0];         nc++;
        sum0+=in[12]*nc[0];       sum1+=in[13]*nc[0];         nc++;
        sum0+=in[14]*nc[0];       sum1+=in[15]*nc[0];         nc++;
        tmp[i]=sum0+in[16]*nc[0]; tmp[17-i]=sum1+in[17]*nc[0];nc++;
      }
    }

    // End 9 point IDCT on odd indices

    // Twiddle factors on odd indices

    tmp[9] *=5.736856623f;
    tmp[10]*=1.931851653f;
    tmp[11]*=1.183100792f;
    tmp[12]*=0.871723397f;
    tmp[13]*=0.707106781f;
    tmp[14]*=0.610387294f;
    tmp[15]*=0.551688959f;
    tmp[16]*=0.517638090f;
    tmp[17]*=0.501909918f;
    
    // Butterflies on 9 point IDCT's
    for(int i=0;i<9;i++)
    {
      REAL save;

      save=tmp[i];
      tmp[i]+=tmp[17-i];
      tmp[17-i]=save-tmp[17-i];
    }
    // end 18 point IDCT
    
    // shift to modified IDCT
    out[ 0]=tmp[9] *win[block_type][ 0];out[ 1]=tmp[10]*win[block_type][ 1];
    out[ 2]=tmp[11]*win[block_type][ 2];out[ 3]=tmp[12]*win[block_type][ 3];
    out[ 4]=tmp[13]*win[block_type][ 4];out[ 5]=tmp[14]*win[block_type][ 5];
    out[ 6]=tmp[15]*win[block_type][ 6];out[ 7]=tmp[16]*win[block_type][ 7];
    out[ 8]=tmp[17]*win[block_type][ 8];
                                        out[ 9]=tmp[17]*win[block_type][ 9];
    out[10]=tmp[16]*win[block_type][10];out[11]=tmp[15]*win[block_type][11];
    out[12]=tmp[14]*win[block_type][12];out[13]=tmp[13]*win[block_type][13];
    out[14]=tmp[12]*win[block_type][14];out[15]=tmp[11]*win[block_type][15];
    out[16]=tmp[10]*win[block_type][16];out[17]=tmp[ 9]*win[block_type][17];
    out[18]=tmp[ 8]*win[block_type][18];out[19]=tmp[ 7]*win[block_type][19];
    out[20]=tmp[ 6]*win[block_type][20];out[21]=tmp[ 5]*win[block_type][21];
    out[22]=tmp[ 4]*win[block_type][22];out[23]=tmp[ 3]*win[block_type][23];
    out[24]=tmp[ 2]*win[block_type][24];out[25]=tmp[ 1]*win[block_type][25];
    out[26]=tmp[ 0]*win[block_type][26];out[27]=tmp[ 0]*win[block_type][27];
    out[28]=tmp[ 1]*win[block_type][28];out[29]=tmp[ 2]*win[block_type][29];
    out[30]=tmp[ 3]*win[block_type][30];out[31]=tmp[ 4]*win[block_type][31];
    out[32]=tmp[ 5]*win[block_type][32];out[33]=tmp[ 6]*win[block_type][33];
    out[34]=tmp[ 7]*win[block_type][34];out[35]=tmp[ 8]*win[block_type][35];
  }
}

void Mpegtoraw::layer3hybrid(int ch,int gr,int sb,REAL in[SSLIMIT],
			                          REAL out[SSLIMIT][SBLIMIT])
{
  REAL raw[36];
  {
    register layer3grinfo *gi=&(sideinfo.ch[ch].gr[gr]);
    int bt=(gi->window_switching_flag && gi->mixed_block_flag && (sb<2))
           ?0 :gi->block_type;
    layer3inv_mdct(in,raw,bt);
  }
  
  /* overlap addition */
  {
    register REAL *pb=prevblck[ch][sb];
    out[ 0][sb]=raw[ 0]+pb[ 0];out[ 1][sb]=raw[ 1]+pb[ 1];
    out[ 2][sb]=raw[ 2]+pb[ 2];out[ 3][sb]=raw[ 3]+pb[ 3];
    out[ 4][sb]=raw[ 4]+pb[ 4];out[ 5][sb]=raw[ 5]+pb[ 5];
    out[ 6][sb]=raw[ 6]+pb[ 6];out[ 7][sb]=raw[ 7]+pb[ 7];
    out[ 8][sb]=raw[ 8]+pb[ 8];out[ 9][sb]=raw[ 9]+pb[ 9];
    out[10][sb]=raw[10]+pb[10];out[11][sb]=raw[11]+pb[11];
    out[12][sb]=raw[12]+pb[12];out[13][sb]=raw[13]+pb[13];
    out[14][sb]=raw[14]+pb[14];out[15][sb]=raw[15]+pb[15];
    out[16][sb]=raw[16]+pb[16];out[17][sb]=raw[17]+pb[17];

    memcpy(pb,&raw[18],SSLIMIT*REALSIZE);
  }
}

#define NEG(a) (a)=-(a)

void Mpegtoraw::extractlayer3(void)
{
  {
    int main_data_end,flush_main;
    int bytes_to_discard;

    layer3getsideinfo();
	 
    for(register int i=layer3slots;i>0;i--)  // read main data.
      bitwindow.putbyte(getbits(8));

    main_data_end=bitwindow.gettotalbit()>>3;// of previous frame

    if((flush_main=(bitwindow.gettotalbit() & 0x7)))
    {
      bitwindow.forward(8-flush_main);
      main_data_end++;
    }

    bytes_to_discard=layer3framestart-main_data_end-sideinfo.main_data_begin;
    if(main_data_end>4096)
    {
      layer3framestart-=4096;
      bitwindow.rewind(4096*8);
    }
  
    layer3framestart+=layer3slots;
  
    if(bytes_to_discard<0)return;
    bitwindow.forward(bytes_to_discard<<3);
  }

  for(int gr=0;gr<2;gr++)
  {
    union
    {
      int  is      [SBLIMIT][SSLIMIT];
      REAL lr   [2][SBLIMIT][SSLIMIT];
      REAL hout [2][SSLIMIT][SBLIMIT];
    }b1;
    union
    {
      REAL ro   [2][SBLIMIT][SSLIMIT];
      REAL hin  [2][SBLIMIT][SSLIMIT];
    }b2;


      layer3part2start=bitwindow.gettotalbit();
      layer3getscalefactors (LS,gr);
      layer3huffmandecode   (LS,gr      ,b1.is);
      layer3dequantizesample(LS,gr,b1.is,b2.ro[LS]);
    if(inputstereo)
    {
      layer3part2start=bitwindow.gettotalbit();
      layer3getscalefactors (RS,gr);
      layer3huffmandecode   (RS,gr      ,b1.is);
      layer3dequantizesample(RS,gr,b1.is,b2.ro[RS]);
    }

    layer3fixtostereo(gr,b2.ro,b1.lr);
    
      layer3reorderandantialias(LS,gr,b1.lr[LS],b2.hin[LS]);
      for(int sb=0;sb<SBLIMIT;sb++)
	layer3hybrid (LS,gr,sb,b2.hin[LS][sb],b1.hout[LS]);
    if(outputstereo)
    {
      layer3reorderandantialias(RS,gr,b1.lr[RS],b2.hin[RS]);
      for(int sb=0;sb<SBLIMIT;sb++)
	layer3hybrid (RS,gr,sb,b2.hin[RS][sb],b1.hout[RS]);
      {
	register int i=2*SSLIMIT*SBLIMIT-1;
	do{
	  NEG(b1.hout[0][0][i   ]);NEG(b1.hout[0][0][i- 2]);
	  NEG(b1.hout[0][0][i- 4]);NEG(b1.hout[0][0][i- 6]);
	  NEG(b1.hout[0][0][i- 8]);NEG(b1.hout[0][0][i-10]);
	  NEG(b1.hout[0][0][i-12]);NEG(b1.hout[0][0][i-14]);
	  NEG(b1.hout[0][0][i-16]);NEG(b1.hout[0][0][i-18]);
	  NEG(b1.hout[0][0][i-20]);NEG(b1.hout[0][0][i-22]);
	  NEG(b1.hout[0][0][i-24]);NEG(b1.hout[0][0][i-26]);
	  NEG(b1.hout[0][0][i-28]);NEG(b1.hout[0][0][i-30]);
	}while((i-=2*SBLIMIT)>0);
      }
    }
    else
    {
      register int i=SSLIMIT*SBLIMIT-1;
      do{
	NEG(b1.hout[0][0][i   ]);NEG(b1.hout[0][0][i- 2]);
	NEG(b1.hout[0][0][i- 4]);NEG(b1.hout[0][0][i- 6]);
	NEG(b1.hout[0][0][i- 8]);NEG(b1.hout[0][0][i-10]);
	NEG(b1.hout[0][0][i-12]);NEG(b1.hout[0][0][i-14]);
	NEG(b1.hout[0][0][i-16]);NEG(b1.hout[0][0][i-18]);
	NEG(b1.hout[0][0][i-20]);NEG(b1.hout[0][0][i-22]);
	NEG(b1.hout[0][0][i-24]);NEG(b1.hout[0][0][i-26]);
	NEG(b1.hout[0][0][i-28]);NEG(b1.hout[0][0][i-30]);
      }while((i-=2*SBLIMIT)>0);
    }

    for(int ss=0;ss<SSLIMIT;ss++)
      subbandsynthesis(b1.hout[LS][ss],b1.hout[RS][ss]);
  }
}
