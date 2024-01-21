
/*
    xskat - a card game for 1 to 3 players.
    Copyright (C) 1996  Gunter Gerhardt

    This program is free software; you can redistribute it freely.
    Use it at your own risk; there is NO WARRANTY.
*/

#undef EXTERN
#ifdef SKAT_C
#define EXTERN
#else
#define EXTERN extern
#endif

#define AS 0
#define ZEHN 1
#define KOENIG 2
#define BUBE 4
#define NEUN 5

enum {
 GEBEN,
 REIZEN,
 HANDSPIEL,
 DRUECKEN,
 ANSAGEN,
 SPIELEN,
 RESULT
};

EXTERN int nullw[]
#ifdef SKAT_C
=
{
  23,35,46,59
}
#endif
;

EXTERN int rwert[]
#ifdef SKAT_C
=
{
  9,10,11,12,24
}
#endif
;

EXTERN int reizw[]
#ifdef SKAT_C
=
{
  18,20,22,23,24,27,30,33,35,36,40,44,45,46,48,50,
  54,55,59,60,63,66,70,72,77,80,81,84,88,90,96,99,
  100,108,110,117,120,121,126,130,132,135,140,143,
  144,150,153,154,156,160,162,165,168,170,171,176,
  180,187,189,190,192,198,200,204,207,209,210,216
}
#endif
;

EXTERN int cardw[]
#ifdef SKAT_C
=
{
  11,10,4,3,2,0,0,0
}
#endif
;

EXTERN int sortw[]
#ifdef SKAT_C
=
{
  0,1,2,3
}
#endif
;

EXTERN int numsp;
EXTERN int maxrw[3],sort1[3],sort2[3],alternate[3],splfirst[3];
EXTERN int lastmsaho[3],protsort[3],hatnfb[3][5];
EXTERN int splsum[3],sum[3],asplsum[3],asum[3],alist[3];
EXTERN int cards[32],gespcd[32];
EXTERN int quit,phase,geber,hoerer,sager,spieler;
EXTERN int saho,reizp,gedr,vmh,stich,ausspl;
EXTERN int possi[10],possc,stcd[3];
EXTERN int trumpf,handsp,stsum,spcards[12];
EXTERN int spgew,spwert,schwz,nullv;
EXTERN int schnang,schwang,ouveang;
EXTERN int splist[360],splstp,splres;
EXTERN int gespfb[4],high[5];
EXTERN int inhand[4][8];
EXTERN int playcd,drkcd;
EXTERN int mes1,mes2,mes3;
EXTERN int predef,logging;
EXTERN long seed[2],savseed,gamenr;
EXTERN char *list_file,*game_file,*prot_file;
EXTERN struct
{
  int stichgem,spieler,trumpf,gereizt,gewonn,augen,ehsso,predef;
  int stiche[10][3],anspiel[10],gemacht[10],skat[2][2];
  long savseed,gamenr;
} prot1,prot2;
