#include <stdio.h>
#include <ctype.h>
#include <strings.h>

#define BAKVEC 10000
#define BALANC 10001
#define BALBAK 10002
#define BANDR 10003
#define BANDV 10004
#define BISECT 10800
#define BQR 11600
#define CBABK2 20100
#define CBAL 20101
#define CG 20600
#define CH 20700
#define CINVIT 20800
#define COMBAK 21400
#define COMHES 21401
#define COMLR2 21402
#define COMLR 21403
#define COMQR2 21404
#define COMQR 21405
#define CORTB 21406
#define CORTH 21407
#define DCHDC 30200
#define DCHDD 30201
#define DCHEX 30202
#define DCHUD 30203
#define DGBCO 30600
#define DGBDI 30601
#define DGBFA 30602
#define DGBSL 30603
#define DGECO 30604
#define DGEDI 30605
#define DGEFA 30606
#define DGESL 30607
#define DGTSL 30608
#define DPBCO 31500
#define DPBDI 31501
#define DPBFA 31502
#define DPBSL 31503
#define DPOCO 31504
#define DPODI 31505
#define DPOFA 31506
#define DPOSL 31507
#define DPPCO 31508
#define DPPDI 31509
#define DPPFA 31510
#define DPPSL 31511
#define DPTSL 31512
#define DSICO 31800
#define DSIDI 31801
#define DSIFA 31802
#define DSISL 31803
#define DSPCO 31804
#define DSPDI 31805
#define DSPFA 31806
#define DSPSL 31807
#define DSVDC 31808
#define DTRCO 31900
#define DTRDI 31901
#define DTRSL 31902
#define DVD 32100
#define ELMBAK 41100
#define ELMHES 41101
#define ELTRAN 41102
#define FIGI2 50800
#define FIGI 50801
#define HQR2 71600
#define HQR 71601
#define HTRIB3 71900
#define HTRIBK 71901
#define HTRID3 71902
#define HTRIDI 71903
#define IMTQL1 81200
#define IMTQL2 81201
#define IMTQLV 81202
#define INVIT 81300
#define MINFIT 120800
#define ORTBAK 141700
#define ORTHES 141701
#define ORTRAN 141702
#define QZHES 162500
#define QZIT 162501
#define QZVAL 162502
#define QZVEC 162503
#define RATQR 170000
#define REDUC2 170400
#define REDUC 170401
#define RGG 170600
#define RG 170601
#define RSB 171800
#define RSGAB 171801
#define RSGBA 171802
#define RSG 171803
#define RSM 171804
#define RSP 171805
#define RST 171806
#define RS 171807
#define RT 171900
#define SCHDC 180200
#define SCHDD 180201
#define SCHEX 180202
#define SCHUD 180203
#define SGBCO 180600
#define SGBDI 180601
#define SGBFA 180602
#define SGBSL 180603
#define SGECO 180604
#define SGEDI 180605
#define SGEFA 180606
#define SGESL 180607
#define SGTSL 180608
#define SPBCO 181500
#define SPBDI 181501
#define SPBFA 181502
#define SPBSL 181503
#define SPOCO 181504
#define SPODI 181505
#define SPOFA 181506
#define SPOSL 181507
#define SPPCO 181508
#define SPPDI 181509
#define SPPFA 181510
#define SPPSL 181511
#define SPTSL 181512
#define SSICO 181800
#define SSIDI 181801
#define SSIFA 181802
#define SSISL 181803
#define SSPCO 181804
#define SSPDI 181805
#define SSPFA 181806
#define SSPSL 181807
#define SSVDC 181808
#define STRCO 181900
#define STRDI 181901
#define STRSL 181902
#define SVD 182100
#define TINVIT 190800
#define TQL1 191600
#define TQL2 191601
#define TQLRAT 191602
#define TRBAK1 191700
#define TRBAK3 191701
#define TRED1 191702
#define TRED2 191703
#define TRED3 191704
#define TRIDIB 191705
#define TSTURM 191800
#define ZBABK2 250100
#define ZBAL 250101
#define ZG 250600
#define ZH 250700
#define ZINVIT 250800
#define ZOMBAK 251400
#define ZOMHES 251401
#define ZOMLR2 251402
#define ZOMLR 251403
#define ZOMQR2 251404
#define ZOMQR 251405
#define ZORTB 251406
#define ZORTH 251407


static char BAarray[][80] = {
    "BAKVEC",
    "BALANC",
    "BALBAK",
    "BANDR",
    "BANDV"
  };

int BAsize = 5;

static char BIarray[][80] = {
    "BISECT"
  };

int BIsize = 1;

static char BQarray[][80] = {
    "BQR"
  };

int BQsize = 1;

static char CBarray[][80] = {
    "CBABK2",
    "CBAL"
  };

int CBsize = 2;

static char CGarray[][80] = {
    "CG"
  };

int CGsize = 1;

static char CHarray[][80] = {
    "CH"
  };

int CHsize = 1;

static char CIarray[][80] = {
    "CINVIT"
  };

int CIsize = 1;

static char COarray[][80] = {
    "COMBAK",
    "COMHES",
    "COMLR2",
    "COMLR",
    "COMQR2",
    "COMQR",
    "CORTB",
    "CORTH"
  };

int COsize = 8;

static char DCarray[][80] = {
    "DCHDC",
    "DCHDD",
    "DCHEX",
    "DCHUD"
  };

int DCsize = 4;

static char DGarray[][80] = {
    "DGBCO",
    "DGBDI",
    "DGBFA",
    "DGBSL",
    "DGECO",
    "DGEDI",
    "DGEFA",
    "DGESL",
    "DGTSL"
  };

int DGsize = 9;

static char DParray[][80] = {
    "DPBCO",
    "DPBDI",
    "DPBFA",
    "DPBSL",
    "DPOCO",
    "DPODI",
    "DPOFA",
    "DPOSL",
    "DPPCO",
    "DPPDI",
    "DPPFA",
    "DPPSL",
    "DPTSL"
  };

int DPsize = 13;

static char DSarray[][80] = {
    "DSICO",
    "DSIDI",
    "DSIFA",
    "DSISL",
    "DSPCO",
    "DSPDI",
    "DSPFA",
    "DSPSL",
    "DSVDC"
  };

int DSsize = 9;

static char DTarray[][80] = {
    "DTRCO",
    "DTRDI",
    "DTRSL"
  };

int DTsize = 3;

static char DVarray[][80] = {
    "DVD"
  };

int DVsize = 1;

static char ELarray[][80] = {
    "ELMBAK",
    "ELMHES",
    "ELTRAN"
  };

int ELsize = 3;

static char FIarray[][80] = {
    "FIGI2",
    "FIGI"
  };

int FIsize = 2;

static char HQarray[][80] = {
    "HQR2",
    "HQR"
  };

int HQsize = 2;

static char HTarray[][80] = {
    "HTRIB3",
    "HTRIBK",
    "HTRID3",
    "HTRIDI"
  };

int HTsize = 4;

static char IMarray[][80] = {
    "IMTQL1",
    "IMTQL2",
    "IMTQLV"
  };

int IMsize = 3;

static char INarray[][80] = {
    "INVIT"
  };

int INsize = 1;

static char MIarray[][80] = {
    "MINFIT"
  };

int MIsize = 1;

static char ORarray[][80] = {
    "ORTBAK",
    "ORTHES",
    "ORTRAN"
  };

int ORsize = 3;

static char QZarray[][80] = {
    "QZHES",
    "QZIT",
    "QZVAL",
    "QZVEC"
  };

int QZsize = 4;

static char RAarray[][80] = {
    "RATQR"
  };

int RAsize = 1;

static char REarray[][80] = {
    "REDUC2",
    "REDUC"
  };

int REsize = 2;

static char RGarray[][80] = {
    "RGG",
    "RG"
  };

int RGsize = 2;

static char RSarray[][80] = {
    "RSB",
    "RSGAB",
    "RSGBA",
    "RSG",
    "RSM",
    "RSP",
    "RST",
    "RS"
  };

int RSsize = 8;

static char RTarray[][80] = {
    "RT"
  };

int RTsize = 1;

static char SCarray[][80] = {
    "SCHDC",
    "SCHDD",
    "SCHEX",
    "SCHUD"
  };

int SCsize = 4;

static char SGarray[][80] = {
    "SGBCO",
    "SGBDI",
    "SGBFA",
    "SGBSL",
    "SGECO",
    "SGEDI",
    "SGEFA",
    "SGESL",
    "SGTSL"
  };

int SGsize = 9;

static char SParray[][80] = {
    "SPBCO",
    "SPBDI",
    "SPBFA",
    "SPBSL",
    "SPOCO",
    "SPODI",
    "SPOFA",
    "SPOSL",
    "SPPCO",
    "SPPDI",
    "SPPFA",
    "SPPSL",
    "SPTSL"
  };

int SPsize = 13;

static char SSarray[][80] = {
    "SSICO",
    "SSIDI",
    "SSIFA",
    "SSISL",
    "SSPCO",
    "SSPDI",
    "SSPFA",
    "SSPSL",
    "SSVDC"
  };

int SSsize = 9;

static char STarray[][80] = {
    "STRCO",
    "STRDI",
    "STRSL"
  };

int STsize = 3;

static char SVarray[][80] = {
    "SVD"
  };

int SVsize = 1;

static char TIarray[][80] = {
    "TINVIT"
  };

int TIsize = 1;

static char TQarray[][80] = {
    "TQL1",
    "TQL2",
    "TQLRAT"
  };

int TQsize = 3;

static char TRarray[][80] = {
    "TRBAK1",
    "TRBAK3",
    "TRED1",
    "TRED2",
    "TRED3",
    "TRIDIB"
  };

int TRsize = 6;

static char TSarray[][80] = {
    "TSTURM"
  };

int TSsize = 1;

static char ZBarray[][80] = {
    "ZBABK2",
    "ZBAL"
  };

int ZBsize = 2;

static char ZGarray[][80] = {
    "ZG"
  };

int ZGsize = 1;

static char ZHarray[][80] = {
    "ZH"
  };

int ZHsize = 1;

static char ZIarray[][80] = {
    "ZINVIT"
  };

int ZIsize = 1;

static char ZOarray[][80] = {
    "ZOMBAK",
    "ZOMHES",
    "ZOMLR2",
    "ZOMLR",
    "ZOMQR2",
    "ZOMQR",
    "ZORTB",
    "ZORTH"
  };

int ZOsize = 8;



main(argc, argv)
char *argv[];
int argc;
{
  int i, j, len;

  if(argc <= 1)
    printf("Usage: equivalence routine_name [ routine_name ... ]\n");
  else
    for(i=1; i<argc; i++)
    {
      len = strlen(argv[i]);
      for(j=0; j<len; j++)
        argv[i][j] = toupper(argv[i][j]);
      convert(argv[i]);
    }
}


convert(input)
char input[];
{
  int value;

  switch(input[0]) {
    case 'B':
      switch(input[1]) {
        case 'A':
          value = compr(BAarray, input, BAsize);
          look_up(value+10000, input);
        break;
        case 'I':
          value = compr(BIarray, input, BIsize);
          look_up(value+10800, input);
        break;
        case 'Q':
          value = compr(BQarray, input, BQsize);
          look_up(value+11600, input);
        break;
        default:  /* end switch */
          look_up(0, input);  /* end switch */
        break;
      };  /* end switch */
    break;
    case 'C':
      switch(input[1]) {
        case 'B':
          value = compr(CBarray, input, CBsize);
          look_up(value+20100, input);
        break;
        case 'G':
          value = compr(CGarray, input, CGsize);
          look_up(value+20600, input);
        break;
        case 'H':
          value = compr(CHarray, input, CHsize);
          look_up(value+20700, input);
        break;
        case 'I':
          value = compr(CIarray, input, CIsize);
          look_up(value+20800, input);
        break;
        case 'O':
          value = compr(COarray, input, COsize);
          look_up(value+21400, input);
        break;
        default:  /* end switch */
          look_up(0, input);  /* end switch */
        break;
      };  /* end switch */
    break;
    case 'D':
      switch(input[1]) {
        case 'C':
          value = compr(DCarray, input, DCsize);
          look_up(value+30200, input);
        break;
        case 'G':
          value = compr(DGarray, input, DGsize);
          look_up(value+30600, input);
        break;
        case 'P':
          value = compr(DParray, input, DPsize);
          look_up(value+31500, input);
        break;
        case 'S':
          value = compr(DSarray, input, DSsize);
          look_up(value+31800, input);
        break;
        case 'T':
          value = compr(DTarray, input, DTsize);
          look_up(value+31900, input);
        break;
        case 'V':
          value = compr(DVarray, input, DVsize);
          look_up(value+32100, input);
        break;
        default:  /* end switch */
          look_up(0, input);  /* end switch */
        break;
      };  /* end switch */
    break;
    case 'E':
      switch(input[1]) {
        case 'L':
          value = compr(ELarray, input, ELsize);
          look_up(value+41100, input);
        break;
        default:  /* end switch */
          look_up(0, input);  /* end switch */
        break;
      };  /* end switch */
    break;
    case 'F':
      switch(input[1]) {
        case 'I':
          value = compr(FIarray, input, FIsize);
          look_up(value+50800, input);
        break;
        default:  /* end switch */
          look_up(0, input);  /* end switch */
        break;
      };  /* end switch */
    break;
    case 'H':
      switch(input[1]) {
        case 'Q':
          value = compr(HQarray, input, HQsize);
          look_up(value+71600, input);
        break;
        case 'T':
          value = compr(HTarray, input, HTsize);
          look_up(value+71900, input);
        break;
        default:  /* end switch */
          look_up(0, input);  /* end switch */
        break;
      };  /* end switch */
    break;
    case 'I':
      switch(input[1]) {
        case 'M':
          value = compr(IMarray, input, IMsize);
          look_up(value+81200, input);
        break;
        case 'N':
          value = compr(INarray, input, INsize);
          look_up(value+81300, input);
        break;
        default:  /* end switch */
          look_up(0, input);  /* end switch */
        break;
      };  /* end switch */
    break;
    case 'M':
      switch(input[1]) {
        case 'I':
          value = compr(MIarray, input, MIsize);
          look_up(value+120800, input);
        break;
        default:  /* end switch */
          look_up(0, input);  /* end switch */
        break;
      };  /* end switch */
    break;
    case 'O':
      switch(input[1]) {
        case 'R':
          value = compr(ORarray, input, ORsize);
          look_up(value+141700, input);
        break;
        default:  /* end switch */
          look_up(0, input);  /* end switch */
        break;
      };  /* end switch */
    break;
    case 'Q':
      switch(input[1]) {
        case 'Z':
          value = compr(QZarray, input, QZsize);
          look_up(value+162500, input);
        break;
        default:  /* end switch */
          look_up(0, input);  /* end switch */
        break;
      };  /* end switch */
    break;
    case 'R':
      switch(input[1]) {
        case 'A':
          value = compr(RAarray, input, RAsize);
          look_up(value+170000, input);
        break;
        case 'E':
          value = compr(REarray, input, REsize);
          look_up(value+170400, input);
        break;
        case 'G':
          value = compr(RGarray, input, RGsize);
          look_up(value+170600, input);
        break;
        case 'S':
          value = compr(RSarray, input, RSsize);
          look_up(value+171800, input);
        break;
        case 'T':
          value = compr(RTarray, input, RTsize);
          look_up(value+171900, input);
        break;
        default:  /* end switch */
          look_up(0, input);  /* end switch */
        break;
      };  /* end switch */
    break;
    case 'S':
      switch(input[1]) {
        case 'C':
          value = compr(SCarray, input, SCsize);
          look_up(value+180200, input);
        break;
        case 'G':
          value = compr(SGarray, input, SGsize);
          look_up(value+180600, input);
        break;
        case 'P':
          value = compr(SParray, input, SPsize);
          look_up(value+181500, input);
        break;
        case 'S':
          value = compr(SSarray, input, SSsize);
          look_up(value+181800, input);
        break;
        case 'T':
          value = compr(STarray, input, STsize);
          look_up(value+181900, input);
        break;
        case 'V':
          value = compr(SVarray, input, SVsize);
          look_up(value+182100, input);
        break;
        default:  /* end switch */
          look_up(0, input);  /* end switch */
        break;
      };  /* end switch */
    break;
    case 'T':
      switch(input[1]) {
        case 'I':
          value = compr(TIarray, input, TIsize);
          look_up(value+190800, input);
        break;
        case 'Q':
          value = compr(TQarray, input, TQsize);
          look_up(value+191600, input);
        break;
        case 'R':
          value = compr(TRarray, input, TRsize);
          look_up(value+191700, input);
        break;
        case 'S':
          value = compr(TSarray, input, TSsize);
          look_up(value+191800, input);
        break;
        default:  /* end switch */
          look_up(0, input);  /* end switch */
        break;
      };  /* end switch */
    break;
    case 'Z':
      switch(input[1]) {
        case 'B':
          value = compr(ZBarray, input, ZBsize);
          look_up(value+250100, input);
        break;
        case 'G':
          value = compr(ZGarray, input, ZGsize);
          look_up(value+250600, input);
        break;
        case 'H':
          value = compr(ZHarray, input, ZHsize);
          look_up(value+250700, input);
        break;
        case 'I':
          value = compr(ZIarray, input, ZIsize);
          look_up(value+250800, input);
        break;
        case 'O':
          value = compr(ZOarray, input, ZOsize);
          look_up(value+251400, input);
        break;
      };  /* end switch */
    break;
    default:
      look_up(0, input);
    break;
  }; /* end switch */
}


compr(array, value, size)
char array[][80];
char value[];
int size;
{
  int i;

  for(i=0; (i <= size) && (strcmp(array[i], value)); i++);
  return(i);
}


look_up(value, input)
int value;
char input[];
{
  switch(value) {
    case BAKVEC:
      printf("%s:\tNo LAPACK equivalent\n", input);
    break;
    case BALANC:
      printf("%s:\n\tSUBROUTINE SGEBAL( JOB, N, A, LDA, ILO, IHI, SCALE, INFO )\n\tSUBROUTINE DGEBAL( JOB, N, A, LDA, ILO, IHI, SCALE, INFO )\n", input);
    break;
    case BALBAK:
      printf("%s:\n\tSUBROUTINE SGEBAK( JOB, SIDE, N, ILO, IHI, SCALE, M, V, LDV,\n\tSUBROUTINE DGEBAK( JOB, SIDE, N, ILO, IHI, SCALE, M, V, LDV, INFO )\n", input);
    break;
    case BANDR:
      printf("%s:\n\tSUBROUTINE SSBTRD( VECT, UPLO, N, KD, AB, LDAB, D, E, Q, LDQ,\n\tSUBROUTINE DSBTRD( VECT, UPLO, N, KD, AB, LDAB, D, E, Q, LDQ, WORK, INFO )\n", input);
    break;
    case BANDV:
      printf("%s:\tNo LAPACK equivalent\n", input);
    break;
    case BISECT:
      printf("%s:\n\tSUBROUTINE SSTEBZ( RANGE, ORDER, N, VL, VU, IL, IU, ABSTOL, D, E,\n\tSUBROUTINE DSTEBZ( RANGE, ORDER, N, VL, VU, IL, IU, ABSTOL, D, E, M, NSPLIT, W, IBLOCK, ISPLIT, WORK, IWORK, INFO )\n", input);
    break;
    case BQR:
      printf("%s:\n\tSUBROUTINE SSBEVX( JOBZ, RANGE, UPLO, N, KD, AB, LDAB, Q, LDQ, VL,\n\tSUBROUTINE DSBEVX( JOBZ, RANGE, UPLO, N, KD, AB, LDAB, Q, LDQ, VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO )\n", input);
    break;
    case CBABK2:
      printf("%s:\n\tSUBROUTINE CGEBAK( JOB, SIDE, N, ILO, IHI, SCALE, M, V, LDV,\n\tSUBROUTINE ZGEBAK( JOB, SIDE, N, ILO, IHI, SCALE, M, V, LDV, INFO )\n", input);
    break;
    case CBAL:
      printf("%s:\n\tSUBROUTINE CGEBAL( JOB, N, A, LDA, ILO, IHI, SCALE, INFO )\n\tSUBROUTINE ZGEBAL( JOB, N, A, LDA, ILO, IHI, SCALE, INFO )\n", input);
    break;
    case CG:
      printf("%s:\n\tSUBROUTINE CGEEV( JOBVL, JOBVR, N, A, LDA, W, VL, LDVL, VR, LDVR,\n\tSUBROUTINE ZGEEV( JOBVL, JOBVR, N, A, LDA, W, VL, LDVL, VR, LDVR, WORK, LWORK, RWORK, INFO )\n", input);
    break;
    case CH:
      printf("%s:\n\tSUBROUTINE CHEEV( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, RWORK,\n\tSUBROUTINE ZHEEV( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, RWORK, INFO )\n", input);
    break;
    case CINVIT:
      printf("%s:\n\tSUBROUTINE CHSEIN( JOB, EIGSRC, INITV, SELECT, N, H, LDH, W, VL,\n\tSUBROUTINE ZHSEIN( JOB, EIGSRC, INITV, SELECT, N, H, LDH, W, VL, LDVL, VR, LDVR, MM, M, WORK, RWORK, IFAILL, IFAILR, INFO )\n", input);
    break;
    case COMBAK:
      printf("%s:\n\tSUBROUTINE CUNMHR( SIDE, TRANS, M, N, ILO, IHI, A, LDA, TAU, C,\n\tSUBROUTINE ZUNMHR( SIDE, TRANS, M, N, ILO, IHI, A, LDA, TAU, C, LDC, WORK, LWORK, INFO )\n", input);
    break;
    case COMHES:
      printf("%s:\n\tSUBROUTINE CGEHRD( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )\n\tSUBROUTINE ZGEHRD( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )\n", input);
    break;
    case COMLR2:
      printf("%s:\n\tSUBROUTINE CUNGHR( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )\n\tSUBROUTINE ZUNGHR( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )\n\n\tSUBROUTINE CHSEQR( JOB, COMPZ, N, ILO, IHI, H, LDH, W, Z, LDZ,\n\tSUBROUTINE ZHSEQR( JOB, COMPZ, N, ILO, IHI, H, LDH, W, Z, LDZ, WORK, LWORK, INFO )\n\n\tSUBROUTINE CTREVC( JOB, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR,\n\tSUBROUTINE ZTREVC( JOB, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR, LDVR, MM, M, WORK, RWORK, INFO )\n", input);
    break;
    case COMLR:
      printf("%s:\n\tSUBROUTINE CHSEQR( JOB, COMPZ, N, ILO, IHI, H, LDH, W, Z, LDZ,\n\tSUBROUTINE ZHSEQR( JOB, COMPZ, N, ILO, IHI, H, LDH, W, Z, LDZ, WORK, LWORK, INFO )\n", input);
    break;
    case COMQR2:
      printf("%s:\n\tSUBROUTINE CUNGHR( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )\n\tSUBROUTINE ZUNGHR( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )\n\n\tSUBROUTINE CHSEQR( JOB, COMPZ, N, ILO, IHI, H, LDH, W, Z, LDZ,\n\tSUBROUTINE ZHSEQR( JOB, COMPZ, N, ILO, IHI, H, LDH, W, Z, LDZ, WORK, LWORK, INFO )\n\n\tSUBROUTINE CTREVC( JOB, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR,\n\tSUBROUTINE ZTREVC( JOB, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR, LDVR, MM, M, WORK, RWORK, INFO )\n", input);
    break;
    case COMQR:
      printf("%s:\n\tSUBROUTINE CHSEQR( JOB, COMPZ, N, ILO, IHI, H, LDH, W, Z, LDZ,\n\tSUBROUTINE ZHSEQR( JOB, COMPZ, N, ILO, IHI, H, LDH, W, Z, LDZ, WORK, LWORK, INFO )\n", input);
    break;
    case CORTB:
      printf("%s:\n\tSUBROUTINE CUNMHR( SIDE, TRANS, M, N, ILO, IHI, A, LDA, TAU, C,\n\tSUBROUTINE ZUNMHR( SIDE, TRANS, M, N, ILO, IHI, A, LDA, TAU, C, LDC, WORK, LWORK, INFO )\n", input);
    break;
    case CORTH:
      printf("%s:\n\tSUBROUTINE CGEHRD( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )\n\tSUBROUTINE ZGEHRD( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )\n", input);
    break;
    case DCHDC:
      printf("%s:\tNo LAPACK equivalent\n", input);
    break;
    case DCHDD:
      printf("%s:\tNo LAPACK equivalent\n", input);
    break;
    case DCHEX:
      printf("%s:\tNo LAPACK equivalent\n", input);
    break;
    case DCHUD:
      printf("%s:\tNo LAPACK equivalent\n", input);
    break;
    case DGBCO:
      printf("%s:\n\tREAL FUNCTION SLANGB( NORM, N, KL, KU, AB, LDAB,\n\tREAL FUNCTION DLANGB( NORM, N, KL, KU, AB, LDAB, WORK )\n\n\tSUBROUTINE SGBTRF( M, N, KL, KU, AB, LDAB, IPIV, INFO )\n\tSUBROUTINE DGBTRF( M, N, KL, KU, AB, LDAB, IPIV, INFO )\n\n\tSUBROUTINE SGBCON( NORM, N, KL, KU, AB, LDAB, IPIV, ANORM, RCOND,\n\tSUBROUTINE DGBCON( NORM, N, KL, KU, AB, LDAB, IPIV, ANORM, RCOND, WORK, IWORK, INFO )\n", input);
    break;
    case DGBDI:
      printf("%s:\tNo LAPACK equivalent\n", input);
    break;
    case DGBFA:
      printf("%s:\n\tSUBROUTINE SGBTRF( M, N, KL, KU, AB, LDAB, IPIV, INFO )\n\tSUBROUTINE DGBTRF( M, N, KL, KU, AB, LDAB, IPIV, INFO )\n", input);
    break;
    case DGBSL:
      printf("%s:\n\tSUBROUTINE SGBTRS( TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV, B, LDB,\n\tSUBROUTINE DGBTRS( TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV, B, LDB, INFO )\n", input);
    break;
    case DGECO:
      printf("%s:\n\tREAL FUNCTION SLANGE( NORM, M, N, A, LDA, WORK )\n\tREAL FUNCTION DLANGE( NORM, M, N, A, LDA, WORK )\n\n\tSUBROUTINE SGETRF( M, N, A, LDA, IPIV, INFO )\n\tSUBROUTINE DGETRF( M, N, A, LDA, IPIV, INFO )\n\n\tSUBROUTINE SGECON( NORM, N, A, LDA, ANORM, RCOND, WORK, IWORK,\n\tSUBROUTINE DGECON( NORM, N, A, LDA, ANORM, RCOND, WORK, IWORK, INFO )\n", input);
    break;
    case DGEDI:
      printf("%s:\n\tSUBROUTINE SGETRI( N, A, LDA, IPIV, WORK, LWORK, INFO )\n\tSUBROUTINE DGETRI( N, A, LDA, IPIV, WORK, LWORK, INFO )\n", input);
    break;
    case DGEFA:
      printf("%s:\n\tSUBROUTINE SGETRF( M, N, A, LDA, IPIV, INFO )\n\tSUBROUTINE DGETRF( M, N, A, LDA, IPIV, INFO )\n", input);
    break;
    case DGESL:
      printf("%s:\n\tSUBROUTINE SGETRS( TRANS, N, NRHS, A, LDA, IPIV, B, LDB, INFO )\n\tSUBROUTINE DGETRS( TRANS, N, NRHS, A, LDA, IPIV, B, LDB, INFO )\n", input);
    break;
    case DGTSL:
      printf("%s:\n\tSUBROUTINE SGTSV( N, NRHS, DL, D, DU, B, LDB, INFO )\n\tSUBROUTINE DGTSV( N, NRHS, DL, D, DU, B, LDB, INFO )\n", input);
    break;
    case DPBCO:
      printf("%s:\n\tREAL FUNCTION SLANSB( NORM, UPLO, N, K, AB, LDAB,\n\tREAL FUNCTION DLANSB( NORM, UPLO, N, K, AB, LDAB, WORK )\n\n\tSUBROUTINE SPBTRF( UPLO, N, KD, AB, LDAB, INFO )\n\tSUBROUTINE DPBTRF( UPLO, N, KD, AB, LDAB, INFO )\n\n\tSUBROUTINE SPBCON( UPLO, N, KD, AB, LDAB, ANORM, RCOND, WORK,\n\tSUBROUTINE DPBCON( UPLO, N, KD, AB, LDAB, ANORM, RCOND, WORK, IWORK, INFO )\n", input);
    break;
    case DPBDI:
      printf("%s:\tNo LAPACK equivalent\n", input);
    break;
    case DPBFA:
      printf("%s:\n\tSUBROUTINE SPBTRF( UPLO, N, KD, AB, LDAB, INFO )\n\tSUBROUTINE DPBTRF( UPLO, N, KD, AB, LDAB, INFO )\n", input);
    break;
    case DPBSL:
      printf("%s:\n\tSUBROUTINE SPBTRS( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )\n\tSUBROUTINE DPBTRS( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )\n", input);
    break;
    case DPOCO:
      printf("%s:\n\tREAL FUNCTION SLANSY( NORM, UPLO, N, A, LDA, WORK )\n\tREAL FUNCTION DLANSY( NORM, UPLO, N, A, LDA, WORK )\n\n\tSUBROUTINE SPOTRF( UPLO, N, A, LDA, INFO )\n\tSUBROUTINE DPOTRF( UPLO, N, A, LDA, INFO )\n\n\tSUBROUTINE SPOCON( UPLO, N, A, LDA, ANORM, RCOND, WORK, IWORK,\n\tSUBROUTINE DPOCON( UPLO, N, A, LDA, ANORM, RCOND, WORK, IWORK, INFO )\n", input);
    break;
    case DPODI:
      printf("%s:\n\tSUBROUTINE SPOTRI( UPLO, N, A, LDA, INFO )\n\tSUBROUTINE DPOTRI( UPLO, N, A, LDA, INFO )\n", input);
    break;
    case DPOFA:
      printf("%s:\n\tSUBROUTINE SPOTRF( UPLO, N, A, LDA, INFO )\n\tSUBROUTINE DPOTRF( UPLO, N, A, LDA, INFO )\n", input);
    break;
    case DPOSL:
      printf("%s:\n\tSUBROUTINE SPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO )\n\tSUBROUTINE DPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO )\n", input);
    break;
    case DPPCO:
      printf("%s:\n\tREAL FUNCTION SLANSY( NORM, UPLO, N, A, LDA, WORK )\n\tREAL FUNCTION DLANSY( NORM, UPLO, N, A, LDA, WORK )\n\n\tSUBROUTINE SPPTRF( UPLO, N, AP, INFO )\n\tSUBROUTINE DPPTRF( UPLO, N, AP, INFO )\n\n\tSUBROUTINE SPPCON( UPLO, N, AP, ANORM, RCOND, WORK, IWORK, INFO )\n\tSUBROUTINE DPPCON( UPLO, N, AP, ANORM, RCOND, WORK, IWORK, INFO )\n", input);
    break;
    case DPPDI:
      printf("%s:\n\tSUBROUTINE SPPTRI( UPLO, N, AP, INFO )\n\tSUBROUTINE DPPTRI( UPLO, N, AP, INFO )\n", input);
    break;
    case DPPFA:
      printf("%s:\n\tSUBROUTINE SPPTRF( UPLO, N, AP, INFO )\n\tSUBROUTINE DPPTRF( UPLO, N, AP, INFO )\n", input);
    break;
    case DPPSL:
      printf("%s:\n\tSUBROUTINE SPPTRS( UPLO, N, NRHS, AP, B, LDB, INFO )\n\tSUBROUTINE DPPTRS( UPLO, N, NRHS, AP, B, LDB, INFO )\n", input);
    break;
    case DPTSL:
      printf("%s:\n\tSUBROUTINE SPTSV( N, NRHS, D, E, B, LDB, INFO )\n\tSUBROUTINE DPTSV( N, NRHS, D, E, B, LDB, INFO )\n", input);
    break;
    case DSICO:
      printf("%s:\n\tREAL FUNCTION SLANSY( NORM, UPLO, N, A, LDA, WORK )\n\tREAL FUNCTION DLANSY( NORM, UPLO, N, A, LDA, WORK )\n\n\tSUBROUTINE SSYTRF( UPLO, N, A, LDA, IPIV, WORK, LWORK, INFO )\n\tSUBROUTINE DSYTRF( UPLO, N, A, LDA, IPIV, WORK, LWORK, INFO )\n\n\tSUBROUTINE SSYCON( UPLO, N, A, LDA, IPIV, ANORM, RCOND, WORK,\n\tSUBROUTINE DSYCON( UPLO, N, A, LDA, IPIV, ANORM, RCOND, WORK, IWORK, INFO )\n", input);
    break;
    case DSIDI:
      printf("%s:\n\tSUBROUTINE SSYTRI( UPLO, N, A, LDA, IPIV, WORK, INFO )\n\tSUBROUTINE DSYTRI( UPLO, N, A, LDA, IPIV, WORK, INFO )\n", input);
    break;
    case DSIFA:
      printf("%s:\n\tSUBROUTINE SSYTRF( UPLO, N, A, LDA, IPIV, WORK, LWORK, INFO )\n\tSUBROUTINE DSYTRF( UPLO, N, A, LDA, IPIV, WORK, LWORK, INFO )\n", input);
    break;
    case DSISL:
      printf("%s:\n\tSUBROUTINE SSYTRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )\n\tSUBROUTINE DSYTRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )\n", input);
    break;
    case DSPCO:
      printf("%s:\n\tREAL FUNCTION SLANSP( NORM, UPLO, N, AP, WORK )\n\tREAL FUNCTION DLANSP( NORM, UPLO, N, AP, WORK )\n\n\tSUBROUTINE SSPTRF( UPLO, N, AP, IPIV, INFO )\n\tSUBROUTINE DSPTRF( UPLO, N, AP, IPIV, INFO )\n\n\tSUBROUTINE SSPCON( UPLO, N, AP, IPIV, ANORM, RCOND, WORK, IWORK,\n\tSUBROUTINE DSPCON( UPLO, N, AP, IPIV, ANORM, RCOND, WORK, IWORK, INFO )\n", input);
    break;
    case DSPDI:
      printf("%s:\n\tSUBROUTINE SSPTRI( UPLO, N, AP, IPIV, WORK, INFO )\n\tSUBROUTINE DSPTRI( UPLO, N, AP, IPIV, WORK, INFO )\n", input);
    break;
    case DSPFA:
      printf("%s:\n\tSUBROUTINE SSPTRF( UPLO, N, AP, IPIV, INFO )\n\tSUBROUTINE DSPTRF( UPLO, N, AP, IPIV, INFO )\n", input);
    break;
    case DSPSL:
      printf("%s:\n\tSUBROUTINE SSPTRS( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )\n\tSUBROUTINE DSPTRS( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )\n", input);
    break;
    case DSVDC:
      printf("%s:\n\tSUBROUTINE SGESVD( JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT,\n\tSUBROUTINE DGESVD( JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT, WORK, LWORK, INFO )\n", input);
    break;
    case DTRCO:
      printf("%s:\n\tSUBROUTINE STRCON( NORM, UPLO, DIAG, N, A, LDA, RCOND, WORK,\n\tSUBROUTINE DTRCON( NORM, UPLO, DIAG, N, A, LDA, RCOND, WORK, IWORK, INFO )\n", input);
    break;
    case DTRDI:
      printf("%s:\n\tSUBROUTINE STRTRI( UPLO, DIAG, N, A, LDA, INFO )\n\tSUBROUTINE DTRTRI( UPLO, DIAG, N, A, LDA, INFO )\n", input);
    break;
    case DTRSL:
      printf("%s:\n\tSUBROUTINE STRTRS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB,\n\tSUBROUTINE DTRTRS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB, INFO )\n", input);
    break;
    case DVD:
      printf("%s:\n\tSUBROUTINE SGESVD( JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT,\n\tSUBROUTINE DGESVD( JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT, WORK, LWORK, INFO )\n", input);
    break;
    case ELMBAK:
      printf("%s:\n\tSUBROUTINE SORMHR( SIDE, TRANS, M, N, ILO, IHI, A, LDA, TAU, C,\n\tSUBROUTINE DORMHR( SIDE, TRANS, M, N, ILO, IHI, A, LDA, TAU, C, LDC, WORK, LWORK, INFO )\n", input);
    break;
    case ELMHES:
      printf("%s:\n\tSUBROUTINE SGEHRD( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )\n\tSUBROUTINE DGEHRD( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )\n", input);
    break;
    case ELTRAN:
      printf("%s:\n\tSUBROUTINE SORGHR( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )\n\tSUBROUTINE DORGHR( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )\n", input);
    break;
    case FIGI2:
      printf("%s:\tNo LAPACK equivalent\n", input);
    break;
    case FIGI:
      printf("%s:\tNo LAPACK equivalent\n", input);
    break;
    case HQR2:
      printf("%s:\n\tSUBROUTINE SHSEQR( JOB, COMPZ, N, ILO, IHI, H, LDH, WR, WI, Z,\n\tSUBROUTINE DHSEQR( JOB, COMPZ, N, ILO, IHI, H, LDH, WR, WI, Z, LDZ, WORK, LWORK, INFO )\n\n\tSUBROUTINE STREVC( JOB, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR,\n\tSUBROUTINE DTREVC( JOB, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR, LDVR, MM, M, WORK, INFO )\n", input);
    break;
    case HQR:
      printf("%s:\n\tSUBROUTINE SHSEQR( JOB, COMPZ, N, ILO, IHI, H, LDH, WR, WI, Z,\n\tSUBROUTINE DHSEQR( JOB, COMPZ, N, ILO, IHI, H, LDH, WR, WI, Z, LDZ, WORK, LWORK, INFO )\n", input);
    break;
    case HTRIB3:
      printf("%s:\n\tSUBROUTINE CUPMTR( SIDE, UPLO, TRANS, M, N, AP, TAU, C, LDC, WORK,\n\tSUBROUTINE ZUPMTR( SIDE, UPLO, TRANS, M, N, AP, TAU, C, LDC, WORK, INFO )\n", input);
    break;
    case HTRIBK:
      printf("%s:\n\tSUBROUTINE CUNMTR( SIDE, UPLO, TRANS, M, N, A, LDA, TAU, C, LDC,\n\tSUBROUTINE ZUNMTR( SIDE, UPLO, TRANS, M, N, A, LDA, TAU, C, LDC, WORK, LWORK, INFO )\n", input);
    break;
    case HTRID3:
      printf("%s:\n\tSUBROUTINE CHPTRD( UPLO, N, AP, D, E, TAU, INFO )\n\tSUBROUTINE ZHPTRD( UPLO, N, AP, D, E, TAU, INFO )\n", input);
    break;
    case HTRIDI:
      printf("%s:\n\tSUBROUTINE CHETRD( UPLO, N, A, LDA, D, E, TAU, WORK, LWORK, INFO )\n\tSUBROUTINE ZHETRD( UPLO, N, A, LDA, D, E, TAU, WORK, LWORK, INFO )\n", input);
    break;
    case IMTQL1:
      printf("%s:\n\tSUBROUTINE SSTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )\n\tSUBROUTINE DSTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )\n", input);
    break;
    case IMTQL2:
      printf("%s:\n\tSUBROUTINE SSTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )\n\tSUBROUTINE DSTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )\n", input);
    break;
    case IMTQLV:
      printf("%s:\n\tSUBROUTINE SSTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )\n\tSUBROUTINE DSTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )\n", input);
    break;
    case INVIT:
      printf("%s:\n\tSUBROUTINE SHSEIN( JOB, EIGSRC, INITV, SELECT, N, H, LDH, WR, WI,\n\tSUBROUTINE DHSEIN( JOB, EIGSRC, INITV, SELECT, N, H, LDH, WR, WI, VL, LDVL, VR, LDVR, MM, M, WORK, IFAILL, IFAILR, INFO )\n", input);
    break;
    case MINFIT:
      printf("%s:\n\tSUBROUTINE SGELSS( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,\n\tSUBROUTINE DGELSS( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK, WORK, LWORK, INFO )\n", input);
    break;
    case ORTBAK:
      printf("%s:\n\tSUBROUTINE SORMHR( SIDE, TRANS, M, N, ILO, IHI, A, LDA, TAU, C,\n\tSUBROUTINE DORMHR( SIDE, TRANS, M, N, ILO, IHI, A, LDA, TAU, C, LDC, WORK, LWORK, INFO )\n", input);
    break;
    case ORTHES:
      printf("%s:\n\tSUBROUTINE SGEHRD( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )\n\tSUBROUTINE DGEHRD( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )\n", input);
    break;
    case ORTRAN:
      printf("%s:\n\tSUBROUTINE SORGHR( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )\n\tSUBROUTINE DORGHR( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )\n", input);
    break;
    case QZHES:
      printf("%s:\tNo LAPACK equivalent\n", input);
    break;
    case QZIT:
      printf("%s:\tNo LAPACK equivalent\n", input);
    break;
    case QZVAL:
      printf("%s:\tNo LAPACK equivalent\n", input);
    break;
    case QZVEC:
      printf("%s:\tNo LAPACK equivalent\n", input);
    break;
    case RATQR:
      printf("%s:\n\tSUBROUTINE SSTEBZ( RANGE, ORDER, N, VL, VU, IL, IU, ABSTOL, D, E,\n\tSUBROUTINE DSTEBZ( RANGE, ORDER, N, VL, VU, IL, IU, ABSTOL, D, E, M, NSPLIT, W, IBLOCK, ISPLIT, WORK, IWORK, INFO )\n", input);
    break;
    case REDUC2:
      printf("%s:\n\tSUBROUTINE SSYGST( ITYPE, UPLO, N, A, LDA, B, LDB, INFO )\n\tSUBROUTINE DSYGST( ITYPE, UPLO, N, A, LDA, B, LDB, INFO )\n", input);
    break;
    case REDUC:
      printf("%s:\n\tSUBROUTINE SSYGST( ITYPE, UPLO, N, A, LDA, B, LDB, INFO )\n\tSUBROUTINE DSYGST( ITYPE, UPLO, N, A, LDA, B, LDB, INFO )\n", input);
    break;
    case RGG:
      printf("%s:\tNo LAPACK equivalent\n", input);
    break;
    case RG:
      printf("%s:\n\tSUBROUTINE SGEEV( JOBVL, JOBVR, N, A, LDA, WR, WI, VL, LDVL, VR,\n\tSUBROUTINE DGEEV( JOBVL, JOBVR, N, A, LDA, WR, WI, VL, LDVL, VR, LDVR, WORK, LWORK, INFO )\n", input);
    break;
    case RSB:
      printf("%s:\n\tSUBROUTINE SSBEV( JOBZ, UPLO, N, KD, AB, LDAB, W, Z, LDZ, WORK,\n\tSUBROUTINE DSBEV( JOBZ, UPLO, N, KD, AB, LDAB, W, Z, LDZ, WORK, INFO )\n", input);
    break;
    case RSGAB:
      printf("%s:\n\tSUBROUTINE SSYGV( ITYPE, JOBZ, UPLO, N, A, LDA, B, LDB, W, WORK,\n\tSUBROUTINE DSYGV( ITYPE, JOBZ, UPLO, N, A, LDA, B, LDB, W, WORK, LWORK, INFO )\n", input);
    break;
    case RSGBA:
      printf("%s:\n\tSUBROUTINE SSYGV( ITYPE, JOBZ, UPLO, N, A, LDA, B, LDB, W, WORK,\n\tSUBROUTINE DSYGV( ITYPE, JOBZ, UPLO, N, A, LDA, B, LDB, W, WORK, LWORK, INFO )\n", input);
    break;
    case RSG:
      printf("%s:\n\tSUBROUTINE SSYGV( ITYPE, JOBZ, UPLO, N, A, LDA, B, LDB, W, WORK,\n\tSUBROUTINE DSYGV( ITYPE, JOBZ, UPLO, N, A, LDA, B, LDB, W, WORK, LWORK, INFO )\n", input);
    break;
    case RSM:
      printf("%s:\n\tSUBROUTINE SSYEVX( JOBZ, RANGE, UPLO, N, A, LDA, VL, VU, IL, IU,\n\tSUBROUTINE DSYEVX( JOBZ, RANGE, UPLO, N, A, LDA, VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK, LWORK, IWORK, IFAIL, INFO )\n", input);
    break;
    case RSP:
      printf("%s:\n\tSUBROUTINE SSPEV( JOBZ, UPLO, N, AP, W, Z, LDZ, WORK, INFO )\n\tSUBROUTINE DSPEV( JOBZ, UPLO, N, AP, W, Z, LDZ, WORK, INFO )\n", input);
    break;
    case RST:
      printf("%s:\n\tSUBROUTINE SSTEV( JOBZ, N, D, E, Z, LDZ, WORK, INFO )\n\tSUBROUTINE DSTEV( JOBZ, N, D, E, Z, LDZ, WORK, INFO )\n", input);
    break;
    case RS:
      printf("%s:\n\tSUBROUTINE SSYEV( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, INFO )\n\tSUBROUTINE DSYEV( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, INFO )\n", input);
    break;
    case RT:
      printf("%s:\tNo LAPACK equivalent\n", input);
    break;
    case SCHDC:
      printf("%s:\tNo LAPACK equivalent\n", input);
    break;
    case SCHDD:
      printf("%s:\tNo LAPACK equivalent\n", input);
    break;
    case SCHEX:
      printf("%s:\tNo LAPACK equivalent\n", input);
    break;
    case SCHUD:
      printf("%s:\tNo LAPACK equivalent\n", input);
    break;
    case SGBCO:
      printf("%s:\n\tREAL FUNCTION SLANGB( NORM, N, KL, KU, AB, LDAB,\n\tREAL FUNCTION DLANGB( NORM, N, KL, KU, AB, LDAB, WORK )\n\n\tSUBROUTINE SGBTRF( M, N, KL, KU, AB, LDAB, IPIV, INFO )\n\tSUBROUTINE DGBTRF( M, N, KL, KU, AB, LDAB, IPIV, INFO )\n\n\tSUBROUTINE SGBCON( NORM, N, KL, KU, AB, LDAB, IPIV, ANORM, RCOND,\n\tSUBROUTINE DGBCON( NORM, N, KL, KU, AB, LDAB, IPIV, ANORM, RCOND, WORK, IWORK, INFO )\n", input);
    break;
    case SGBDI:
      printf("%s:\tNo LAPACK equivalent\n", input);
    break;
    case SGBFA:
      printf("%s:\n\tSUBROUTINE SGBTRF( M, N, KL, KU, AB, LDAB, IPIV, INFO )\n\tSUBROUTINE DGBTRF( M, N, KL, KU, AB, LDAB, IPIV, INFO )\n", input);
    break;
    case SGBSL:
      printf("%s:\n\tSUBROUTINE SGBTRS( TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV, B, LDB,\n\tSUBROUTINE DGBTRS( TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV, B, LDB, INFO )\n", input);
    break;
    case SGECO:
      printf("%s:\n\tREAL FUNCTION SLANGE( NORM, M, N, A, LDA, WORK )\n\tREAL FUNCTION DLANGE( NORM, M, N, A, LDA, WORK )\n\n\tSUBROUTINE SGETRF( M, N, A, LDA, IPIV, INFO )\n\tSUBROUTINE DGETRF( M, N, A, LDA, IPIV, INFO )\n\n\tSUBROUTINE SGECON( NORM, N, A, LDA, ANORM, RCOND, WORK, IWORK,\n\tSUBROUTINE DGECON( NORM, N, A, LDA, ANORM, RCOND, WORK, IWORK, INFO )\n", input);
    break;
    case SGEDI:
      printf("%s:\n\tSUBROUTINE SGETRI( N, A, LDA, IPIV, WORK, LWORK, INFO )\n\tSUBROUTINE DGETRI( N, A, LDA, IPIV, WORK, LWORK, INFO )\n", input);
    break;
    case SGEFA:
      printf("%s:\n\tSUBROUTINE SGETRF( M, N, A, LDA, IPIV, INFO )\n\tSUBROUTINE DGETRF( M, N, A, LDA, IPIV, INFO )\n", input);
    break;
    case SGESL:
      printf("%s:\n\tSUBROUTINE SGETRS( TRANS, N, NRHS, A, LDA, IPIV, B, LDB, INFO )\n\tSUBROUTINE DGETRS( TRANS, N, NRHS, A, LDA, IPIV, B, LDB, INFO )\n", input);
    break;
    case SGTSL:
      printf("%s:\n\tSUBROUTINE SGTSV( N, NRHS, DL, D, DU, B, LDB, INFO )\n\tSUBROUTINE DGTSV( N, NRHS, DL, D, DU, B, LDB, INFO )\n", input);
    break;
    case SPBCO:
      printf("%s:\n\tREAL FUNCTION SLANSB( NORM, UPLO, N, K, AB, LDAB,\n\tREAL FUNCTION DLANSB( NORM, UPLO, N, K, AB, LDAB, WORK )\n\n\tSUBROUTINE SPBTRF( UPLO, N, KD, AB, LDAB, INFO )\n\tSUBROUTINE DPBTRF( UPLO, N, KD, AB, LDAB, INFO )\n\n\tSUBROUTINE SPBCON( UPLO, N, KD, AB, LDAB, ANORM, RCOND, WORK,\n\tSUBROUTINE DPBCON( UPLO, N, KD, AB, LDAB, ANORM, RCOND, WORK, IWORK, INFO )\n", input);
    break;
    case SPBDI:
      printf("%s:\tNo LAPACK equivalent\n", input);
    break;
    case SPBFA:
      printf("%s:\n\tSUBROUTINE SPBTRF( UPLO, N, KD, AB, LDAB, INFO )\n\tSUBROUTINE DPBTRF( UPLO, N, KD, AB, LDAB, INFO )\n", input);
    break;
    case SPBSL:
      printf("%s:\n\tSUBROUTINE SPBTRS( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )\n\tSUBROUTINE DPBTRS( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )\n", input);
    break;
    case SPOCO:
      printf("%s:\n\tREAL FUNCTION SLANSY( NORM, UPLO, N, A, LDA, WORK )\n\tREAL FUNCTION DLANSY( NORM, UPLO, N, A, LDA, WORK )\n\n\tSUBROUTINE SPOTRF( UPLO, N, A, LDA, INFO )\n\tSUBROUTINE DPOTRF( UPLO, N, A, LDA, INFO )\n\n\tSUBROUTINE SPOCON( UPLO, N, A, LDA, ANORM, RCOND, WORK, IWORK,\n\tSUBROUTINE DPOCON( UPLO, N, A, LDA, ANORM, RCOND, WORK, IWORK, INFO )\n", input);
    break;
    case SPODI:
      printf("%s:\n\tSUBROUTINE SPOTRI( UPLO, N, A, LDA, INFO )\n\tSUBROUTINE DPOTRI( UPLO, N, A, LDA, INFO )\n", input);
    break;
    case SPOFA:
      printf("%s:\n\tSUBROUTINE SPOTRF( UPLO, N, A, LDA, INFO )\n\tSUBROUTINE DPOTRF( UPLO, N, A, LDA, INFO )\n", input);
    break;
    case SPOSL:
      printf("%s:\n\tSUBROUTINE SPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO )\n\tSUBROUTINE DPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO )\n", input);
    break;
    case SPPCO:
      printf("%s:\n\tREAL FUNCTION SLANSY( NORM, UPLO, N, A, LDA, WORK )\n\tREAL FUNCTION DLANSY( NORM, UPLO, N, A, LDA, WORK )\n\n\tSUBROUTINE SPPTRF( UPLO, N, AP, INFO )\n\tSUBROUTINE DPPTRF( UPLO, N, AP, INFO )\n\n\tSUBROUTINE SPPCON( UPLO, N, AP, ANORM, RCOND, WORK, IWORK, INFO )\n\tSUBROUTINE DPPCON( UPLO, N, AP, ANORM, RCOND, WORK, IWORK, INFO )\n", input);
    break;
    case SPPDI:
      printf("%s:\n\tSUBROUTINE SPPTRI( UPLO, N, AP, INFO )\n\tSUBROUTINE DPPTRI( UPLO, N, AP, INFO )\n", input);
    break;
    case SPPFA:
      printf("%s:\n\tSUBROUTINE SPPTRF( UPLO, N, AP, INFO )\n\tSUBROUTINE DPPTRF( UPLO, N, AP, INFO )\n", input);
    break;
    case SPPSL:
      printf("%s:\n\tSUBROUTINE SPPTRS( UPLO, N, NRHS, AP, B, LDB, INFO )\n\tSUBROUTINE DPPTRS( UPLO, N, NRHS, AP, B, LDB, INFO )\n", input);
    break;
    case SPTSL:
      printf("%s:\n\tSUBROUTINE SPTSV( N, NRHS, D, E, B, LDB, INFO )\n\tSUBROUTINE DPTSV( N, NRHS, D, E, B, LDB, INFO )\n", input);
    break;
    case SSICO:
      printf("%s:\n\tREAL FUNCTION SLANSY( NORM, UPLO, N, A, LDA, WORK )\n\tREAL FUNCTION DLANSY( NORM, UPLO, N, A, LDA, WORK )\n\n\tSUBROUTINE SSYTRF( UPLO, N, A, LDA, IPIV, WORK, LWORK, INFO )\n\tSUBROUTINE DSYTRF( UPLO, N, A, LDA, IPIV, WORK, LWORK, INFO )\n\n\tSUBROUTINE SSYCON( UPLO, N, A, LDA, IPIV, ANORM, RCOND, WORK,\n\tSUBROUTINE DSYCON( UPLO, N, A, LDA, IPIV, ANORM, RCOND, WORK, IWORK, INFO )\n", input);
    break;
    case SSIDI:
      printf("%s:\n\tSUBROUTINE SSYTRI( UPLO, N, A, LDA, IPIV, WORK, INFO )\n\tSUBROUTINE DSYTRI( UPLO, N, A, LDA, IPIV, WORK, INFO )\n", input);
    break;
    case SSIFA:
      printf("%s:\n\tSUBROUTINE SSYTRF( UPLO, N, A, LDA, IPIV, WORK, LWORK, INFO )\n\tSUBROUTINE DSYTRF( UPLO, N, A, LDA, IPIV, WORK, LWORK, INFO )\n", input);
    break;
    case SSISL:
      printf("%s:\n\tSUBROUTINE SSYTRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )\n\tSUBROUTINE DSYTRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )\n", input);
    break;
    case SSPCO:
      printf("%s:\n\tREAL FUNCTION SLANSP( NORM, UPLO, N, AP, WORK )\n\tREAL FUNCTION DLANSP( NORM, UPLO, N, AP, WORK )\n\n\tSUBROUTINE SSPTRF( UPLO, N, AP, IPIV, INFO )\n\tSUBROUTINE DSPTRF( UPLO, N, AP, IPIV, INFO )\n\n\tSUBROUTINE SSPCON( UPLO, N, AP, IPIV, ANORM, RCOND, WORK, IWORK,\n\tSUBROUTINE DSPCON( UPLO, N, AP, IPIV, ANORM, RCOND, WORK, IWORK, INFO )\n", input);
    break;
    case SSPDI:
      printf("%s:\n\tSUBROUTINE SSPTRI( UPLO, N, AP, IPIV, WORK, INFO )\n\tSUBROUTINE DSPTRI( UPLO, N, AP, IPIV, WORK, INFO )\n", input);
    break;
    case SSPFA:
      printf("%s:\n\tSUBROUTINE SSPTRF( UPLO, N, AP, IPIV, INFO )\n\tSUBROUTINE DSPTRF( UPLO, N, AP, IPIV, INFO )\n", input);
    break;
    case SSPSL:
      printf("%s:\n\tSUBROUTINE SSPTRS( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )\n\tSUBROUTINE DSPTRS( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )\n", input);
    break;
    case SSVDC:
      printf("%s:\n\tSUBROUTINE SGESVD( JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT,\n\tSUBROUTINE DGESVD( JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT, WORK, LWORK, INFO )\n", input);
    break;
    case STRCO:
      printf("%s:\n\tSUBROUTINE STRCON( NORM, UPLO, DIAG, N, A, LDA, RCOND, WORK,\n\tSUBROUTINE DTRCON( NORM, UPLO, DIAG, N, A, LDA, RCOND, WORK, IWORK, INFO )\n", input);
    break;
    case STRDI:
      printf("%s:\n\tSUBROUTINE STRTRI( UPLO, DIAG, N, A, LDA, INFO )\n\tSUBROUTINE DTRTRI( UPLO, DIAG, N, A, LDA, INFO )\n", input);
    break;
    case STRSL:
      printf("%s:\n\tSUBROUTINE STRTRS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB,\n\tSUBROUTINE DTRTRS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB, INFO )\n", input);
    break;
    case SVD:
      printf("%s:\n\tSUBROUTINE SGESVD( JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT,\n\tSUBROUTINE DGESVD( JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT, WORK, LWORK, INFO )\n", input);
    break;
    case TINVIT:
      printf("%s:\n\tSUBROUTINE SSTEIN( N, D, E, M, W, IBLOCK, ISPLIT, Z, LDZ, WORK,\n\tSUBROUTINE DSTEIN( N, D, E, M, W, IBLOCK, ISPLIT, Z, LDZ, WORK, IWORK, IFAIL, INFO )\n", input);
    break;
    case TQL1:
      printf("%s:\n\tSUBROUTINE SSTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )\n\tSUBROUTINE DSTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )\n", input);
    break;
    case TQL2:
      printf("%s:\n\tSUBROUTINE SSTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )\n\tSUBROUTINE DSTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )\n", input);
    break;
    case TQLRAT:
      printf("%s:\n\tSUBROUTINE SSTERF( N, D, E, INFO )\n\tSUBROUTINE DSTERF( N, D, E, INFO )\n", input);
    break;
    case TRBAK1:
      printf("%s:\n\tSUBROUTINE SORMTR( SIDE, UPLO, TRANS, M, N, A, LDA, TAU, C, LDC,\n\tSUBROUTINE DORMTR( SIDE, UPLO, TRANS, M, N, A, LDA, TAU, C, LDC, WORK, LWORK, INFO )\n", input);
    break;
    case TRBAK3:
      printf("%s:\n\tSUBROUTINE SOPMTR( SIDE, UPLO, TRANS, M, N, AP, TAU, C, LDC, WORK,\n\tSUBROUTINE DOPMTR( SIDE, UPLO, TRANS, M, N, AP, TAU, C, LDC, WORK, INFO )\n", input);
    break;
    case TRED1:
      printf("%s:\n\tSUBROUTINE SSYTRD( UPLO, N, A, LDA, D, E, TAU, WORK, LWORK, INFO )\n\tSUBROUTINE DSYTRD( UPLO, N, A, LDA, D, E, TAU, WORK, LWORK, INFO )\n", input);
    break;
    case TRED2:
      printf("%s:\n\tSUBROUTINE SSYTRD( UPLO, N, A, LDA, D, E, TAU, WORK, LWORK, INFO )\n\tSUBROUTINE DSYTRD( UPLO, N, A, LDA, D, E, TAU, WORK, LWORK, INFO )\n\n\tSUBROUTINE SORGTR( UPLO, N, A, LDA, TAU, WORK, LWORK, INFO )\n\tSUBROUTINE DORGTR( UPLO, N, A, LDA, TAU, WORK, LWORK, INFO )\n", input);
    break;
    case TRED3:
      printf("%s:\n\tSUBROUTINE SSPTRD( UPLO, N, AP, D, E, TAU, INFO )\n\tSUBROUTINE DSPTRD( UPLO, N, AP, D, E, TAU, INFO )\n", input);
    break;
    case TRIDIB:
      printf("%s:\n\tSUBROUTINE SSTEBZ( RANGE, ORDER, N, VL, VU, IL, IU, ABSTOL, D, E,\n\tSUBROUTINE DSTEBZ( RANGE, ORDER, N, VL, VU, IL, IU, ABSTOL, D, E, M, NSPLIT, W, IBLOCK, ISPLIT, WORK, IWORK, INFO )\n", input);
    break;
    case TSTURM:
      printf("%s:\n\tSUBROUTINE SSTEBZ( RANGE, ORDER, N, VL, VU, IL, IU, ABSTOL, D, E,\n\tSUBROUTINE DSTEBZ( RANGE, ORDER, N, VL, VU, IL, IU, ABSTOL, D, E, M, NSPLIT, W, IBLOCK, ISPLIT, WORK, IWORK, INFO )\n\n\tSUBROUTINE SSTEIN( N, D, E, M, W, IBLOCK, ISPLIT, Z, LDZ, WORK,\n\tSUBROUTINE DSTEIN( N, D, E, M, W, IBLOCK, ISPLIT, Z, LDZ, WORK, IWORK, IFAIL, INFO )\n", input);
    break;
    case ZBABK2:
      printf("%s:\n\tSUBROUTINE CGEBAK( JOB, SIDE, N, ILO, IHI, SCALE, M, V, LDV,\n\tSUBROUTINE ZGEBAK( JOB, SIDE, N, ILO, IHI, SCALE, M, V, LDV, INFO )\n", input);
    break;
    case ZBAL:
      printf("%s:\n\tSUBROUTINE CGEBAL( JOB, N, A, LDA, ILO, IHI, SCALE, INFO )\n\tSUBROUTINE ZGEBAL( JOB, N, A, LDA, ILO, IHI, SCALE, INFO )\n", input);
    break;
    case ZG:
      printf("%s:\n\tSUBROUTINE CGEEV( JOBVL, JOBVR, N, A, LDA, W, VL, LDVL, VR, LDVR,\n\tSUBROUTINE ZGEEV( JOBVL, JOBVR, N, A, LDA, W, VL, LDVL, VR, LDVR, WORK, LWORK, RWORK, INFO )\n", input);
    break;
    case ZH:
      printf("%s:\n\tSUBROUTINE CHEEV( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, RWORK,\n\tSUBROUTINE ZHEEV( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, RWORK, INFO )\n", input);
    break;
    case ZINVIT:
      printf("%s:\n\tSUBROUTINE CHSEIN( JOB, EIGSRC, INITV, SELECT, N, H, LDH, W, VL,\n\tSUBROUTINE ZHSEIN( JOB, EIGSRC, INITV, SELECT, N, H, LDH, W, VL, LDVL, VR, LDVR, MM, M, WORK, RWORK, IFAILL, IFAILR, INFO )\n", input);
    break;
    case ZOMBAK:
      printf("%s:\n\tSUBROUTINE CUNMHR( SIDE, TRANS, M, N, ILO, IHI, A, LDA, TAU, C,\n\tSUBROUTINE ZUNMHR( SIDE, TRANS, M, N, ILO, IHI, A, LDA, TAU, C, LDC, WORK, LWORK, INFO )\n", input);
    break;
    case ZOMHES:
      printf("%s:\n\tSUBROUTINE CGEHRD( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )\n\tSUBROUTINE ZGEHRD( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )\n", input);
    break;
    case ZOMLR2:
      printf("%s:\n\tSUBROUTINE CUNGHR( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )\n\tSUBROUTINE ZUNGHR( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )\n\n\tSUBROUTINE CHSEQR( JOB, COMPZ, N, ILO, IHI, H, LDH, W, Z, LDZ,\n\tSUBROUTINE ZHSEQR( JOB, COMPZ, N, ILO, IHI, H, LDH, W, Z, LDZ, WORK, LWORK, INFO )\n\n\tSUBROUTINE CTREVC( JOB, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR,\n\tSUBROUTINE ZTREVC( JOB, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR, LDVR, MM, M, WORK, RWORK, INFO )\n", input);
    break;
    case ZOMLR:
      printf("%s:\n\tSUBROUTINE CHSEQR( JOB, COMPZ, N, ILO, IHI, H, LDH, W, Z, LDZ,\n\tSUBROUTINE ZHSEQR( JOB, COMPZ, N, ILO, IHI, H, LDH, W, Z, LDZ, WORK, LWORK, INFO )\n", input);
    break;
    case ZOMQR2:
      printf("%s:\n\tSUBROUTINE CUNGHR( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )\n\tSUBROUTINE ZUNGHR( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )\n\n\tSUBROUTINE CHSEQR( JOB, COMPZ, N, ILO, IHI, H, LDH, W, Z, LDZ,\n\tSUBROUTINE ZHSEQR( JOB, COMPZ, N, ILO, IHI, H, LDH, W, Z, LDZ, WORK, LWORK, INFO )\n\n\tSUBROUTINE CTREVC( JOB, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR,\n\tSUBROUTINE ZTREVC( JOB, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR, LDVR, MM, M, WORK, RWORK, INFO )\n", input);
    break;
    case ZOMQR:
      printf("%s:\n\tSUBROUTINE CHSEQR( JOB, COMPZ, N, ILO, IHI, H, LDH, W, Z, LDZ,\n\tSUBROUTINE ZHSEQR( JOB, COMPZ, N, ILO, IHI, H, LDH, W, Z, LDZ, WORK, LWORK, INFO )\n", input);
    break;
    case ZORTB:
      printf("%s:\n\tSUBROUTINE CUNMHR( SIDE, TRANS, M, N, ILO, IHI, A, LDA, TAU, C,\n\tSUBROUTINE ZUNMHR( SIDE, TRANS, M, N, ILO, IHI, A, LDA, TAU, C, LDC, WORK, LWORK, INFO )\n", input);
    break;
    case ZORTH:
      printf("%s:\n\tSUBROUTINE CGEHRD( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )\n\tSUBROUTINE ZGEHRD( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )\n", input);
    break;
    default:
      printf("%s:\tNo LAPACK equivalent\n", input);
    break;
  };  /* end switch */
}
