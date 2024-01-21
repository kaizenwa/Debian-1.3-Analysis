/*================================================================
 * SoundFont(tm) file format
 *================================================================*/

#ifndef SBK_H_DEF
#define SBK_H_DEF

typedef struct _tchunk {
	char id[4];
	int size;
} tchunk;

typedef struct _tsbkheader {
	char riff[4];	/* RIFF */
	int size;	/* size of sbk after there bytes */
	char sfbk[4];	/* sfbk id */
} tsbkheader;

typedef struct _tsamplenames {
	char name[20];
} tsamplenames;

typedef struct _tpresethdr {
	char name[20];
	unsigned short preset, bank, bagNdx;
	/*int lib, genre, morphology;*/ /* reserved */
} tpresethdr;

typedef struct _tsampleinfo {
	int startsample, endsample;
	int startloop, endloop;
	/* ver.2 additional info */
	int samplerate;
	unsigned char originalPitch;
	unsigned char pitchCorrection;
	unsigned short samplelink;
	unsigned short sampletype;  /*1=mono, 2=right, 4=left, 8=linked, $8000=ROM*/
} tsampleinfo;

typedef struct _tinsthdr {
	char name[20];
	unsigned short bagNdx;
} tinsthdr;

typedef struct _tgenrec {
	short oper;
	short amount;
} tgenrec;


/*
 *
 */

typedef struct _SFInfo {
	short version, minorversion;
	long samplepos, samplesize;

	int nrsamples;
	tsamplenames *samplenames;

	int nrpresets;
	tpresethdr *presethdr;
	
	int nrinfos;
	tsampleinfo *sampleinfo;

	int nrinsts;
	tinsthdr *insthdr;

	int nrpbags, nribags;
	unsigned short *presetbag, *instbag;

	int nrpgens, nrigens;
	tgenrec *presetgen, *instgen;

	tsbkheader sbkh;

} SFInfo;


/*----------------------------------------------------------------
 * functions
 *----------------------------------------------------------------*/

void load_sbk(FILE *fp, SFInfo *sf);
void free_sbk(SFInfo *sf);
void sbk_to_text(char *text, int type, int val, SFInfo *sf);

#endif
