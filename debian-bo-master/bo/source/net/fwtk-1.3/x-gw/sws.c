/*-
 * Copyright (c) 1993, Trusted Information Systems, Incorporated
 * All rights reserved.
 *
 * Redistribution and use are governed by the terms detailed in the
 * license document ("LICENSE") included with the toolkit.
 */
/*
 *      Author: Wei Xu, Trusted Information Systems, Inc.
 */
static  char    RcsId[] = "Header: ";

#include "ulib.h"
extern char	*strsave();


/* ******************************** freesws ******************************
 * free sws_l
 *************************************************************************/
void	freesws( swsl )
sws_l	*swsl;
{
	sws_t	*psws;

	if(!swsl || !(psws=swsl->sws)) return;
	while( psws->key ) {
		free(psws->key);
		if(psws->arg) free(psws->arg);
		free(psws++);
	}
	free(swsl);
}

/* ******************************** rd_sws *******************************
 * Read the switch in argv. If an argv is match to the switch defined in
 * sws->key, argv++ is to be pointed by sws->arg.
 * return pswsl->cnt:   0 if no args or number of input args.
 * return pswsl: NULL there are some thing wrong or unmatch to the inputs.
 *************************************************************************/
sws_l 	*rd_sws( argc,argv,sws  )
int	argc;
char	*argv[],*sws;
{
	int	i, n, z=sizeof(sws_t);
	sws_l	*pswsl;
	sws_t	*psws;
	char	**plist;

		/* get sws as token pointered by plist */
	if(!(plist=(char**)tokens(sws,":",&n)) || !n) return NULL;

		/* malloc pswsl and init key */
	pswsl=(sws_l*)malloc(sizeof(sws_l));
	bzero((psws=pswsl->sws=(sws_t*)calloc(n+1,z)),(n+1)*z);
	while(n--) pswsl->sws[n].key=plist[n]; 

		/* get inputs matched to sws and save in psws->arg */
	n=argc-1; pswsl->cnt=0;
	for(i=1;i<argc;i++) { 
		while(psws->key) {
			if(argv[i][0]=='-' && i<n &&
			  !strncmp(&argv[i][1],psws->key,strlen(psws->key)) ){
				i++; pswsl->cnt++;   /* got matched inputs */
				psws->arg=strsave(argv[i],strlen(argv[i]));
				goto next;
			} psws++;
		}
		freesws(pswsl); return NULL; /* Ooops, input not match */
		next: psws=pswsl->sws;
	}
	return pswsl;	/* pswsl->cnt has the # of matched inputs */
}

/* ************** getarg *******************************************
 * get the arg point saved by rd_sws with key match to sws->key.
 * return NULL if unmatch or no key entry. 
 * return the point if found in sws table.        
 *******************************************************************/
char	*getarg( key,swsl )
char	*key;
sws_l	*swsl;
{
	int	n=(!key || !*key)?0:strlen(key);
	sws_t	*sws;

	if(!n || !swsl || swsl->cnt<0 || !(sws=swsl->sws)) return NULL;
	while(sws->key) {
		if(!strncmp(sws->key,key,n)) return sws->arg;
		sws++;
	}
	return NULL;
}

/* ************** setarg *********************************************
 * set an arg in the sws. return arg if success or NULL if failed.
 *********************************************************************/
char	*setarg( key,arg,swsl )
char    *key,*arg;
sws_l   *swsl;
{
	int     n;
	sws_t   *sws;

	if(!swsl || !key || !*key) return NULL;

	sws=swsl->sws; n=strlen(key);
	while(sws->key) {
		if(!strncmp(sws->key,key,n)) {
			if(sws->arg) free(sws->arg);
			sws->arg=(arg && *arg)?strsave(arg,strlen(arg)):NULL;
			return sws->arg;
		} sws++;
	}
	return NULL;
}

