/*==========================================================================
 =                                                                         =
 =                        Project CTI-Print                                =
 =                                                                         =
 = Author:                                                                 =
 =   Panos Dimakopoulos, Systems Programmer,                               =
 =   Computer Technology Institute,                                        =
 =   Division of Computing Facilities,                                     =
 =   P.O. Box 1122,                                                        =
 =   261 10  Patras,                                                       =
 =   Greece                                                                =
 =   (e-mail: dimakop@cti.gr)                                              =
 =   Tel: +30 61 992061                                                    =
 =   Fax: +30 61 993973                                                    =
 =                                                                         =
 = Created by Patrick Powell  <papowell@sdsu.edu>                          =
 =   for LPRng software Sat Aug 26 06:54:25 PDT 1995                       =
 ==========================================================================*/


/*==========================================================================
  Version CTI-LPRng-1.0
  =========================================================================*/


/*
 * $Id: fonts.c,v 1.5 1996/11/14 19:56:28 papowell Exp papowell $
 */

#include "portable.h"
#include "common.h"
#include "hp4.h"


/*
 * Unfortunately the series 4 printers cannot always keep the downloaded
 * fonts permanently. They can only when they have the Resource Saving
 * option installed. Details about this can be obtained from the HP
 * LaserJet 4 Plus and 4M Plus Printer User's Manual" on page 5-9.
 * Therefore the RESRCSAVE env var in ifhp4.sh denotes if such an option
 * is in place.
 */

/*
 * This function decides which font to download by reading 
 * MACHINESFONTS file.
 */
void selectfont()
{
	FILE *pfu = 0;    /* previous font used */
	char prevfnt[MAXPATHLEN];
	char *fntname, *t;
	int len;

	/* we force downloading if no resource saving */
	if ( !resourcesave ) update_pfu( pfu_fname, " " );

	/* find font for current job */
	fntname = 0;
	if(zopts != NULL){
        if ((fntname=strstr(zopts,"font")) != NULL){
			log(1,"-Zfont option detected");
			if( (fntname=strchr(fntname,'=')) ){
				static char *dups;
				if( dups ) free(dups);
				dups = 0;

				fntname++;
				/* use a duplicate of the string */
				fntname = strdup(fntname);
				if( (t = strpbrk(fntname, " \t" )) ) *t = 0;
			}
		}
	}
	
	if( fntname == 0 ){
		fntname = fnt_scan(host);
	}

	if( fntname == 0 ){
		fntname = default_font;
	}

	if( strcasecmp(fntname,"NONE") ){
		if( fntname[0] != '/' ){
			log(4,"selectfont: font name '%s' not abosolute path", fntname );
			return;
		}
	} else {
		log(4,"selectfont: NO default font, sending '%s'", PGRESET );
		len = strlen(PGRESET);
		if (writecn(STDOUT, PGRESET, len) != len){
			logerr(3,"Cannot send PGRESET string");
		}
		update_pfu( pfu_fname, " " );
		return;
	}

	log(4,"selectfont: font '%s'", fntname );

	/* find font used on last job */
	prevfnt[0] = 0;
	log(4,"selectfont: pfu_fname=[%s]",pfu_fname);
	if ( (pfu=fopen(pfu_fname,"r")) == NULL){
		logerr(3,"Cannot open pfu file");
	} else if( fgets(prevfnt, sizeof(prevfnt), pfu) == 0) {
		logerr(3,"Cannot read previously used font");
	} else {
		fclose(pfu);
	}
	if( (t = strchr(prevfnt, '\n')) ) *t = 0;

	/* if fonts different, send new font, else do select old one */
	if( strcmp(prevfnt,fntname) ){
		log(2, "Downloading font '%s'", fntname );
		newfont(fntname);
	}
	/* select the font either downloaded or from previous job */
	t = SELFONT;
	len = strlen( t );
	if (writecn(STDOUT, t, len ) != len ) {
		log(2,"Cannot send SELFONT string '%s'", SELFONT );
		updatepfu=TRUE;
	} else {
		log(2,"Font selected successfully");
	}
	if( updatepfu ){
		update_pfu( pfu_fname, fntname );
	} else {
		delete_pfu(pfu_fname);
	}

	log(4,"selectfont: done");
}

void newfont(fontname)
char *fontname;
{
	char *s = DELFONTS;
	int len = strlen(DELFONTS);

	log(4,"newfont: sending %s", DELFONTS );
	updatepfu=FALSE;
	if( writecn(STDOUT, s, len ) != len ){
		log(2, "Cannot delete fonts" );
	} else if( sendfont(fontname) == 0 ){
		updatepfu=TRUE;
	}
}

char *fnt_scan( mchn )
char *mchn;
{
	FILE *fontfp;
	char *s, *name, *dir, *file;
	char *end = 0;
	char fntname[MAXPATHLEN];
	static char finalfntname[MAXPATHLEN];

	if( mchn == 0 || *mchn == 0 ) return( (char *) 0 );

	s = MACHINESFONTS;
	log(4, "Opening host font file '%s'", s);
	if( (fontfp = fopen( s, "r" )) == NULL ){
		log(2, "fnt_scan: missing host font file '%s'", s );
		return( (char *)0 );
	}

	while( end == 0 && fgets( fntname, sizeof(fntname)-1, fontfp ) ) {
		log(4, "line '%s'", fntname);
		if( (s = strchr( fntname, '\n' ) ) ) *s = 0;
		name = fntname;
		while( isspace(*name) ) ++name;
		if ( *name == '#' )	continue;
		dir = strpbrk(name, " \t" );
		if( dir == 0 ) continue;
		while( isspace( *dir ) ) *dir++ = 0;
		if( *dir == 0 ) continue;
		log(4,"fnt_scan: checking '%s' = '%s', dir '%s'", mchn, name, dir );
		if( strcasecmp( mchn, name ) ) continue;
		file = strpbrk(dir, " \t" );
		if( file ){
			*file++ = 0;
			while( isspace(*file) ) ++file;
			s = strpbrk(file, " \t" );
			if( s ) *s = 0;
		}
		log(4,"fnt_scan: dir '%s' file '%s'", dir, file );
		if( *dir != '/' ){
			strcpy( finalfntname, FONTPATH );
			s = &finalfntname[strlen( finalfntname ) ];
			if( s[-1] != '/' ) s[0] = '/';
		}
		strcat( finalfntname, dir );
		if( file  && *file && *file != '/' ){
			s = &finalfntname[strlen( finalfntname ) ];
			if( s[-1] != '/' ) s[0] = '/';
		}
		if( file ) strcat( finalfntname, file );
		end = finalfntname;
	}

	fclose( fontfp );
	return( end );
}
