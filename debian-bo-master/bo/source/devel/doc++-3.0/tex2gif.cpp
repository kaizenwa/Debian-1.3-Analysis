/*************************************************************************

    DOC++, a C++ (and C) documentation system for LaTeX and HTML

	    Copyright (C) 1996  Roland Wunderling,
				Malte Zoeckler


    DOC++ is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation. This program
    is distributed WITHOUT ANY WARRANTY; without even the implied
    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    See the GNU General Public License for more details.

    If you intend to use DOC++ commercially on a regular basis you
    must contact the authors for an appropriate donation.

 *************************************************************************/

#include <string.h>
#include <stdio.h>
#include <iostream.h>
#include <fstream.h>
#include <unistd.h>

#include "nametable.hh"
#include "doc.h"

extern NameTable	gifs ;
extern int		forceGifs ;
extern char*		texFile ;
extern char*		texOption ;
extern char*		texPackage ;

void	makeGif( int gifNum, const char* tex )
{
    cerr << "makeing gif nr " << gifNum << ":	'" << tex << "'\n" ;
}

void _system (const char *b)
{
    fprintf(stderr,"SYSTEM: %s\n",b);
    system(b);
}

void	makeGifs(const char *dir)
{       
    char buf[200];
    char buf2[200];
    McDArray<int> tog; // Table of GIFs

    char *olddir=0;
    if (dir) {
	olddir  = getcwd(NULL,200);
	chdir(dir);
    }
    
    FILE *texfile = fopen("dxxgifs.tex","w");
    if (!texfile) {
	printf("ERROR: can't open %s/dxxgifs.tex for writing",dir);
	return  ;
    }

    if( texFile )
    {
	ifstream env( texFile ) ;
	if( env )
	{
	    char	ch ;
	    while( env )
	    {
		env >> ch ;
		putc( ch, texfile ) ;
	    }
	}
	else
	    cerr << "could not open " << texFile << endl ;
    }
    else
    {
	fprintf( texfile, "\\documentclass" ) ;
	if( texOption )
	    fprintf( texfile, "[%s]", texOption ) ;
	fprintf( texfile, "{article}\n" ) ;
	if( texPackage )
	    fprintf( texfile, "\\usepackage{%s}\n", texPackage ) ;
    }
    fprintf( texfile, "\\pagestyle{empty}\n" ) ;
    fprintf( texfile, "\\begin{document}\n" ) ;

    for( gifs.first() ; gifs.current() ; gifs.next() )
    {
	int	n = gifs[gifs.current()] ;
	sprintf(buf,"g%06d.gif",n);
	FILE *exist = fopen(buf,"r");
	if (!exist || forceGifs){
	    // Roland replaced this line:
	    // fprintf(texfile,"%s\n\n\n\\pagebreak\n\n\n",gifs.current());
	    // by the following 3 lines:
	    out = texfile ;
	    printYYDOC( 0, gifs.current() );
	    fprintf(texfile,"\n\n\n\\pagebreak\n\n\n");
	    tog.append(n);
	}
	if( exist)
	    fclose(exist);
    }
    fprintf(texfile,"\\end{document}\n");
    fclose(texfile);


    int i,numPages= tog.size();

    if (numPages)
	_system("latex dxxgifs.tex </dev/null");

    for ( i=0 ; i<numPages ; i++){
	sprintf(buf,"dvips -D 600 -E -n 1 -p %d -o dxx%04d.eps dxxgifs.dvi",
		i+1,i);
	_system(buf);
    }
    
    int x1,x2,y1,y2;
    for ( i=0 ; i<numPages ; i++){
	sprintf(buf,"dxx%04d.eps",i);
 	FILE *in=fopen(buf,"r");
	if( in )
	{
	    int num=0;
	    while (!feof(in)) {
		fgets(buf2,200,in);
		if (strncmp("%%BoundingBox:",buf2,14)==0){
		    num=sscanf (buf2,"%%%%BoundingBox:%d %d %d %d",
				&x1,&y1,&x2,&y2);
		    break;
		}
	    }
	    if (num!=4)
		printf("ERROR: Couldn't extract BoundingBox "
		       "from dxx%04d.eps.\n",i);
	    fclose(in);

	    FILE *psfile = fopen("dxxps.ps","w");
	    
	    fprintf(psfile,
		    ".7 .7 .7 setrgbcolor newpath -1 -1 moveto %d -1 lineto %d %d "
		    "lineto -1 %d lineto closepath fill \n"
		    "-%d -%d translate "
		    "0 0 0 setrgbcolor \n (dxx%04d.eps) run",
		    x2-x1+2,x2-x1+2,y2-y1+2,y2-y1+2,x1,y1,i);

	    fclose(psfile);
	    float resfac=4;
	    
	    int gx=(int)((x2-x1)*resfac);
	    int gy=(int)((y2-y1)*resfac);
	    
	    gx=((gx+7)/8)*8;
	    
	    sprintf(buf,"gs -g%dx%d -r%dx%d -sDEVICE=ppmraw -sOutputFile=dxxtmp.pnm -DNOPAUSE -- dxxps.ps",
		    gx,gy,(int)(resfac*72),(int)(resfac*72),i);
	    
	    _system(buf);

	    sprintf(buf,"pnmscale -xscale .333 -yscale .333 dxxtmp.pnm | "
		    "pnmgamma .9 | "
		    "ppmtogif -transparent rgb:ffff/ffff/ffff > g%06d.gif",tog[i]);
	    
	    _system(buf);	
	}
	else
	{
	    cerr << "WARNING: Problems generating gifs.\n"
		 << "         Check latex file dxxgifs.tex for latex errors.\n" ;
	    break ;
	}
    }

    if (olddir){
	chdir(olddir);
	free(olddir);
    }
}

