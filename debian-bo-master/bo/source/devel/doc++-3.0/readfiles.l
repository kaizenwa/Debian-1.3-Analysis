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


%{
#include <assert.h>
#include <stdio.h>
#include <sys/types.h>
#include "McDirectory.h"


#include "McString.h"
#include "McSorter.h"
#include "nametable.hh"
#include "doc.h"

/*
 *	statics
 */
static NameTable	fileTable ;
static McString*	inputFile ;
static McString		fileName ;
static McString		currDir ;
static int		lineNr ;

static void startfile( const char* file, int line )
{
    lineNr   = line ;
    fileName = file ;
    *inputFile += char( 6 ) ;
    *inputFile += file ;
    *inputFile += char( 6 ) ;
}

extern "C" {
 int	readfilesyywrap( void )		{ return 1 ; }
}

//@ ---------------------------------------------------------------------------
%}

%x Include

%%
^[ \t]*"//@Include:"[ \t]*	{ BEGIN(Include) ; }
<Include>[^ \t\n]*		{
				    int			tmpNr   = lineNr ;
				    McString		tmpName = fileName ;
				    McString		tmpDir  = currDir ;
				    YY_BUFFER_STATE	tmpBuf  = YY_CURRENT_BUFFER ;
				    readfile ( inputFile, yytext, 1 ,currDir) ;
				    currDir=tmpDir;
				    startfile( tmpName, tmpNr ) ;
				    yy_switch_to_buffer( tmpBuf ) ;
				    BEGIN(Include) ;
				}
<Include>.
<Include>\n			{ *inputFile += *yytext ; lineNr++ ; BEGIN(0) ; }
\r\n				{ *inputFile += '\n' ; lineNr++ ; }
\n\r				{ *inputFile += '\n' ; lineNr++ ; }
\r				{ *inputFile += '\n' ; lineNr++ ; }
\n				{ *inputFile += '\n' ; lineNr++ ; }
.				{ *inputFile += *yytext ; }
%%

//@ ---------------------------------------------------------------------------



class StringCompare {
public:
    operator()(const char *t1,const char *t2) {
	 return stricmp(t1,t2);
    }
};

void	readfile( McString* in, const char* file, int startLine, 
		  const char *directory,int scanSubDirs)
{

    // printf ("readfile(%s)\n",file);
    McString path;    
    
    if (scanSubDirs && directory) {
	McString d=directory;
	McDArray<char *> list;
        McDirectory::scan(d, list, "*");
	if (verb) {
	    printf("Scanning for subdirs in %s, found %d files\n",
		   (const char *)d,list.size());
	}	
	int n=0;
	for (int i=0 ; i<list.size() ; i++) {	    
	    McString tmp=d;
	    tmp += McDirectory::pathDelimiter(); tmp+=list[i];
	    if (McDirectory::isDirectory(tmp)) {
		readfile(in,file,1,tmp,1);
		n++;
	    }
	    free(list[i]);
	}
	if (verb)
	    printf("done scanning for subdirs in %s, found %d files\n",
		   (const char *)d,n);
    }

    if (directory) {
	path=directory;
    }
    
    if (file[0]==McDirectory::pathDelimiter()) 
	path=file;
    else {
	if (path.length())
	    if (path[path.length()-1]!=McDirectory::pathDelimiter())
		path+=McDirectory::pathDelimiter();
	path+=file; 
    }

    if( fileTable.has( path ) )
	return ;


    int slash=path.rindex(McDirectory::pathDelimiter());
    
    McString realFile;
	
    if (slash>-1){
	McString dir(path,0,slash+1);
	//printf(" %s: dir=%s\n",file,(const char *) dir);
	currDir = dir;
	realFile=McString(path,slash+1,path.length()-slash-1);
    } else {
	currDir="";
	realFile=path;
    }
    int d=realFile.index('*');
    if (d>-1) {
	McString d=currDir;
	if (!d.length()) {
	    d=".";
	    d+=McDirectory::pathDelimiter();
	}	
	McDArray<char *> list;
	McDirectory::scan(d, list,realFile);

       if (verb) 
	   printf(" %s: dir=%s\n %d files",(const char *)realFile,(const char *) d,list.size());
       for (int i=0 ; i<list.size() ; i++){
	   readfile(in,list[i],1,d);
	   free(list[i]);
       }
       return;
    } 

    if (verb)
	printf("reading file %s\n", file );

    if (McDirectory::isDirectory(path)) {
	if (verb) 
	    printf("file %s is a directory, reading all entries...",(const char *)path);
	if (java) {
	    readfile(in,"*.java",1,path,1);
	} else {
	    readfile(in,"*.h*",1,realFile,1);
	}
	return;
    }
    
    
    FILE *newin = fopen( path, "r" ) ;
    if( newin )
    {
        yyin=newin;
        fileTable.add( 0, path ) ;
        inputFile = in ;
        startfile( file, startLine ) ;
        yy_switch_to_buffer( yy_create_buffer( yyin, YY_BUF_SIZE ) );
	BEGIN(0);
	readfilesyylex() ;
	fclose(yyin);
    }
    else
	fprintf( stderr, "ERROR: could not open file '%s' for reading.\n", file) ;
}
