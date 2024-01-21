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


#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <iostream.h>
#include <iostream.h>

#include <sys/types.h>
#include <sys/stat.h>

#include "doc.h"
#include "java.h"
#include "gifs.h"
#include "classgraph.hh"
#include "McHashTable.h"
#include "McSorter.h"

#ifdef __BORLANDC__
#include <dir.h>
#endif
#ifdef __VISUALC__
#include <direct.h>
#endif

#define max(a,b) ((a)>(b) ? (a) : (b))

static McString banner;
static char *HTML_SUFFIX="html";

static McString indexHeader("<html><head><TITLE>Table of Contents</TITLE>"
			    "</head><BODY>\n");
static McString indexFooter("<I><A HREF=\"HIER.html\"> hierarchy of classes</A></I><P></BODY>");

static McString hierHeader("<html><head><TITLE>Hierarchy ClassDoc</TITLE></head>\n<BODY>\n");
static McString hierFooter("<I><A HREF=\"aindex.html\"> alphabetic index</A></I><P></BODY>");

static McString pageHeader("<body>\n");
static McString pageFooter("<I><A HREF=\"aindex.html\"> alphabetic index</A></I>  <I><A HREF=\"HIER.html\"> hierarchy of classes</A></I><P></BODY>");


int makedir(const char *d, int perm)
{
#ifdef __BORLANDC__
return mkdir(d);
#else
#ifdef __VISUALC__
return _mkdir(d);
#else
return mkdir(d,perm);
#endif
#endif
}

#define myisalnum(c) ((c>='a' && c<='z')||(c>='A' && c<='Z')||\
		      (c>='0' && c<='9')||c=='_'||c=='.'||c=='-')

char *makeFileName(const char* str)
{
    static McHashTable<char *,int> files(1);
    McString s,ls;
    int l=strlen(str);
    static int cnt=0;
    if (shortFilenames){
	s="f";
	cnt++;
	char buf[40];
	sprintf(buf,"%d",cnt);
	s+=buf;
    } else {
	for (int i=0 ; i<l ; i++){
	    if (myisalnum(str[i])){
	      char c=str[i];
	      s+=c;
	      if (c>='A' && c<='Z')
		c+=(((int)'a')-'A');
	      ls+=c;
	    }
	}
	char *tmp=strdup(ls);
	int *val=files.insert(tmp);	
	if (verb)
	    printf("Inserted %s, got %p (=%d)\n",(const char *)s,val,*val);	
	if (*val>1) {
	    free(tmp);
	    s+='.';
	    char buf[40];
	    sprintf(buf,"%d",*val);
	    s+=buf;
	}
	(*val)++;
    }
    s+=".";
    s+=HTML_SUFFIX;
    return strdup((const char *)s) ;
}

int subEntryIsToBeDocumented(Entry *entry)
{
    int toBeDocumented = 0;
    if (entry->protection!=PRIV || withPrivate) {
	if (entry->retrn.length()  ||
	    entry->param.size() || entry->author.length() ||
	    entry->see.size()   || entry->version.length())
	    toBeDocumented = 1;
	if (entry->doc.length() > entry->memo.length() || 
	    (entry->doc.length() == entry->memo.length() &&
	     entry->doc == (const char *)entry->memo))
	    toBeDocumented = 1;
    }

    if (alwaysPrintDocSection)
	toBeDocumented = 1;
    return toBeDocumented;
}


/// Writes classgraphs. Either as text-graphs or as java-graphs.
class ClassGraphWriter
{
public:
    static void writeJava(FILE *f,Entry *e,int directOnly=1);
    static void writeText(FILE *f,Entry *e);
    static void write(FILE *f,Entry *e);
	static void writeImplements(FILE *f,Entry *e);
};


/// This class writes members.
class MemberWriter
{
protected:
    McString heading;
    int first;
    FILE *f;
    virtual char* startString() {return "<DL>";};
    virtual char* endString() {return "</DL>";};
    virtual void showSubMembers(Entry *);
    struct ListEntry {
	Entry* entry;
	int links;
	int withSub;
    };
    McDArray<ListEntry> list;
    virtual void writeMember(Entry *e, int links,int withSub=1);
    class EntryCompare {
    public:
	operator()(const ListEntry& l1,const ListEntry& l2) {
	    return stricmp(l1.entry->name,l2.entry->name);
	}
    };
public:
    virtual void sort() {
	EntryCompare comp;
	if (list.size())
	    ::sort((ListEntry *)list,list.size(),comp);
    }
    virtual void startList(FILE *f,char *heading,int withLinks);
    virtual void addMember(Entry *e, int links,int withSub=1) {
	ListEntry le;
	le.entry=e;
	le.links=links;
	le.withSub=withSub;
	list.append(le);
    }
    virtual void endList();
};

class MemberWriterTable : public MemberWriter
{
protected:
    virtual char* startString() {
	if (withBorders)
	    return "<TABLE BORDER>"; else  return "<TABLE>";
    };
    virtual char* endString() {return "</TABLE>";};
    virtual void writeMember(Entry *e, int links,int withSub=1);
public:
};

class SortedList : public MemberWriter
{
protected:
    struct ListEntry {
	char *string;
	char *name;
    };
    McDArray<ListEntry> list;
public:
    virtual void sort() {
	EntryCompare comp;
	if (list.size()>1)
	    ::sort((ListEntry *)list,list.size(),comp);
    }
    class EntryCompare {
    public:
	operator()(const ListEntry& l1,const ListEntry& l2) {
	    return stricmp(l1.name,l2.name);
	}
    };
    void addString(const char *name,const char *string);
    void write(FILE *f);
    ~SortedList();
};

void SortedList::write(FILE *f)
{
    for (int i=0 ; i<list.size() ; i++)
	fprintf(f,"%s",list[i].string);
}

void SortedList::addString(const char *name,const char *string)
{
    ListEntry le;
    le.string=strdup(string);
    le.name=strdup(name);
    list.append(le);
}

SortedList::~SortedList()
{
    for (int i=0 ; i<list.size() ; i++){
	if (list[i].string)
	    free(list[i].string);
	if (list[i].name)
	    free(list[i].name);
    }
}

void MemberWriter::showSubMembers(Entry *e)
{
    fprintf(f,"\n%s\n",startString());
    for (Entry *tmp=e->sub ; tmp ; tmp=tmp->next){
	writeMember(tmp,1);
    }
    fprintf(f,"\n%s\n",endString());
}

void printRefLabel(FILE *f,Entry *entry) 
{
    fprintf(f, "\n<A NAME=\"%s\">\n<A NAME =\"DOC.",(const char *)entry->name);
    entry->dumpNumber( f ) ;
    fprintf( f, "\">\n");
}

void MemberWriter::writeMember(Entry *entry,int link, int withSub)
{
    if (first) {
	fprintf(f,"%s\n%s",startString(),(const char *)heading);	
	first=0;
    }

    if (!subEntryIsToBeDocumented(entry) && link) 
	printRefLabel(f,entry);

    fprintf(f,"<DT>");
    if ( subEntryIsToBeDocumented(entry) && link) {
	fprintf( f, "<A HREF=\"#DOC." ) ;
	entry->dumpNumber( f ) ;
	fprintf( f, "\"> <IMG BORDER=0 SRC=icon1.gif></A> " ) ;
    } else if (entry->section!=MANUAL_SEC){
	fprintf(f,"<IMG SRC=icon2.gif> ");
    } else 
	fprintf(f,"<P>");
   
    char *args=entry->hargs;
    char *type=entry->htype;
    
    fprintf(f,"%s ",type); 
    fprintf(f,"<B>%s</B>",entry->hname);
    
    if (link)
	fprintf(f,"%s </B>\n <DD><I>%s</I>\n",
		args,
		(entry->hmemo));
    else
	fprintf(f,"%s\n",args);
    if (entry->sub && withSub)
	showSubMembers(entry);
}

void MemberWriter::startList(FILE *file,char *head,int)
{
    f=file;
    heading=head;
    first=1;
    list.resize(0);
}

void MemberWriter::endList()
{
    if (sortEntries) {
	sort();
    }
    for (int i=0 ; i<list.size() ; i++)
	writeMember(list[i].entry,list[i].links,list[i].withSub);
    
    if (!first)
	fprintf(f,"%s",endString());
}

void MemberWriterTable::writeMember(Entry *entry,int link, int /*withSub*/)
{
    if (first) {
	fprintf(f,"%s\n%s",startString(),(const char *)heading);	
	first=0;
    }
    fprintf(f,"<TR>");
    fprintf(f,"<TD VALIGN=top>");

    if (!subEntryIsToBeDocumented(entry) && link) 
	printRefLabel(f,entry);

    if (subEntryIsToBeDocumented(entry) && link) {
	fprintf( f, "<A HREF=\"#DOC." ) ;
	entry->dumpNumber( f ) ;
	fprintf( f, "\"> <IMG BORDER=0 SRC=icon1.gif></A> " ) ;
    } else if (entry->section!=MANUAL_SEC){
	fprintf(f,"<IMG SRC=icon2.gif> ");
    } else fprintf(f,"<P>");
    
    char *args=entry->hargs;
    char *type=entry->htype;
    
    fprintf(f,"%s",type); 
    fprintf(f,"</TD><TD>");
    fprintf(f,"<B>%s</B> %s<br>",entry->hname,args);
    
    if (link)
	fprintf(f,"\n<I>%s</I>\n",(entry->hmemo));
    if (entry->sub )
	showSubMembers(entry);
    fprintf(f,"</TD></TR>");  
}


void ClassGraphWriter::writeJava(FILE *f,Entry *entry,int directOnly)
{
    ClassGraph	cg( entry, 0 );
    ClassGraph*	cls = &cg ;
    cg.addBases() ;

    if (directOnly)
	cg.addDirectChilds() ;
    else
	cg.addAllChilds() ;

    McString classes,before,after,indent;
    char first=1;
    int numLines=0,longest=0;
    for( cls = cg.firstLine ; cls ; cls = cls->nextLine ){
	numLines++;    
	if (first)
	    first=0;
	else {
	    classes+=",";
	    before +=",";
	    after  +=",";
	    indent +=",";
	}
	if (cls->entry){
	    if (longest<cls->entry->name.size())
		longest=cls->entry->name.size();
	    
	    if (cls->entry->section==CLASS_SEC)
		classes+="C";
	    else
		classes+="I";
	    classes+=cls->entry->name;
	    classes+=",M";
	    classes+=cls->entry->fileName;
	} else {
	    if (longest<cls->name.size())
		longest=cls->name.size();
	    classes+="M"+cls->name;
	    classes+=",M";
	}
	before+="M"+cls->before;
	after+="M"+cls->after;
	char buf[20];
	sprintf(buf,"%d",cls->indent);
	indent+=buf;
    }
    
    fprintf(f,"<APPLET CODE=\"ClassGraph.class\" WIDTH=600 HEIGHT=%d>\n",
	    numLines*30+5);
    fprintf(f,"<param name=classes value=\"%s\">\n",(const char *)classes);
    fprintf(f,"<param name=before value=\"%s\">\n",(const char *)before);
    fprintf(f,"<param name=after value=\"%s\">\n",(const char *)after);
    fprintf(f,"<param name=indent value=\"%s\">\n",(const char *)indent);
    fprintf(f,"<param name=arrowdir value=");
    if (upArrows)
	fprintf(f,"\"up\">\n");
    else
	fprintf(f,"\"down\">\n");

  fprintf(f,"</APPLET>\n");    
}

void ClassGraphWriter::writeText(FILE *f,Entry *e)
{
    int i;
    for (i=0 ; i<max(1,e->baseclasses.size()) ; i++){
	if (i < e->baseclasses.size() && e->baseclasses[i]->section == INTERFACE_SEC) {
	    if (i==0) {
		fprintf(f,"<H3> %s</h3>\n",e->hname);
	    }
	    continue;
	}
	fprintf(f,"<H3> %s\n",e->hname);
	Entry* c=e ;
	while (c->baseclasses.size()>0){
	    if (c==e) c=c->baseclasses[i]; else  c=c->baseclasses[0];
	    if (c)
		fprintf(f,"&lt <A HREF=\"%s\"> %s </A>\n",(const char*)(c->fileName),
			(const char*)(c->hname));
	    else
		fprintf(f,"&lt %s </A>\n",(const char*)(c->name));  
	}
	fprintf(f,"</h3>\n");	
    }
}

void ClassGraphWriter::write(FILE *f,Entry *e)
{
    if (javaGraphs)
	writeJava(f,e);
    else 
	writeText(f,e);
}

void ClassGraphWriter::writeImplements(FILE *f,Entry *e)
{
    int i,first=1;
    for (i=0 ; i<e->baseclasses.size() ; i++){
		if (e->baseclasses[i]->section != INTERFACE_SEC)
			continue;

		if (first) 
				fprintf(f,"<hr>\n <h2> Implements:</h2>\n");
		first=0;

		Entry* c=e->baseclasses[i];
			if (i>0)
				fprintf(f,", ");
			if (c)
				fprintf(f,"<A HREF=\"%s\"> %s </A>",(const char*)(c->fileName),
					(const char*)(c->hname));
		
    }
}

void copyright(FILE *f)
{
  fprintf (f,"<hr>\n") ;
  if (banner.length()<=0 && ownBanner==0){
      fprintf (f,"<A HREF=\"http://www.zib.de/Visual/software/doc++/index.html\">"
	       "<IMG BORDER=0 ALIGN=RIGHT SRC=logo.gif></A>\n");
      fprintf (f,"<P Align=Center><I>this page has been generated automatically by doc++\n");
      fprintf (f,"<P Align=Center>(c)opyright by <A HREF=\"http://www.zib.de/zoeckler/\"> Malte  Z&oumlckler</A>, "
	       "<A HREF=\"mailto:wunderling@zib.de\"> Roland Wunderling </A>"
	       "<br>contact: <A HREF=\"mailto:doc++@zib.de\"> doc++@zib.de </a>");
  } else {
      fprintf (f,"%s\n",(const char *)banner);
      fprintf(f,"<P Align=right><I>generated by <A HREF=\"http://www.zib.de/Visual/software/doc++/index.html\">doc++</A></I>");
  }
}

extern char *strToHtml(const char *in,char *dest=0, Entry* ct=0,int withLinks=0);
extern char *seeToHtml(const char *in, Entry* ct=0);

void	entry2link( McString& u, Entry* ref,const char *linkname)
{
    Entry *globref = ref ;
    while( globref->parent != root && !globref->fileName.length())
	globref = globref->parent ;
    if( ref->fileName.length()) {
	u+="<!1><A HREF=\"" ; 
	u+=ref->fileName ; 
	u+="\">" ; 
	if (linkname)
	  u+=linkname;
	else
	  u+=ref->hname ;
	u+="</A>";
    } else if (globref){
	u+="<!2><A HREF=\""+globref->fileName+"#DOC." ;
	ref->dumpNumber( u ) ;
	u+="\">" ;
	if (linkname)
	    u+=linkname;
	else
	    u+=ref->hname ;
	u+="</A>";
    }
}  


void writeTOCentry(McString &out,Entry *e,int memo)
{ 
    McString link;
    if (e->fileName.length()){
	link= "<A HREF=\"";
	link+=(const char *)(e->fileName);
	link+="\">";
	link+=(const char *)(e->hname);
	link+="</A>";
    } else {
	entry2link(link,e,(const char *)(e->hname));
    }
    out+=link;
    if (memo){
	if (e->memo.length()) {
	    out+=" <I>";
	    out+=(const char*)e->hmemo;
	    out+="</I>\n";
    } else
	out+='\n';    
    }
}
 
void writeTOCentry(FILE *f,Entry *e,int memo)
{ 
    McString out;
    writeTOCentry(out,e,memo);
    fprintf(f,"%s",(const char *)out);
}

void writeTOCentry(SortedList& l,Entry *e,int memo)
{ 
    McString out("<LI>");
    writeTOCentry(out,e, memo);
    l.addString(e->name,(const char *)out);
}

void writeHIERentry(FILE *f,Entry *k,int memo)
{
    fprintf(f,"<LI>");
    writeTOCentry(f,k,memo);
    if( k->pubChilds.size() || k->proChilds.size() ){
	fprintf(f,"<UL>\n");
	int i ;
	for (i=0 ; i<k->pubChilds.size() ; i++){
	    writeHIERentry(f,k->pubChilds[i],memo);
	}
	for (i=0 ; i<k->proChilds.size() ; i++){
	    writeHIERentry(f,k->proChilds[i],memo);
	}
	fprintf(f,"</UL>\n");
    }
}


struct {
    int sec;
    const char *name;
} toc_sections[]={
    { MANUAL_SEC, "General"},
    { PACKAGE_SEC, "Packages"},
    { CLASS_SEC, "Classes"},
    { INTERFACE_SEC, "Interfaces"},
    { FUNCTION_SEC, "Functions, Macros"},
    { VARIABLE_SEC, "Variables"},
    { MACRO_SEC, "Macros"},
    { UNION_SEC, "Enums, Unions, Structs"},
    {0,0}
};

void writeTOCRec(SortedList& list,FILE *f,Entry *root,int section,int& first)
{    
    if (root->section == toc_sections[section].sec && root->name.length()){
	if (first){
	    fprintf(f,"<H2>%s</H2>\n",toc_sections[section].name);
	    fprintf(f,"<UL>\n");
	    first=0;
	}
	if (root->section==MANUAL_SEC || root->section==PACKAGE_SEC) {
	  fprintf(f,"<LI>\n");
	  writeTOCentry(f,root,1);
	} else 
	  writeTOCentry(list,root,1);
    }
    if ((root->section==MANUAL_SEC || root->section==PACKAGE_SEC)
	&& root->sublist.size()){

	int lastfirst=first;
	if (toc_sections[section].sec==MANUAL_SEC || 
	    toc_sections[section].sec==PACKAGE_SEC) {
	    if (!lastfirst)
		fprintf(f,"<UL>\n");
	}
	for (int i=0 ; i<root->sublist.size() ; i++){
	    writeTOCRec(list,f,root->sublist[i],section,first);
	}
	if (!lastfirst)
	    if (toc_sections[section].sec==MANUAL_SEC || 
		toc_sections[section].sec==PACKAGE_SEC)
		fprintf(f,"</UL>\n");
    }
}

void writeTOC(FILE *f)
{
    
    int *index=new int[root->sublist.size()+1];
    
    //fprintf(f,"<html><head><TITLE>Table of Contents</TITLE></head>\n");


    fprintf(f,"%s",(const char *) indexHeader);
    fprintf(f,"\n<H1>Table of contents</H1>\n");    
    int first=1;
    for (int k=0 ; toc_sections[k].name ; k++){
	first=1;
	SortedList list;
	writeTOCRec(list,f,root,k,first);
	list.sort();
	list.write(f);
	if (!first)
	    fprintf(f,"</UL>\n");
    }
    //fprintf(f,"<I><A HREF=\"HIER.%s\"> hierarchy of classes</A></I><P>",HTML_SUFFIX);
    fprintf(f,"%s",(const char *) indexFooter);
    copyright(f);
}


void writeHIERrec(FILE *f,Entry* root)
{
    for (int i=0 ; i<root->sublist.size() ; i++){
	if (root->sublist[i]->baseclasses.size()==0)
	    if ( root->sublist[i]->isClass() 
		&&  root->sublist[i]->proBaseclasses.size() == 0
		&&  root->sublist[i]->pubBaseclasses.size() == 0 ){
		writeHIERentry(f,root->sublist[i],0);
	    }
	writeHIERrec(f,root->sublist[i]);
  }
}

void writeHIER(FILE *f)
{
  //fprintf(f,"<html><head><TITLE>Hierarchy ClassDoc</TITLE></head>\n");
  fprintf(f,"%s",(const char *) hierHeader);
    
  fprintf(f,"<H1>Hierarchy of classes</H1>\n");
  fprintf(f,"<UL>\n");
  writeHIERrec(f,root);
  fprintf(f,"</UL>\n");
  //fprintf(f,"<I><A HREF=\"aindex.%s\"> alphabetic index</A></I><P>",
  //HTML_SUFFIX);

  fprintf(f,"%s",(const char *) hierFooter);
  
  copyright(f);
}

void writeHIERrecJava(FILE *f,Entry *root)
{
    for (int i=0 ; i<root->sublist.size() ; i++){
	if (root->sublist[i]->baseclasses.size()==0)
	    if (root->sublist[i]->isClass()
		&&  root->sublist[i]->proBaseclasses.size() == 0
		&&  root->sublist[i]->pubBaseclasses.size() == 0 ){
		ClassGraphWriter::writeJava(f,root->sublist[i],0);
	    }
	writeHIERrecJava(f,root->sublist[i]);
  }
}

void writeHIERjava(FILE *f)
{
    fprintf(f,"<html><head><TITLE>Hierarchy ClassDoc</TITLE></head>\n");
    fprintf(f,"<H1>Hierarchy of classes</H1>\n");
    fprintf(f,"<UL>\n");
    writeHIERrecJava(f,root);
    fprintf(f,"</UL>\n");
    fprintf(f,"<I><A HREF=\"aindex.%s\"> alphabetic index</A></I><P>",HTML_SUFFIX);
    copyright(f);
}


/** This class keeps track of overloading relationships. Insert mebers
  using addMember. It returns NULL in case this is a new Member, != 0 
  otherwise.*/

class MemberList {
    McHashTable<char *,Entry *> list;
public:
    MemberList() : list((Entry*) NULL) {}
    /** Add a new member. Returns NULL, if no compatible member 
      has yet occurred.*/
  Entry *addMember(Entry *e,Entry *father) {
      McString signature;
      signature=e->name;
      if (e->language==LANG_JAVA) {
	signature+=e->args;
      }
      char *tmp=strdup((const char *)signature);
	Entry **val=list.insert(tmp);
	if (*val==0) {
	    *val=father;
	    return(0);
	} else {
	    if (verb) printf("Member %s was there\n",tmp);
	    free (tmp);
	}
	return (*val);
    }
};

void showSubMembers(FILE *f,Entry *entry);
extern int withPrivate;
extern int showFilenames;

void showMembers( Entry *k,FILE *f,int links,MemberList *ignore=0)
{
    static struct {
	char *heading;
	int protection;
	int secMask;
    } sections[] = {{"<DT><h3>Public Classes</h3><DD>",PUBL,CLASS_SEC|UNION_SEC},
		    {"<DT><h3>Public Fields</h3><DD>",PUBL,VARIABLE_SEC},
		    {"<DT><h3>Public Methods</h3><DD>",PUBL,FUNCTION_SEC},
		    {"<DT><h3>Public</h3><DD>",PUBL,
		     ~(CLASS_SEC|VARIABLE_SEC| FUNCTION_SEC)},
		    {"<DT><h3>Protected Classes</h3><DD>",PROT,CLASS_SEC|UNION_SEC},
		    {"<DT><h3>Protected Fields</h3><DD>",PROT,VARIABLE_SEC},
		    {"<DT><h3>Protected Methods</h3><DD>",PROT,FUNCTION_SEC},
		    {"<DT><h3>Protected</h3><DD>",PROT,
		     ~(CLASS_SEC|VARIABLE_SEC| FUNCTION_SEC)},
		    {"<DT><h3>Private Fields</h3><DD>",PRIV,VARIABLE_SEC},
		    {"<DT><h3>Private Methods</h3><DD>",PRIV,FUNCTION_SEC},
		    {"<DT><h3>Private</h3><DD>",PRIV,
		     ~(CLASS_SEC|VARIABLE_SEC| FUNCTION_SEC)},
		    {0,0,0}};

    int first;
    first=1;
    fprintf(f,"\n<DL>\n");
    Entry* tmp ;
    MemberWriter *memberWriter;
    if (withTables && links){
	memberWriter=new MemberWriterTable();
    } else {
	memberWriter=new MemberWriter();
    }
    for (int i=0 ; sections[i].heading ; i++){
	memberWriter->startList(f,sections[i].heading,links);
	for( tmp = k->sub ; tmp ; tmp = tmp->next ) {
	    if (tmp->protection==sections[i].protection && 
		(tmp->section&sections[i].secMask)){
		if (links || (strcmp(tmp->name,k->name)!=0
			      && (((const char *)tmp->name)[0]!='~' || 
				  strcmp(&((const char *)tmp->name)[1],k->name)!=0))){
		  int ignoreThisOne = 0;		  
		  if (ignore) {
		    Entry* othersFather=ignore->addMember(tmp,k);
		    if (othersFather!=0 && othersFather != k)
		      ignoreThisOne=1;
		  }
		  if (!ignoreThisOne || links)
		    memberWriter->addMember(tmp,links);
		}
	    }
	}
	memberWriter->endList();
    }

    /*
    memberWriter->startList(f,"<DT><h3>public members:</h3><DD>",links);
    for( tmp = k->sub ; tmp ; tmp = tmp->next ) {
	if (tmp->protection==PUBL){
	    if (links || (strcmp(tmp->name,k->name)!=0
			  && (((const char *)tmp->name)[0]!='~' || 
			      strcmp(&((const char *)tmp->name)[1],k->name)!=0))){
		if ((ignore && ignore->addMember(tmp)==0) || ignore==0)
		    memberWriter->addMember(tmp,links);
	    }
	}
    }
    memberWriter->endList();
    
    memberWriter->startList(f,"<DT><h3>protected members:</h3><DD>",links);
    for( tmp = k->sub ; tmp ; tmp = tmp->next ) {
	if (links || (strcmp(tmp->name,k->name)!=0
		      && (((const char *)tmp->name)[0]!='~' || 
			  strcmp(&((const char *)tmp->name)[1],k->name)!=0))){
	    if (tmp->protection==PROT){
		if ((ignore && ignore->addMember(tmp)==0) || ignore==0)
		    memberWriter->addMember(tmp,links);
	    }
	}
    }
    memberWriter->endList();
    
    memberWriter->startList(f,"<DT><h3>private members:</h3><DD>",links);
    if (withPrivate && links){
	if (!first)  fprintf(f,"</DL>\n");  first=1;
	for( tmp = k->sub ; tmp ; tmp = tmp->next ){
	    if (tmp->protection==PRIV){
		if ((ignore && ignore->addMember(tmp)==0) || ignore==0)
		    memberWriter->addMember(tmp,links);
	    }
	}
    }
    memberWriter->endList();*/ 
    delete memberWriter;
    fprintf(f,"</DL>\n"); 
}

void writeInherited(struct Entry *k,FILE *f,MemberList *list=0)
{
  int i;
  showMembers(k,f,0,list);
  for (i=0 ; i<k->baseclasses.size() ; i++){
    fprintf(f,"<hr><H3>Inherited from <A HREF = \"%s\"> %s:</A></h3>\n",
	    (const char *)((k->baseclasses)[i]->fileName),
	    (const char *)((k->baseclasses)[i]->hname));
    writeInherited((k->baseclasses)[i],f,list);
  }
}

/* This function writes the @-fields (except @memo, @name) of the
   specified entry */
void writeTags(FILE *f,Entry *entry)
{
    fprintf( f, "<DL>");    
    
    if (entry->exception.size()) {
	fprintf(f,"<DT><B>Throws:</B><DD>");
	for (int i=0 ; i<entry->exception.size() ; i++) {
	    int k=0;
	    McString s;
	    while (k<entry->exception[i]->length() && (myisalnum((*entry->exception[i])[k]) 
						       || (*entry->exception[i])[k]=='_'))
		s+=(*entry->exception[i])[k++];
	    fprintf(f,"<B>%s</B> ",strToHtml((const char *)s,0,entry,1));
	    while (k<entry->exception[i]->length())
		fprintf(f,"%c",(*entry->exception[i])[k++]);
	    fprintf(f,"<br>");
	}
    }
    if (entry->retrn.length()) {
	fprintf(f,"<DT><B>Returns:</B><DD>%s\n", (const char *)entry->retrn);
    }
    if (entry->param.size()) {
	fprintf(f,"<DT><B>Parameters:</B><DD>");
	for (int k=0 ; k<entry->param.size() ; k++) {
	  
	    int i=0;
	    fprintf(f,"<B>");
	    while (i<entry->param[k]->length() && 
		   (myisalnum((*entry->param[k])[i]) || 
		    (*entry->param[k])[i]=='_'))
		fprintf(f,"%c",(*entry->param[k])[i++]);
	    fprintf(f,"</B> - ");

	    while (i<entry->param[k]->length())
		fprintf(f,"%c",(*entry->param[k])[i++]);
	    fprintf(f,"<br>");
	}
    }
    if (entry->author.length()) {
	fprintf(f,"<DT><B>Author:</B><DD>%s\n", (const char *)entry->author);
    }
    if (entry->version.length()) {
	fprintf(f,"<DT><B>Version:</B><DD>%s\n", (const char *)entry->version);
    }
    if (entry->see.size()) {
	fprintf(f,"<DT><B>See Also:</B><DD>");
	for (int k=0 ; k<entry->see.size() ; k++) {
	    if (entry->see[k]->length())
		fprintf(f,"%s<br>",seeToHtml(*entry->see[k],entry));
	}
    }
    
    fprintf( f, "</DL><P>");    
}

void writeDoc(FILE *f,Entry *entry)
{  
    if (entry->ownPage)
	return;
    int toBeDocumented = subEntryIsToBeDocumented(entry);
    
    if (toBeDocumented) {
	char *args=(entry->hargs);
	char *type=(entry->htype);
	printRefLabel(f,entry ) ;
	fprintf( f, "<DT>");
	fprintf( f, "<IMG BORDER=0 SRC=icon2.gif><TT><B> %s %s",
		 type,
		 (char *)(entry->hname));
	fprintf( f, "%s", args);	
	fprintf( f, "</B></TT>\n");

	if (entry->doc.length()>0)
	    fprintf( f, "<DD>%s\n",(char *)(entry->hdoc));
	else if (entry->memo.length()>0)
	    fprintf( f, "<DD>%s\n",(char *)(entry->hmemo));

	writeTags(f,entry);
    }
    if (entry->sub){
	fprintf(f,"<DL>\n");
	for (Entry *tmp=entry->sub ; tmp ; tmp=tmp->next){
	    writeDoc(f,tmp);
	}
	fprintf(f,"</DL>\n");
    }
}


void writeHeader( Entry *e,FILE *f)
{
    fprintf(f,"<html><head><TITLE>%s</TITLE></head>\n",
	    (char *)(e->hname));
    
    fprintf(f,"%s",(const char *) pageHeader);    
    if (showFilenames && e->section!=PACKAGE_SEC && e->section!=MANUAL_SEC)
	fprintf(f,"In file %s:",(const char *) e->file);
    
    if (e->section == PACKAGE_SEC) {
	McString tmp;
	e->getPackage(tmp);
	fprintf(f,"<H2>package %s</H2>",(char *)tmp);
    } else {
	fprintf(f,"<H2><A HREF =\"#DOC.DOCU\" > <IMG BORDER=0 SRC=down.gif></A>");
	if (e->language==LANG_JAVA) {
	    McString tmp;
	    e->getPackage(tmp);
	    fprintf(f," %s %s.%s",e->htype,(const char *)tmp,e->hname);
        } else
	    fprintf(f," %s %s",e->htype,e->hname);

	fprintf(f," %s </H2>", (const char*)(e->hargs) );
    }
    if (e->memo.length()>1) {
	fprintf(f,"<BLOCKQUOTE>\n%s\n</BLOCKQUOTE>\n",
		(e->hmemo));
    }
}

void writeManPage( Entry *e,FILE *f)
{
  int i;
  Entry *c;
  MemberList list;
  writeHeader(e,f);  
  if (verb)
      printf ("Page for %s\n",(const char *)e->name);  
  if (e->isClass()){ // Is it really a class ?
      // the Inheritance:
      int numChilds = e->pubChilds.size()+e->proChilds.size()+
	  e->priChilds.size();
      int numParents=0;
	  if (e->language==LANG_JAVA)
		numParents = 	e->pubBaseclasses.size()+
			e->proBaseclasses.size() + e->otherPubBaseclasses.size()+
			e->otherProBaseclasses.size();
	  else
          numParents = 	e->pubBaseclasses.size()+e->priBaseclasses.size()+
			e->proBaseclasses.size() + e->otherPubBaseclasses.size()+
			e->otherPriBaseclasses.size()+  e->otherProBaseclasses.size();
	  
	  if (numParents || numChilds || trivialGraphs) {
	    	  fprintf(f,"<hr>\n <h2> Inheritance:</h2>\n");
		  ClassGraphWriter::write(f,e);
	    if (e->language==LANG_JAVA && e->implements.size()) {
		      ClassGraphWriter::writeImplements(f,e);
	    }
      }      
      // the members 
      if (e->sub){
	  fprintf(f,"<hr>\n");  
	  showMembers(e,f,1,&list);
      }
  } else { // This is not a class
      if (e->sub){
	  MemberWriter memberWriter;
	  fprintf(f,"\n<hr>\n");
	  memberWriter.startList(f," ",1);
	  for (Entry *tmp=e->sub; tmp ; tmp=tmp->next){
	      memberWriter.addMember(tmp,1);
	  }
	  memberWriter.endList();
      }
  }
  if (e->isClass()){ // Is it really a class ?
      if (showInherited)
	  for (i=0 ; i<e->baseclasses.size() ; i++){
	      fprintf(f,"<hr><H3>Inherited from <A HREF=\"%s\">"
		      "%s:</A></h3>\n",
		      (const char *)((e->baseclasses)[i]->fileName),
		      (const char *)((e->baseclasses)[i]->hname));
	      writeInherited((e->baseclasses)[i],f,&list);
	  }
  }
  // the documentation: 
  fprintf(f,"<A NAME=\"DOC.DOCU\">\n");
  fprintf(f,"<hr>\n <h2> Documentation </h2>\n");
  
  fprintf(f,"<BLOCKQUOTE>\n");
  if (e->doc.length()>1)
    fprintf(f,"%s\n\n", (e->hdoc));
  else if (e->memo.length()>1) 
    fprintf(f,"%s\n\n", (e->hmemo));  
  fprintf(f,"</BLOCKQUOTE>\n");

  //int doc_count=0;
  fprintf(f,"<DL>\n");
  {
    for (Entry *tmp=e->sub; tmp ; tmp=tmp->next){
      writeDoc(f,tmp);
    } 
  }
  fprintf(f,"</DL>\n");
  

  // the children:
  if (e->isClass()){ // Is it really a class ?
    if( e->pubChilds.size() || e->proChilds.size() )
    {
      fprintf(f,"<hr>\n<DL><DT><B>Direct child classes:\n</B><DD>");
      c=e;
      for ( i=0 ; i<c->pubChilds.size() ; i++){
	fprintf(f,"<A HREF=\"%s\"> %s </A><br>\n",(const char *)c->pubChilds[i]->fileName,
		(const char *)c->pubChilds[i]->hname);
      }
      for ( i=0 ; i<c->proChilds.size() ; i++){
	fprintf(f,"<A HREF=\"%s\"> %s </A><br>\n",(const char *)c->proChilds[i]->fileName,
		(const char *)c->proChilds[i]->hname);
      }
      fprintf(f,"</DL>\n");
    } else
      fprintf(f,"<hr>\n <DL><DT><B>This class has no child classes."
	      "</B></DL>\n");
  }
  
  writeTags(f,e);
  /*
  fprintf(f,"<DL>\n");
  if (e->author.length()) {
    fprintf(f,"<DT><B>Author:</B><DD>%s\n", (const char *)e->author);
  }
  if (e->version.length()) {
    fprintf(f,"<DT><B>Version:</B><DD>%s\n", (const char *)e->version);
  }
  fprintf(f,"</DL>\n");
  
  if (e->see.size()) {
	fprintf(f,"\n<DL><DT><B>See Also:</B>\n<DD>");
	for (int k=0 ; k<e->see.size() ; k++) {
	    if (e->see[k]->length())
		fprintf(f,"%s<br>\n",seeToHtml(*e->see[k],e));
	}
	fprintf(f,"</DD></DL>\n");
  }*/

    fprintf(f,"%s",(const char *) pageFooter);

  copyright(f);
}



FILE *myOpen(const char *dir,const char *name)
{
  char buf[200];
  sprintf(buf,"%s%c%s",dir,PATH_DELIMITER,name);
  FILE *f=fopen(buf,"wb");
  if (!f){
    printf("Cannot open file %s\n",buf);
    exit(-1);
  }
  return f;
}

void makeHtmlNames(Entry* entry)
{
    //  static int cnt;
    Entry*  tmp ;  
    entry->hname=strToHtml(entry->name, 0, entry,0);
    if (entry->section==EMPTY_SEC)
	entry->section=PACKAGE_SEC;
    //printf("Name for %s is %s\n",(const char *)entry->name,(const char *)entry->hname);
    if (entry->sub){
	for( tmp = entry->sub ; tmp ; tmp = tmp->next )
	    makeHtmlNames(tmp);
    }
}

void makeFileNames(Entry* entry)
{
    Entry*  tmp ;  
    
    if (entry->ownPage){
	McString file;
	if (entry->language==LANG_JAVA) {
	    McString pack;
	    if (entry->section==PACKAGE_SEC)
		file = "package-";
	    entry->getPackage(pack);
	    if (pack.length()) {
		file+=pack;
		if (entry->section!=PACKAGE_SEC)
		    file+='.'; 
	    }
	}
	if (entry->language!=LANG_JAVA || entry->section!=PACKAGE_SEC)
	    file+=entry->name;
	entry->fileName=makeFileName(file);
    }
    
    if (entry->sub){
	for( tmp = entry->sub ; tmp ; tmp = tmp->next )
	    makeFileNames(tmp);
    }
}

void decideAboutOwnPages(Entry* entry)
{
    Entry*  tmp ;  

    if (entry->section == EMPTY_SEC)
	entry->section = MANUAL_SEC;

    if (entry->sub || entry->doc.length()>1 || entry->isClass())
	entry->ownPage=1;
    
    if (entry->sub && (entry->section==MANUAL_SEC || entry->section==PACKAGE_SEC)){
	for( tmp = entry->sub ; tmp ; tmp = tmp->next )
	    decideAboutOwnPages(tmp);
    }
}

void makeHtml(Entry* entry)
{
    //static int cnt;
    Entry*  tmp ;
    int i;
    entry->hmemo=strToHtml(entry->memo, 0, entry,0);
    entry->hdoc =strToHtml(entry->doc, 0, entry,0);
    
    entry->hargs =strToHtml(entry->args, 0, entry ,1);
    entry->htype =strToHtml(entry->type, 0, entry ,1);
    
    entry->author =strToHtml(entry->author, 0, entry ,0);
    entry->version =strToHtml(entry->version, 0, entry ,0);
    entry->retrn =strToHtml(entry->retrn, 0, entry ,0);
    for (i=0 ; i<entry->param.size() ; i++)
	*entry->param[i] = strToHtml(*entry->param[i], 0, entry ,0);

    for (i=0 ; i<entry->exception.size() ; i++)
	*entry->exception[i] = strToHtml(*entry->exception[i], 0, entry ,0);

    if (entry->sub){
	for( tmp = entry->sub ; tmp ; tmp = tmp->next )
	    makeHtml(tmp);
    }
}

void writePageSub(FILE *f,Entry *tmp)
{
    if (withTables){
	  fprintf(f,"<TR>");
	  fprintf(f,"<TD VALIGN=top>");
    } else {
	fprintf( f, "<DT>\n");
    }

    McString htype,hargs;

    if (tmp->fileName.length()){
      // no type, args      
    } else {
	htype = tmp->htype;
	hargs = tmp->hargs;
    }
    
    fprintf( f, "<IMG BORDER=0 SRC=icon1.gif>");

    if (1 || tmp->section!=MANUAL_SEC){
	if (withTables)
	    fprintf(f,"%s </TD><TD>",(const char *)htype);
	else 
	    fprintf(f,"%s ",(const char *)htype);
    }
	
    if (tmp->fileName.length()){	
	fprintf( f, "<A HREF=%s><B>%s</B></A> ",
		 (const char *)tmp->fileName,
		 (const char *)tmp->hname) ;
    } else {
	fprintf( f, "<B>%s</B>",
		 (const char *)tmp->hname) ;
    }
    if (withTables && (tmp->section==MANUAL_SEC &&0))
	fprintf(f,"</TD><TD>");
    
    if (1 || tmp->section!=MANUAL_SEC)
	fprintf( f, "%s", (const char *)hargs) ;

    if (withTables){
	fprintf(f,"<br>");
	if (tmp->hmemo)
	    fprintf(f,"\n<I>%s</I>\n",(const char *) tmp->hmemo);    
	fprintf(f,"</TD></TR>");
    } else {
	if (tmp->hmemo)
	    fprintf(f,"\n <DD><I>%s</I>\n",(const char *) tmp->hmemo);    
    }
}

void writeManPageRec(const char *dir, Entry *e)
{
    if (verb)
	printf("in writeManPageRec %s\n",(const char *)e->name);
    if (e->fileName.length()){
	char buf[80];
	sprintf(buf,"%s",(const char *)(e->fileName));
	FILE *f=myOpen(dir,buf);
	if (!f){
	    printf("Cannot open %s for writing !\n",buf);
	    return;
	}
	if (e->section!=MANUAL_SEC && e->section!=PACKAGE_SEC){
	    writeManPage(e,f);
	} else {
	    writeHeader(e,f);
	    if (e->sub){
		if (withTables)
		    fprintf(f,"\n<TABLE>\n");
		else
		    fprintf(f,"\n<hr>\n<DL>\n");
		for (Entry *tmp=e->sub; tmp ; tmp=tmp->next){
		    writePageSub(f,tmp);
		}
		if (withTables)
		    fprintf(f,"\n</TABLE>\n");
		else
		    fprintf(f,"</DL>\n");
	    }
	    
	    fprintf(f,"<A NAME=\"DOC.DOCU\">\n");
	    //fprintf(f,"<hr>\n <h2> Documentation </h2>\n");
  
	    if (e->doc.length()>1){
		fprintf(f,"<BLOCKQUOTE>\n%s\n</BLOCKQUOTE>\n",
			(e->hdoc));
	    } else if (e->memo.length()>1) {
		fprintf(f,"<BLOCKQUOTE>\n%s\n</BLOCKQUOTE>\n",
			(e->hmemo));
	    }

	    writeTags(f,e);

	    /*fprintf(f,"<DL>\n");
	  if (e->author.length()) {
	    fprintf(f,"<DT><B>Author:</B><DD>%s\n", (const char *)e->author);
	  }
	  if (e->version.length()) {
	    fprintf(f,"<DT><B>Version:</B><DD>%s\n", (const char *)e->version);
	  }

	  fprintf(f,"</DL>\n");
	    if (e->see.size()) {
	      fprintf(f,"\n<DL><DT><B>See Also:</B>\n<DD>");
	      for (int k=0 ; k<e->see.size() ; k++) {
		if (e->see[k]->length())
		  fprintf(f,"%s<br>\n",seeToHtml(*e->see[k],e));
	      }
	      fprintf(f,"</DD></DL>\n");
	    }*/
	    fprintf(f,"%s",(const char *) pageFooter);

	    copyright(f);
	}
	fclose(f);
    }
    if ((e->section==MANUAL_SEC || e->section==PACKAGE_SEC)
	&& e->sub){
	for (Entry *tmp=e->sub; tmp ; tmp=tmp->next){
	    if (verb)
		printf("doing sub %s\n",(const char *)tmp->name);
	    writeManPageRec(dir,tmp);
	}
    }
}


void dumpFile(const char *dir,const char *name,const unsigned char *data,int size)
{
  FILE *f=myOpen(dir,name);
  fwrite(data,1,size,f);
  fclose(f);
}

void readTemplates()
{
  FILE *f;
  int c;
  if (f=fopen("indexHeader.inc","r")) {
    indexHeader=" ";
    while ((c=fgetc(f)) != EOF) indexHeader+=c;
    fclose(f);
  }
  if (f=fopen("indexFooter.inc","r")) {
    indexFooter=" ";
    while ((c=fgetc(f)) != EOF) indexFooter+=c;
    fclose(f);
  }
  if (f=fopen("hierHeader.inc","r")) {
    hierHeader=" ";
    while ((c=fgetc(f)) != EOF) hierHeader+=c;
    fclose(f);
  }
  if (f=fopen("hierFooter.inc","r")) {
    hierFooter=" ";
    while ((c=fgetc(f)) != EOF) hierFooter+=c;
    fclose(f);
  }
  if (f=fopen("classHeader.inc","r")) {
    pageHeader=" ";
    while ((c=fgetc(f)) != EOF) pageHeader+=c;
    fclose(f);
  }
  if (f=fopen("classFooter.inc","r")) {
    pageFooter=" ";
    while ((c=fgetc(f)) != EOF) pageFooter+=c;
    fclose(f);
  }

}

void doHTML(char *dir,Entry*)
{
    FILE *f;
    Entry *tmp;

    if( makedir( dir,0755) != 0 ) {
	if( errno == EEXIST ) {
	    FILE *exist = fopen(dir,"a");
	    if( exist ) {
		printf("file exists %s\n", dir) ;
		fclose( exist ) ;
		//exit( -1 ) ;
	    }
	} else {
	    printf("Could not create directory %s\n",dir); 
	    //exit(-1) ;
	}
    }
    
    if (shortFilenames){
	HTML_SUFFIX=strdup("htm");
    }

    printf("converting DOC++ to HTML ...");
    
    decideAboutOwnPages(root);
    makeFileNames(root);
    
    if (root->name.length() && root->section == MANUAL_SEC){
	root->fileName="index.";
	root->fileName+=HTML_SUFFIX;
    } else if (root->sublist.size()==1
	       && root->sublist[0]->section == MANUAL_SEC){
	root->sublist[0]->fileName="index.";
	root->sublist[0]->fileName+=HTML_SUFFIX;
    }
    
    if (root->section==EMPTY_SEC)
	root->section=MANUAL_SEC;
    for( tmp = root ; tmp ; tmp = tmp->next )
	makeHtmlNames(tmp);
    for( tmp = root ; tmp ; tmp = tmp->next )
	makeHtml(tmp);
    
  /*  for( tmp = root ; tmp ; tmp = tmp->next )
      makeLinks(tmp);*/
    printf(" done\n");
    
    readTemplates();
    printf("writing files.\n");
    dumpFile(dir,"logo.gif",logo,sizeof(logo));
    dumpFile(dir,"icon1.gif",blueBall,sizeof(blueBall));
    dumpFile(dir,"icon2.gif",greyBall,sizeof(greyBall));
    dumpFile(dir,"down.gif",down,sizeof(down));
    
    if (ownBanner){
	FILE *in=fopen(ownBanner,"r");
	if (!in && strcmp("none",ownBanner) && strlen(ownBanner)>1){
	    printf("Warning: Can't open %s, producing no banner.\n",
		    ownBanner);
	} else {	  
	    int	c ;
	    while ( (c = fgetc(in)) != EOF )
		banner += (char) c ;
	    fclose(in);
	}
    }
  
    if (javaGraphs){
	dumpFile(dir,"ClassGraph.class",ClassGraph_class,sizeof(ClassGraph_class));
	dumpFile(dir,"ClassGraphPanel.class",ClassGraphPanel_class,sizeof(ClassGraphPanel_class));
	dumpFile(dir,"ClassLayout.class",ClassLayout_class,sizeof(ClassLayout_class));
	dumpFile(dir,"NavigatorButton.class",NavigatorButton_class,sizeof(NavigatorButton_class));    
  }


  char buf[40];

  {
      sprintf(buf,"index.%s",HTML_SUFFIX);
      f=myOpen(dir,buf);
      writeTOC(f);
      fclose(f);      
  }

  sprintf(buf,"HIER.%s",HTML_SUFFIX);
  f=myOpen(dir,buf);
  writeHIER(f);
  fclose(f);  

  sprintf(buf,"HIERjava.%s",HTML_SUFFIX);
  f=myOpen(dir,buf);
  writeHIERjava(f);
  fclose(f);  

  sprintf(buf,"aindex.%s",HTML_SUFFIX);
  f=myOpen(dir,buf);
  writeTOC(f);
  fclose(f);
  writeManPageRec(dir,root);
}      
      
static void	dumpEntry (FILE* out, Entry* entry,int)
{
  Entry*	tmp ;

  if (entry->sub){
    for( tmp = entry->sub ; tmp ; tmp = tmp->next )
      dumpEntry(out,tmp,0);
  }
}

void	usermanHTML(char*, Entry* root)
{
    if( root->name.length() )
	dumpEntry (out, root,1) ;
    else
	for( root=root->sub ; root ; root=root->next )
	    dumpEntry (out, root,1) ;
}

