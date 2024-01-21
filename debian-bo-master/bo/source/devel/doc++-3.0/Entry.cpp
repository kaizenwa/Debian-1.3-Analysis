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

#include "doc.h"

#define max(a,b) ((a)>(b) ? (a) : (b))

Entry::Entry()
{
    static int n=1;
    number        = n++;
    subnumber     = 0 ;
    
    if (java)
	language = LANG_JAVA;
    else 
	language = LANG_CXX;

    protection    = PUBL ;
    section       = EMPTY_SEC ;
    file="No file";
    parent        = 0 ;
    next          = 0 ;
    sub           = 0 ;
    tex           = 0 ;
    done          = 0 ;
    ownPage       = 0 ;
    override      = 0 ;
}

Entry *findJavaClass(McString n,Entry *root)
{
    int i=n.index('.');
    if (i>-1) {
	McString package(n,0,i);
	if (root->section == PACKAGE_SEC && package==root->name) {
	    n.remove(0,i+1);
	    return(findJavaClass(n,root));
	}
	for (Entry *tmp=root->sub ; tmp ; tmp=tmp->next){
	    if (tmp->section == PACKAGE_SEC && package==tmp->name) {
		n.remove(0,i+1);
		return(findJavaClass(n,tmp));
	    }
	}
    } else {
	for (Entry *tmp=root->sub ; tmp ; tmp=tmp->next){
	    if (tmp->section == CLASS_SEC || tmp->section == INTERFACE_SEC)
		if (n==tmp->name)
		    return(tmp);
	}
    }
    return(0);
}

void	Entry::addSubEntry (Entry* current)
{
    int	i = 1 ;
    Entry **e = &sub ;
    sublist.append(current);
    for( ; *e ; e = &((*e)->next), ++i )
	;
    *e = current ;
    current->parent    = this ;
    current->next      = 0 ;
    current->subnumber = i ;
}

Entry*	Entry::newSubEntry()
{
    Entry*	newentry = new Entry ;
    addSubEntry( newentry ) ;
    return newentry ;
}

void	Entry::dumpNumber( McString& s )
{
    if( parent )
    {
	parent->dumpNumber(s) ;
	static char	buf[33] ;
	sprintf( buf, "%d", subnumber ) ;
	if( parent->parent )
	{
	    s += '.' ;
	    s += buf ;
	}
	else
	    s += buf ;
    }
}

void	Entry::dumpNumber( FILE* f )
{
  McString	num ;
  dumpNumber( num ) ;
  fprintf( f, "%s", (const char*)num ) ;
}

void	Entry::makeSubList()
{
    Entry*	tmp ;
    int		i = 0 ;

    sublist.clear() ;
    for( tmp = sub ; tmp ; tmp = tmp->next )
	++i ;

    if( i )
    {
	sublist.remax( i ) ;
	for( tmp = sub ; tmp ; tmp = tmp->next )
	{
	    sublist.append( (Entry*)0 ) ;
	    for( i = sublist.size()-1 ; i > 0 ; --i )
	    {
		if( sublist[i-1]->name <= (const char *) tmp->name )
		    break ;
		sublist[i] = sublist[i-1] ;
	    }
	    sublist[i] = tmp ;
	}
    }
}


void	makeSubLists( Entry* rt )
{
    rt->makeSubList() ;
    for( int i = rt->sublist.size() ; --i >= 0 ; )
	makeSubLists( rt->sublist[i] ) ;
}

int	isIt( const char* n, const Entry* m)
{
const char*	mn= m->name ;
/*
int ret =    (  m->section == CLASS_SEC 	\
	     || m->section == MANUAL_SEC	\
	     || m->section == UNION_SEC		\
	    ) && !strcmp(n, mn) ;
    return ret ;
*/
    return !strcmp(n, mn) ;
}

Entry*	Entry::findSub(const char *name,int von,int bis)
{
    if (bis-von<4)
    {
	for (int i=von+1 ; i<=bis-1 ; i++)
	{
	    if( isIt(name, sublist[i]) )
		return sublist[i];
	}
	return NULL;
    }

    int neu=(von+bis)/2;
    int result=strcmp(name,(sublist[neu]->name));

    if (result==0)
	return(sublist[neu]);
    if (result>0)
	return findSub(name,neu,bis);
    return findSub(name,von,neu);
}


Entry*	Entry::findSub(const char* name )
{
    if( sublist.size() ) {
	if( isIt(name, sublist[0]) )
	    return sublist[0];
	if( isIt(name, sublist[sublist.size()-1]) )
	    return sublist[sublist.size()-1];
	Entry*	found = findSub(name,0,sublist.size()-1) ;
	if( found )
	    return found ;
    }
    for( int i = sublist.size() ; i-- > 0 ; ) {
	if( sublist[i]->section == MANUAL_SEC ) {
	    Entry*	found = sublist[i]->findSub( name ) ;
	    if( found )
		return found ;
	}
    }
    return NULL;
}

void	Entry::addBaseClass(Entry* base,PROTECTION state)
{
    Entry *tmp2=this;
    if( state == PUBL ) {
	pubBaseclasses.append( base ) ;
	base->pubChilds.insert( 0,1,&tmp2 ) ;
    }
    else if (state == PROT) {
	proBaseclasses.append( base ) ;
	base->proChilds.insert( 0,1,&tmp2 ) ;
    }
    else {
	priBaseclasses.append( base ) ;
	base->priChilds.insert( 0,1,&tmp2 ) ;
    }
    baseclasses.append( base ) ;
}

void	Entry::addBaseClass(const char *classname,PROTECTION state)
{
    Entry*	tmp=0;    
    if (language==LANG_JAVA) {
	McString t(classname);
	makeFullName(t);
	tmp=findJavaClass(t,root);
    } else
	tmp = getRefEntry( classname, parent ) ;

    if (verb)
	printf("This= %s, base=%s\n",(const char*)name,classname);
    if( tmp  &&  (tmp->section == CLASS_SEC || tmp->section == INTERFACE_SEC)) {
	if (verb)printf("Known !\n");
	addBaseClass(tmp,state);
    } else {
      McString	*str = new McString( classname ) ;
      if( state == PUBL )
	otherPubBaseclasses.append( str ) ;
      else if ( state == PROT )
	otherProBaseclasses.append( str ) ;
      else 
	otherPriBaseclasses.append( str ) ;
    }
}



void Entry::getPackage(McString &n)
{
    Entry *tmp=this;
    n="";
    while (tmp) {
	if (tmp->section==PACKAGE_SEC) {
	    if (n.length())
		n.insert(0,".");
	    n.insert(0,tmp->name);
	}
	tmp=tmp->parent;
    }    
}


void Entry::makeFullName(McString &name)
{
    if (verb)
      printf("Make full name from %s\n",(const char *)name);

    /* If name is allready a fully qualified name, just return.*/
    if (name.index('.')>=0)
	return;
    
    /* Search in the list of import statements.*/
    for (int i=0 ; i<import.size() ; i++) {
	int ri=import[i]->rindex('.');
	if (ri>=0) {
	    if (strcmp((char *)name,(char *)(*(import[i]))+ri+1)==0) {
		name=*(import[i]);
		return;
	    }
	}
    }
    /* OK, assume that its the same package !*/
    McString tmp;
    getPackage(tmp);
    if (tmp.length())
	tmp+='.';
    name.insert(0,tmp);
    if (verb)
      printf("    -> %s\n",(const char *)name);
    
}

void Entry::makeRefs()
{
  int	i ;
  if (language==LANG_CXX) {
      if (section==CLASS_SEC)
	  findBases() ;
  } else if (language==LANG_JAVA ) {
      if (section==CLASS_SEC || section==INTERFACE_SEC) {
	  for ( i=0 ; i<implements.size() ; i++) {
	      McString tmp=*(implements[i]);
	      makeFullName(tmp);
	      addBaseClass((char *) tmp,PUBL);
	  }
	  for ( i=0 ; i<extends.size() ; i++) {
	      McString tmp=*(extends[i]);
	      makeFullName(tmp);	      
	      addBaseClass((char *) tmp,PUBL);
	  }	  
      }
  } else {
      fprintf(stderr,"Entry::makeRefs: Unknown language.\n");
  }
  for( i = 0 ; i < sublist.size() ; ++i )
      sublist[i]->makeRefs();
}

static	McString* get1RefName( McString& str, int start, int end )
{
  McString*	s = 0 ;

  while( (str[start] == ' ' || str[start] == '\t') && start < end )
    start++ ;
  while( (str[end] == ' ' || str[end] == '\t' || str[end] == ',')
	 && start < end )
    end-- ;
  
  if( start < end )
    {
      s = new McString(str, start, end ) ;
    }
  
  return s ;
}


int mergeCCFunctionInHeader(Entry *ccentry)
{
    if (ccentry->section!=FUNCTION_SEC) {
	if (verb)
	    printf("mergeCCFunctionInHeader: %s is no function\n",
		   (const char *)ccentry->name);
	return(0);
    }
    int skope=ccentry->name.rindex(':');
    skope--;
    if (skope<1 || ccentry->name[skope]!=':') {
	if (verb)
	    printf("No reason to merge function %s\n",(const char *)ccentry->name);
	return(0);
    }
    McString tmp(ccentry->name,0,skope);
    Entry *father=getRefEntry(tmp,ccentry->parent);
    
    if (!father) {
	if (verb)
	    printf("No class %s for %s found.\n",(const char *)tmp,
		   (const char *)ccentry->name);
	return(0);
    }
    ccentry->parent->removeSub(ccentry);
    father->addSubEntry(ccentry);
    return(1);
    //for (int i=0 ; i<father.sublist.size() ; i++) {
    //if (father.sublist[i]->section==FUNCTION_SEC && )
    //}
}
void mergeCCFunctions(Entry *root)
{
    for (int i=0 ; i<root->sublist.size() ; i++) {
	if (root->sublist[i]->section==FUNCTION_SEC) {
	    if (mergeCCFunctionInHeader(root->sublist[i])) {
		i--;
	    }
	} else if (root->sublist[i]->section==MANUAL_SEC ||
		   root->sublist[i]->section==PACKAGE_SEC)
	    mergeCCFunctions(root->sublist[i]);
    }
}

void Entry::removeSub(Entry *e)
{
    for (int i=0 ; i<sublist.size() ; i++)
	if (sublist[i]==e) {
	    sublist.remove(i);
	    break;
	}
    if (sub==e)
	sub=e->next;
    else {
	for (Entry *tmp=sub ; tmp->next; tmp=tmp->next)
	    if (tmp->next==e) {
		tmp->next=e->next;
		break;
	    }
    }
}

void getRefNames( McDArray<McString*> strings, const char* names )
{
  McString	str = names ;
    int		j ;
    int		i = 0 ;

    while( (j = str.index( ",", i) ) >= 0 )
    {
      McString*	s = get1RefName( str, i, j) ;
	if( s )
	    strings.append(s) ;
	i = j+1 ;
    }
    McString*	s = get1RefName( str, i, str.length() ) ;
    if( s )
	strings.append(s) ;
}

Entry*	getRefEntry( const char* name, Entry* rot )
{
    McString	tmp( name ) ;
    return getRefEntry( tmp, rot ) ;
}


/* We are looking for an entry named name. We start on level of entry.
   name can be a simple identifier or it can contain C++ Skopes (::).*/
Entry*	getRefEntry( McString& name, Entry* entry )
{    
    int i;
    Entry *rot=entry;
    if (entry->language==LANG_JAVA && name.index('.')>=0) {
	Entry *result = findJavaClass(name,root);
	//printf("returning: %p for %s\n",result,(const char *) name);
	return(result);
    }
    /* Suche in der darueberliegenden Ebene. Typischerweise
       in der Klasse, von der diese Fkt. member ist.*/
    /*if( entry != root )
	rot = entry->parent ;
    else
	rot=root;*/
    
    while (rot->section == MANUAL_SEC && rot->parent)
	rot=rot->parent;        
    
    int e = name.index("::");
    
    // Ein simpler Name:
    if (e<0) {
	Entry *tmp = rot->findSub(name);
	if (tmp) return(tmp);
	
	/* Sonst suchen wir in allen Vaterklassen.*/
	for( i = 0 ; i < rot->pubBaseclasses.size() ; i++ ) {
	    entry = getRefEntry( name, rot->pubBaseclasses[i]) ;
	    if( entry )
		return entry ;
	}

	for( i = 0 ; i < rot->proBaseclasses.size() ; i++ ) {
	    entry = getRefEntry( name, rot->proBaseclasses[i]) ;
	    if( entry )
		return entry ;
	}
	/* Letzte Chance, suche ein doc++-level hoeher: */
	if (rot->parent)
	    return (getRefEntry( name, rot->parent));
	return(0);
    } else {
	McString tmp((const char *)name);
	while ( (e=tmp.index("::"))>=0) {	    
	    if (e==0) {
		rot=root;	       
		tmp.remove(0,2);
	    } else {
		McString base(name,0, e);		
		tmp.remove(0,e+2);
		rot = getRefEntry(base,rot);
		if (!rot)
		    return(NULL);
	    }
	}
	return(getRefEntry(tmp,rot));
    }
    
    printf("BUG ! Still there !\n");
}

Entry*	getRefEntryOLD( McString& name, Entry* entry )
{
    int i;
    Entry *rot;

    // Just for safety.
    if( entry == 0 ) return 0 ;

    // This is the entry we are looking for.
    if (name == (const char *)(entry->name))
	return(entry);
    
    /* Suche in der darueberliegenden Ebene. Typischerweise
       in der Klasse, von der diese Fkt. member ist.*/
    if( entry != root )
	rot = entry->parent ;
    else
	rot=root;
    
    if (name == (const char *)(rot->name))
	return(rot);
    
    Entry*	rt = rot ;
    int		s, t, e ;

    s = 0 ;
    do
    {	
	e = name.index("::",s);
	// t ist der index des ersten :: (oder length(), falls kein :: da).
	t = (e >= 0) ? e : name.length() ;

	// There is a outerClass:: before the name
	if( t > s ) {
	    /// may still be false !!
	    
	    // Wir nehmen das, was vor dem :: steht, und kopieren das in base.
	    McString base(name,s, t-s) ;
	    
	    // Let's see whether we find the name before ::.
	    rt = rot->findSub(base) ;

	    // if not found on this level, look in the baseClasses:
	    if( rt == 0 ) {
		/* Wenn wir es nicht gefunden haben, und ganz oben sind,
		   dann eben nicht.*/
		if( rot == root )
		    return 0 ;

		Entry *entry = 0 ;
		/* Sonst suchen wir in allen Vaterklassen.*/
		for( i = 0 ; i < rot->pubBaseclasses.size() ; i++ ) {
		    entry = getRefEntry( name, rot->pubBaseclasses[i]) ;
		    if( entry )
			return entry ;
		}
		for( i = 0 ; i < rot->proBaseclasses.size() ; i++ ) {
		    entry = getRefEntry( name, rot->proBaseclasses[i]) ;
		    if( entry )
			return entry ;
		}
		// Not found in the baseClasses, look one doc++ level higher.
		return getRefEntry( name, rot ) ;
	    }
	    s = t+2 ;
	} else if ( e == 0 ) { // This is a global skope, like  ::name
	    s = 2 ;
	    rt = rot = root ;
	} else
	    return 0 ;
    } while( e > 0 ) ;

    if( rt )
	return rt ;

    if( rot == root )
	return 0 ;

    return getRefEntry( name, root ) ;
}
