#ifndef _MC_STRING_H
#define _MC_STRING_H

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "McDArray.h"
#include "McHandable.h"
#include "McWildMatch.h"

/// Character Strings
class McString : public McDArray<char>, public McHandable
{
    void	remove0()	{ resize( size() - 1 ) ; }
public:
    /// clear string
    void	clear()	
    { 
	McDArray<char>::clear() ;
	append( char(0) ) ;
    }

    /// initialize string with #str#
    McString&	operator=( const char* str )
    {
	McDArray<char>::clear() ;
	append( strlen(str) + 1, str ) ;
	return *this ;
    }

    /// append #str# to string
    McString&	operator+=( const char* str )
    {
	remove0() ;
	append( strlen(str)+1, str ) ;
	return *this ;
    }
    /// append #ch# to string
    McString&	operator+=( char ch )
    {
	(*this)[length()] = ch ;
	append( char(0) ) ;
	return *this ;
    }

    ///
    int operator==(const char *other) const
    {
	return(strcmp(*this,other)==0);
    }
    ///
    int operator==(const McString &other) const
    {
	return(*this==(const char *)other);
    }
    ///
    int operator<(const char *other) const
    {
	return(strcmp(*this,other)<0);
    }
    ///
    int operator<=(const char *other) const
    {
	return(strcmp(*this,other)<=0);
    }
    ///
    int operator>(const char *other) const
    {
	return(strcmp(*this,other)>0);
    }
    ///
    int operator>=(const char *other) const
    {
	return(strcmp(*this,other)>=0);
    }
    /// find a substring, returns index or -1 if not found
    int index(const char *needle) const
    {
	return(index(needle,0));
    }
    /// Return last char in string. Not the trailing zero.
    char &last() {
	assert(length()>0);
	return(data[length()-1]);
    }
    /// Return last char in string. Not the trailing zero.
    const char &last() const{
	assert(length()>0);
	return(data[length()-1]);
    }
    
    /// Insert string #addBefore#-the element.
    int insert(int addBefore, char *str) {
	assert(str);
	return McDArray<char>::insert(addBefore,strlen(str),str);
    }

    /*/ Find element t, starting at index. The index of #t# is
      returned, or -1 if t was not found.*/
    index (const char t,int start=0) const {
	if (length()<1)
	    return(-1);
	const char *end=&last();
	for (const char *i=data+start ; i<=end ; i++) {
	    if (*i==t){
		return (i-data);
	    }
	}
	return(-1);
    }

    /*/ Find element t, starting at #start#, searching {\bf
      backward}. If start is -1, search is started at the end. The
      index of #t# is returned, or -1 if t was not found.*/
    rindex(const char t,int start=-1) const {
	if (length()<1)
	    return(-1);
	const char *i;
	if (start>=0){
	    i=data+start;
	} else {
	    i=&last();
	}
	    
	for ( ; i>=data ; i--) {
	    if (*i==t){
		return (i-data);
	    }
	}	
	return(-1);
    }

    /// find a substring, start searching at ndx, return index or -1 if not found
    int index(const char *needle,int indx) const
    {
	const char *tmp=strstr(&(*this)[indx],needle);
	if (tmp==NULL) return(-1);
	return(tmp-&(*this)[0]);
    }

    /// number of characters in string
    int		length() const			{ return size() - 1 ; }

    ///
    McString() : McDArray<char>(0,16,(float)1.5)	{ append(char(0)) ; }

    ///
    McString( const McString& rhs )
	: McDArray<char>( rhs )
    { }

    ///
    McString( const char* str )
	: McDArray<char>( 0, 16, (float) 1.5 )
    { if (str) append( strlen(str)+1, str ) ; }

    /// Copy a part from another string.
    McString( const char* str , int start,int len)
	: McDArray<char>( 0, len+1 )
    { append( len, &str[start] ) ; append(char(0)); }
  
    /// 
    McString& operator+( const char *str2) const
    {
	McString *str = new McString(*this);
	*str+=str2;
	return(*str);
    }
    ///
    friend McString& operator+( const char* str1,const McString& str2 )
    {
	McString *str = new McString(str1);
	*str+=str2;
	return(*str);
    }

    /// Matches string against pattern conatining wildcards.
    int matches(const char* pattern) const {
	return mcWildMatch((const char*)(*this), pattern);
    }

};

#endif	
