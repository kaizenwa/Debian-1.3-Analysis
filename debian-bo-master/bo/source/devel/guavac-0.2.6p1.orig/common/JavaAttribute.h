// Copyright (c) 1996  David Engberg  All rights reserved
// $Id: JavaAttribute.h,v 1.3 1996/01/08 03:03:38 geppetto Exp $
#pragma interface
#ifndef _JavaAttribute_h
#define _JavaAttribute_h

#include "dynamic_cast.h"
#include "JavaClassFile.h"
class ostream;

//
//  Class name : CJavaAttribute
//  Description : The java attribute class is used to represent an 'extension'
//    onto one of the parts of the Java class file.  Since these are handled
//    pseudo-polymorphically, I mirror that structure here by making a
//    virtual base class that you must DYNAMIC_CAST down to its children
//    to use them.
//
class CJavaAttribute {
  DynamicCastDeclarations;
public:
  static CJavaAttribute* ParseBuffer(string::const_iterator& javaBuffer,
				     const CJavaClassFile& classFile);
  virtual ~CJavaAttribute();
protected:
  CJavaAttribute();
};


//
//  Class name : CJavaConstantValueAttribute
//  Description : This is the attribute used in fields on a class when that
//    field is actually a static constant value.
//
class CJavaConstantValueAttribute : public CJavaAttribute {
  DynamicCastDeclarations;
public:
  CJavaConstantValueAttribute(JavaConstantIndex constantValueIndex);
  CJavaConstantValueAttribute(const CJavaConstantValueAttribute& source);
  virtual ~CJavaConstantValueAttribute();
  
  JavaConstantIndex GetConstantValue() const { return fValueIndex; }
private:
  JavaConstantIndex fValueIndex;
};

//
//  Class name : CJavaSourceFileAttribute
//  Description : This is the attribute used to encapsulate the index of the
//    string which is the name of the source file from which a class was
//    compiled.
//
class CJavaSourceFileAttribute : public CJavaAttribute {
  DynamicCastDeclarations;
public:
  CJavaSourceFileAttribute(const unicode_string& fileName);
  virtual ~CJavaSourceFileAttribute();
  
  unicode_string GetFileName() const { return fFileName; }
private:
  unicode_string fFileName;
};


#endif
