// Copyright (c) 1996  David Engberg  All rights reserved
// $Id: IntermediateClass.h,v 1.5 1996/05/26 00:17:21 geppetto Exp $
#ifndef _IntermediateClass_h
#define _IntermediateClass_h
#pragma interface

#include <deque>
#include "unicode_string.h"
#include "JavaAccessFlags.h"
#include "JavaClassFile.h"
#include "parser_decls.h"
class CCompiler;
class CIntermediateFunction;
class CCompoundStatement;

//
//  Class name : CIntermediateClass
//  Description : This represents a Java class during the process of
//    compilation.  It's not intended as a usable end-value, but rather
//    as an intermediate phase, tightly bound to the mechanics of compilation.
//    Therefore, you can probably ignore this class unless you're playing with
//    compilation, which is why this class is 'friend CCompiler'
//    For all intents and purposes, this is basically a protected struct.
//
class CIntermediateClass {
  friend class CCompiler;
public:
  unicode_string GetName() const { return fName; }
  unicode_string GetShortName() const;
protected:
  CIntermediateClass(const string& sourceFileName,
		     const unicode_string& className,
		     CJavaAccessFlags* adoptModifiers,
		     unicode_string* adoptExtends,
		     deque<unicode_string>* adoptInterfaces);
  ~CIntermediateClass();
  
private:
  string fSourceFileName;
  unicode_string fName;
  CJavaAccessFlags fAccessFlags;
  unicode_string* fExtends;
  deque<unicode_string> fInterfaces;
  deque<CIntermediateFunction*> fFunctions;
  unsigned short fStaticLocalVariables;
  CCompoundStatement* fStaticInitializer;
  StatementList fStaticDeclarations;
  StatementList fNonStaticDeclarations;
  CJavaClassFile* fRealClass;
};

#endif
