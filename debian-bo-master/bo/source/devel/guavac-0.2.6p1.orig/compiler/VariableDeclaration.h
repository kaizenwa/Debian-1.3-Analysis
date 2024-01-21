// Copyright (c) 1995  David Engberg  All rights reserved
// $Id: VariableDeclaration.h,v 1.5 1996/04/25 21:10:43 geppetto Exp $
#ifndef _VariableDeclaration_h
#define _VariableDeclaration_h

#include "unicode_string.h"
#include "JavaFieldSignature.h"
class CExpression;
class CCodeSequence;
class CCompileError;
class CJavaMethodInfo;
class CJavaClassFile;
class CCompiler;
class CJavaAccessFlags;
class CCompileContext;

//
//  Class name : CVariableDeclaration
//  Description : This class is used as the intermediate representation of
//    a field or variable declaration in Java source, including the
//    initializer.  This value is either a straightforward expression or
//    a CArrayInitializer, which allows shorthand initialization of arrays.
//
class CVariableDeclaration {
public:
  CVariableDeclaration(const CJavaTypeSignature& type,
		       const unicode_string& name,
		       CExpression* adoptInitializer = 0);
  ~CVariableDeclaration();

  CJavaTypeSignature GetType() const;
  void SetType(const CJavaTypeSignature& type);
  const CJavaFieldSignature& GetSignature() const { return fSignature; }
  virtual CCompileError* GenerateCode(CCodeSequence& code,
		 CCompileContext& context, const CJavaAccessFlags& modifiers,
	         unsigned short& stackUsed);
  CCompileError* GetConstantValue(CExpression*& intoPointer,
				  CCompileContext& context);
private:
  CJavaFieldSignature fSignature;
  CExpression* fInitializer;
};

#endif
