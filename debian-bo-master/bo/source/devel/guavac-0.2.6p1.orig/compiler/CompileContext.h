// Copyright (c) 1996  David Engberg  All rights reserved
// $Id: CompileContext.h,v 1.2 1996/07/12 20:43:40 geppetto Exp $
#ifndef _CompileContext_h
#define _CompileContext_h
#pragma interface

#include "JavaTypeSignature.h"
#include "Bitset.h"
#include <deque>
class CCompiler;
class CJavaClassFile;
class CJavaMethodInfo;

//
//  Class name : CCompileContext
//  Description : This is a big catch-all class that encapsulates the inherited
//    attributes used during compilation.  For example, code generation
//    occassionally needs to know what class the code is being compiled in,
//    so the compile context provides that information.  Conceptually, this
//    is information that primarily flows top-down.
//
class CCompileContext {
public:
  CCompileContext(CCompiler* compiler, CJavaClassFile* inClass,
		  CJavaMethodInfo* inMethod, unsigned long localVariablesUsed);
  CCompileContext(const CCompileContext& source);
  ~CCompileContext();
  CCompileContext& operator=(const CCompileContext& source);
  
  void Merge(const CCompileContext& other);

  CCompiler& GetCompiler() { return *fCompilerAlias; }
  const CCompiler& GetCompiler() const { return *fCompilerAlias; }

  CJavaClassFile& GetClass() { return *fClassAlias; }
  const CJavaClassFile& GetClass() const { return *fClassAlias; }

  CJavaMethodInfo& GetMethod() { return *fMethodAlias; }
  const CJavaMethodInfo& GetMethod() const { return *fMethodAlias; }

  bool IsVariableInitialized(unsigned long index) const;
  void InitializeVariable(unsigned long index);

  bool IsReachable() const { return fReachable; }
  void SetUnreachable() { fReachable = false; }
  void SetReachable() { fReachable = true; }

  bool Throwable(const CJavaTypeSignature& type) const;
  void PushThrowable(const CJavaTypeSignature& type);
  void PopThrowable();

private:
  CCompiler* fCompilerAlias;
  CJavaClassFile* fClassAlias;
  CJavaMethodInfo* fMethodAlias;
  CBitset fLocalVariablesInitialized;
  bool fReachable;
  deque<CJavaTypeSignature> fThrowable;
};

#endif
