// Copyright (c) 1996  David Engberg  All rights reserved
// $Id: CompileContext.C,v 1.3 1996/08/03 03:02:55 geppetto Exp $
#pragma implementation
#include "CompileContext.h"
#include "Compiler.h"
#include <cassert>

//
//  Method name : CCompileContext
//  Description : Constructs a compile context that gloms together a group
//    of different attributes, each of with is provided as an argument.
//    It initially assumes that all local variables have not been initialized
//    and the current entry point is reachable.
//    All of the provided pointer arguments are assumed to be valid aliases
//    to these elements.
//
CCompileContext::CCompileContext(CCompiler* compiler, CJavaClassFile* inClass,
		  CJavaMethodInfo* inMethod, unsigned long localVariablesUsed)
  : fCompilerAlias(compiler),
    fClassAlias(inClass),
    fMethodAlias(inMethod),
    fLocalVariablesInitialized(localVariablesUsed),
    fReachable(true)
{
  assert(compiler != 0 && inClass != 0 && inMethod != 0);
}

//
//  Method name : CCompileContext
//  Description : Copy constructor.
//
CCompileContext::CCompileContext(const CCompileContext& source)
  : fCompilerAlias(source.fCompilerAlias),
    fClassAlias(source.fClassAlias),
    fMethodAlias(source.fMethodAlias),
    fLocalVariablesInitialized(source.fLocalVariablesInitialized),
    fReachable(source.fReachable),
    fThrowable(source.fThrowable)
{
}

//
//  Method name : ~CCompileContext
//  Description : Destructor
//
CCompileContext::~CCompileContext()
{
}

//
//  Method name : operator=
//  Description : Assignment operator.
//
CCompileContext&
CCompileContext::operator=(const CCompileContext& source)
{
  if (&source != this) {
    fCompilerAlias = source.fCompilerAlias;
    fClassAlias = source.fClassAlias;
    fMethodAlias = source.fMethodAlias;
    fLocalVariablesInitialized = source.fLocalVariablesInitialized;
    fReachable = source.fReachable;
    fThrowable = source.fThrowable;
  }
  return *this;
}

//
//  Method name : IsVariableInitialized
//  Description : Returns true if the local variable in the current compile
//    context at the provided index has already been initialized.
//
bool
CCompileContext::IsVariableInitialized(unsigned long index) const
{
  return fLocalVariablesInitialized.GetElement(index);
}

//
//  Method name : InitializeVariable
//  Description : Indicates to the compile context that the provided local
//    variable slot has been assigned to, and therefore initialized.
//
void
CCompileContext::InitializeVariable(unsigned long index)
{
  fLocalVariablesInitialized.SetElement(index);
}

//
//  Method name : Merge
//  Description : This operation takes two compile contexts and merges them
//    at the join node of a control-flow graph.  This is the type of operation
//    that happens, for example, after the 'then' and 'else' branches of
//    an 'if' statement are calculated and the results need to be merged.
//    This operation assumes the two compile contexts come from the same
//    root and tries to reflect the state of the control flow regardless which
//    branch may be taken at run-time.
//    This modifies the current compile context to take on the merged result,
//    but leaves the provided argument untouched.
//
void
CCompileContext::Merge(const CCompileContext& other)
{
  assert(fCompilerAlias == other.fCompilerAlias &&
	 fClassAlias == other.fClassAlias &&
	 fMethodAlias == other.fMethodAlias);
  if (other.fReachable) {
    if (fReachable) {
      fLocalVariablesInitialized &= other.fLocalVariablesInitialized;
    } else {
      fLocalVariablesInitialized = other.fLocalVariablesInitialized;
    }
  }
  fReachable |= other.fReachable;
  // don't do anything with fThrowable
}

//
//  Method name : Throwable
//  Description : Returns true if the provided type signature can be legally
//    thrown in this compile context.  This means that it is either declared
//    in the 'throws' clause for this method, handled by a surrounding
//    try/catch clause, or descends from Error or RuntimeException.
//
bool
CCompileContext::Throwable(const CJavaTypeSignature& type) const
{
  static const CJavaTypeSignature errorType(CCompiler::kErrorName);
  static const CJavaTypeSignature
    runtimeExceptionType(CCompiler::kRuntimeExceptionName);
  if (fCompilerAlias->SameType(type, errorType) ||
      fCompilerAlias->AssignableSubtype(type, errorType) ||
      fCompilerAlias->SameType(type, runtimeExceptionType) ||
      fCompilerAlias->AssignableSubtype(type, runtimeExceptionType)) {
    return true;
  }
  deque<CJavaTypeSignature>::const_iterator end = fThrowable.end();
  for (deque<CJavaTypeSignature>::const_iterator i = fThrowable.begin();
       !(i == end); ++i) {
    if (fCompilerAlias->SameType(type, *i) ||
	fCompilerAlias->AssignableSubtype(type, *i)) {
      return true;
    }
  }
  return false;
}

//
//  Method name : PushThrowable
//  Description : Specifies that values of the provided type can be thrown
//    within this context.  These throwable types are held in a LIFO, so
//    the last one pushed will be the first one popped.
//
void
CCompileContext::PushThrowable(const CJavaTypeSignature& type)
{
  fThrowable.push_back(type);
}

//
//  Method name : PopThrowable
//  Description : Removes the last-pushed type from this context's throwable
//    stack.
//
void
CCompileContext::PopThrowable()
{
  fThrowable.pop_back();
}
