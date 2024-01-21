// Copyright (c) 1996  David Engberg  All rights reserved
// $Id: CompilerInstantiations2.C,v 1.4 1996/04/25 21:10:43 geppetto Exp $

#include "unicode_string.h"
#include "JavaFieldSignature.h"
#include "CodeSequence.h"
#include <vector>
#include <deque>
class CCatchClause;
class CExpression;

template class deque<CJavaFieldSignature>;
template bool operator!=(deque<CJavaFieldSignature>::iterator const &,
			 deque<CJavaFieldSignature>::iterator const &);
template bool operator!=(deque<CJavaFieldSignature>::const_iterator const &,
			 deque<CJavaFieldSignature>::const_iterator const &);
template class deque<CCatchClause*>;
template bool operator!=(deque<CCatchClause*>::iterator const &,
			 deque<CCatchClause*>::iterator const &);
template bool operator!=(deque<CCatchClause*>::const_iterator const &,
			 deque<CCatchClause*>::const_iterator const &);
template class deque<CExpression*>;
template bool operator!=(deque<CExpression*>::iterator const &,
			 deque<CExpression*>::iterator const &);
template bool operator!=(deque<CExpression*>::const_iterator const &,
			 deque<CExpression*>::const_iterator const &);
template class deque<deque<unicode_string> >;
template bool operator!=(deque<deque<unicode_string> >::iterator const &,
			 deque<deque<unicode_string> >::iterator const &);
template bool operator!=(deque<deque<unicode_string> >::const_iterator const &,
			 deque<deque<unicode_string> >::const_iterator const&);

template class vector<CCodeSequence::Instruction>;
template class vector<unsigned long>;
