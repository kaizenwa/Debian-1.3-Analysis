#ifdef SP_NAMESPACE
namespace SP_NAMESPACE {
#endif


#ifdef SP_NAMESPACE
}
#endif
// Copyright (c) 1995 James Clark
// See the file COPYING for copying permission.

#include "config.h"

#ifdef SP_MANUAL_INST

#define SP_DEFINE_TEMPLATES
#include "Vector.h"
#undef SP_DEFINE_TEMPLATES

#include "Location.h"

#ifdef SP_NAMESPACE
namespace SP_NAMESPACE {
#endif

#ifdef __DECCXX
#pragma define_template Vector<unsigned>
#else
#ifdef SP_ANSI_CLASS_INST
template class Vector<unsigned>;
#else
typedef Vector<unsigned> Dummy_0;
#endif
#endif
#ifdef __DECCXX
#pragma define_template Vector<Location>
#else
#ifdef SP_ANSI_CLASS_INST
template class Vector<Location>;
#else
typedef Vector<Location> Dummy_1;
#endif
#endif

#ifdef SP_NAMESPACE
}
#endif

#endif /* SP_MANUAL_INST */
