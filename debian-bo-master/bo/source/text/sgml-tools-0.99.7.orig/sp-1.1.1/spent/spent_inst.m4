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

__instantiate(Vector<unsigned>)
__instantiate(Vector<Location>)

#ifdef SP_NAMESPACE
}
#endif

#endif /* SP_MANUAL_INST */
