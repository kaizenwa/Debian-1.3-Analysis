/* Copyright (c) 1995 by Sanjay Ghemawat */
#include "intset.h"

#define HTABLE   IntSet
#define HTYPE    int
#define HCONTROL IntControl
#include "htable_p.h"
