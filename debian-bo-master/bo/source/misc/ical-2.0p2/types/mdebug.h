/* Copyright (c) 1995 by Sanjay Ghemawat */
#ifndef _MDEBUG_H
#define _MDEBUG_H

// Include this file and compile with -DMALLOC_DEBUG if you want
// to find memory leaks.

#ifdef MALLOC_DEBUG

extern "C" {
    extern char* _malloc_file;
    extern unsigned int _malloc_line;
}

// XXX This is very very very ugly and probably buggy as well.
#define new ((_malloc_line = __LINE__) && (_malloc_file = __FILE__) && 0) ? 0 : new

#endif

#endif /* _MDEBUG_H */
