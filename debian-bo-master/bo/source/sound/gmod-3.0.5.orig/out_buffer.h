// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#ifndef _out_buffer_h
#define _out_buffer_h

struct out_buffer
{
	int n_bytes;
	int offset;
	char buffer[2048];
};
#endif
