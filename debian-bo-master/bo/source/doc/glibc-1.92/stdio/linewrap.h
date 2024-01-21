/* Word-wrapping and line-truncating streams.
Copyright (C) 1996 Free Software Foundation, Inc.
This file is part of the GNU C Library.

The GNU C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU C Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

#ifndef __LINEWRAP_H__
#define __LINEWRAP_H__

#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include <features.h>

#include <string.h>		/* Need size_t.  */

__BEGIN_DECLS

/* We keep this data for each line-wrapping stream.  */
struct line_wrap_data
  {
    size_t lmargin, rmargin;	/* Left and right margins.  */
    size_t wmargin;		/* Margin to wrap to, or -1 to truncate.  */
    size_t point;		/* Current column of last chars flushed.  */

    /* Original cookie and hooks from the stream.  */
    void *cookie;
    void (*output) (FILE *, int);
    __io_close_fn *close;
    __io_fileno_fn *fileno;
    __io_seek_fn *seek;
  };

/* Modify STREAM so that it prefixes lines written on it with LMARGIN spaces
   and limits them to RMARGIN columns total.  If WMARGIN >= 0, words that
   extend past RMARGIN are wrapped by replacing the whitespace before them
   with a newline and WMARGIN spaces.  Otherwise, chars beyond RMARGIN are
   simply dropped until a newline.  Returns STREAM after modifying it, or
   NULL if there was an error.  */
FILE *line_wrap_stream (FILE *stream,
			size_t lmargin, size_t rmargin, size_t wmargin);

/* Remove the hooks placed in STREAM by `line_wrap_stream'.  */
void line_unwrap_stream (FILE *stream);

/* Returns true if STREAM is line wrapped.  */
extern inline int line_wrapped (FILE *stream); 

/* If STREAM is not line-wrapped return -1, else return its left margin.  */
extern size_t line_wrap_lmargin (FILE *stream); 

/* If STREAM is not line-wrapped return -1, else set its left margin to
   LMARGIN and return the old value.  */
extern size_t line_wrap_set_lmargin (FILE *stream, size_t lmargin); 

/* If STREAM is not line-wrapped return -1, else return its left margin.  */
extern size_t line_wrap_rmargin (FILE *stream); 

/* If STREAM is not line-wrapped return -1, else set its right margin to
   RMARGIN and return the old value.  */
extern size_t line_wrap_set_rmargin (FILE *stream, size_t rmargin); 

/* If STREAM is not line-wrapped return -1, else return its wrap margin.  */
extern size_t line_wrap_wmargin (FILE *stream); 

/* If STREAM is not line-wrapped return -1, else set its left margin to
   WMARGIN and return the old value.  */
extern size_t line_wrap_set_wmargin (FILE *stream, size_t wmargin); 

/* If STREAM is not line-wrapped return -1, else return the column number of
   the current output point.  */
extern size_t line_wrap_point (FILE *stream); 


#ifdef	__OPTIMIZE__

extern void __line_wrap_output (FILE *, int); /* private */

/* Returns true if STREAM is line wrapped.  */
extern inline int
line_wrapped (FILE *stream)
{
  return (stream->__room_funcs.__output == &__line_wrap_output);
}

/* If STREAM is not line-wrapped return -1, else return its left margin.  */
extern inline size_t
line_wrap_lmargin (FILE *stream)
{
  if (! line_wrapped (stream))
    return -1;
  return ((struct line_wrap_data *)stream->__cookie)->lmargin;
}

/* If STREAM is not line-wrapped return -1, else set its left margin to
   LMARGIN and return the old value.  */
extern inline size_t
line_wrap_set_lmargin (FILE *stream, size_t lmargin)
{
  if (! line_wrapped (stream))
    return -1;
  else
    {
      struct line_wrap_data *d = stream->__cookie;
      size_t old = d->lmargin;
      d->lmargin = lmargin;
      return old;
    }
}

/* If STREAM is not line-wrapped return -1, else return its left margin.  */
extern inline size_t
line_wrap_rmargin (FILE *stream)
{
  if (! line_wrapped (stream))
    return -1;
  return ((struct line_wrap_data *)stream->__cookie)->rmargin;
}

/* If STREAM is not line-wrapped return -1, else set its right margin to
   RMARGIN and return the old value.  */
extern inline size_t
line_wrap_set_rmargin (FILE *stream, size_t rmargin)
{
  if (! line_wrapped (stream))
    return -1;
  else
    {
      struct line_wrap_data *d = stream->__cookie;
      size_t old = d->rmargin;
      d->rmargin = rmargin;
      return old;
    }
}

/* If STREAM is not line-wrapped return -1, else return its wrap margin.  */
extern inline size_t
line_wrap_wmargin (FILE *stream)
{
  if (! line_wrapped (stream))
    return -1;
  return ((struct line_wrap_data *)stream->__cookie)->wmargin;
}

/* If STREAM is not line-wrapped return -1, else set its left margin to
   WMARGIN and return the old value.  */
extern inline size_t
line_wrap_set_wmargin (FILE *stream, size_t wmargin)
{
  if (! line_wrapped (stream))
    return -1;
  else
    {
      struct line_wrap_data *d = stream->__cookie;
      size_t old = d->wmargin;
      d->wmargin = wmargin;
      return old;
    }
}

/* If STREAM is not line-wrapped return -1, else return the column number of
   the current output point.  */
extern inline size_t
line_wrap_point (FILE *stream)
{
  if (! line_wrapped (stream))
    return -1;
  return ((struct line_wrap_data *)stream->__cookie)->point;
}

#endif /* Optimizing.  */

__END_DECLS

#endif /* __LINEWRAP_H__ */
