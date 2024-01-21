/* This files contains functions to convert between big endian internal
   representation and little endian external representation. */

#if 0
#ifdef linux
#include <a.out.h>
#else
#include "../a.out.h"
#endif
#endif

#include <linux/a.out.h>

#include "md.h"

long
md_chars_to_number (p, nbytes)
     unsigned char *p;
     int nbytes;
{
  long value;

  for (value = 0, p += nbytes - 1; nbytes--; p--)
    value = (value << 8) | *p;
  return value;
}

void
md_chars_to_hdr (p, hdr)
     unsigned char *p;
     struct exec *hdr;
{
  hdr->a_info = md_chars_to_number (p, sizeof hdr->a_info);
  hdr->a_text = md_chars_to_number (p + 4, sizeof hdr->a_text);
  hdr->a_data = md_chars_to_number (p + 8, sizeof hdr->a_data);
  hdr->a_bss = md_chars_to_number (p + 12, sizeof hdr->a_bss);
  hdr->a_syms= md_chars_to_number (p + 16, sizeof hdr->a_syms);
  hdr->a_entry = md_chars_to_number (p + 20, sizeof hdr->a_entry);
  hdr->a_trsize = md_chars_to_number (p + 24, sizeof hdr->a_trsize);
  hdr->a_drsize = md_chars_to_number (p + 28, sizeof hdr->a_drsize);
}
