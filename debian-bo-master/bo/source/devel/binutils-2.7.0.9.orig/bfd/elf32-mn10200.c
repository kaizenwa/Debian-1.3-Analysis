/* Matsushita 10200 specific support for 32-bit ELF
   Copyright (C) 1996, 1997 Free Software Foundation, Inc.

This file is part of BFD, the Binary File Descriptor library.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#include "bfd.h"
#include "sysdep.h"
#include "libbfd.h"
#include "elf-bfd.h"

static reloc_howto_type *bfd_elf32_bfd_reloc_type_lookup
  PARAMS ((bfd *abfd, bfd_reloc_code_real_type code));
static void mn10200_info_to_howto
  PARAMS ((bfd *, arelent *, Elf32_Internal_Rela *));


/* We have to use RELA instructions since md_apply_fix3 in the assembler
   does absolutely nothing.  */
#define USE_RELA

enum reloc_type
{
  R_MN10200_NONE = 0,
  R_MN10200_32,
  R_MN10200_16,
  R_MN10200_8,
  R_MN10200_24,
  R_MN10200_PCREL8,
  R_MN10200_PCREL16,
  R_MN10200_PCREL24,
  R_MN10200_MAX
};

static reloc_howto_type elf_mn10200_howto_table[] =
{
  /* Dummy relocation.  Does nothing.  */
  HOWTO (R_MN10200_NONE,
	 0,
	 2,
	 16,
	 false,
	 0,
	 complain_overflow_bitfield,
	 bfd_elf_generic_reloc,
	 "R_MN10200_NONE",
	 false,
	 0,
	 0,
	 false),
  /* Standard 32 bit reloc.  */
  HOWTO (R_MN10200_32,
	 0,
	 2,
	 32,
	 false,
	 0,
	 complain_overflow_bitfield,
	 bfd_elf_generic_reloc,
	 "R_MN10200_32",
	 false,
	 0xffffffff,
	 0xffffffff,
	 false),
  /* Standard 16 bit reloc.  */
  HOWTO (R_MN10200_16,
	 0,
	 1,
	 16,
	 false,
	 0,
	 complain_overflow_bitfield,
	 bfd_elf_generic_reloc,
	 "R_MN10200_16",
	 false,
	 0xffff,
	 0xffff,
	 false),
  /* Standard 8 bit reloc.  */
  HOWTO (R_MN10200_8,
	 0,
	 0,
	 8,
	 false,
	 0,
	 complain_overflow_bitfield,
	 bfd_elf_generic_reloc,
	 "R_MN10200_8",
	 false,
	 0xff,
	 0xff,
	 false),
  /* Standard 24 bit reloc.  */
  HOWTO (R_MN10200_24,
	 0,
	 2,
	 24,
	 false,
	 0,
	 complain_overflow_bitfield,
	 bfd_elf_generic_reloc,
	 "R_MN10200_24",
	 false,
	 0xffffff,
	 0xffffff,
	 false),
  /* Simple 8 pc-relative reloc.  */
  HOWTO (R_MN10200_PCREL8,
	 0,
	 0,
	 8,
	 true,
	 0,
	 complain_overflow_bitfield,
	 bfd_elf_generic_reloc,
	 "R_MN10200_PCREL8",
	 false,
	 0xff,
	 0xff,
	 true),
  /* Simple 16 pc-relative reloc.  */
  HOWTO (R_MN10200_PCREL16,
	 0,
	 1,
	 16,
	 true,
	 0,
	 complain_overflow_bitfield,
	 bfd_elf_generic_reloc,
	 "R_MN10200_PCREL16",
	 false,
	 0xffff,
	 0xffff,
	 true),
  /* Simple 32bit pc-relative reloc with a 1 byte adjustment
     to get the pc-relative offset correct.  */
  HOWTO (R_MN10200_PCREL24,
	 0,
	 2,
	 24,
	 true,
	 0,
	 complain_overflow_bitfield,
	 bfd_elf_generic_reloc,
	 "R_MN10200_PCREL24",
	 false,
	 0xffffff,
	 0xffffff,
	 true),
};

struct mn10200_reloc_map
{
  unsigned char bfd_reloc_val;
  unsigned char elf_reloc_val;
};

static const struct mn10200_reloc_map mn10200_reloc_map[] =
{
  { BFD_RELOC_NONE, R_MN10200_NONE, },
  { BFD_RELOC_32, R_MN10200_32, },
  { BFD_RELOC_16, R_MN10200_16, },
  { BFD_RELOC_8, R_MN10200_8, },
  { BFD_RELOC_24, R_MN10200_24, },
  { BFD_RELOC_8_PCREL, R_MN10200_PCREL8, },
  { BFD_RELOC_16_PCREL, R_MN10200_PCREL16, },
  { BFD_RELOC_24_PCREL, R_MN10200_PCREL24, },
};

static reloc_howto_type *
bfd_elf32_bfd_reloc_type_lookup (abfd, code)
     bfd *abfd;
     bfd_reloc_code_real_type code;
{
  unsigned int i;

  for (i = 0;
       i < sizeof (mn10200_reloc_map) / sizeof (struct mn10200_reloc_map);
       i++)
    {
      if (mn10200_reloc_map[i].bfd_reloc_val == code)
	return &elf_mn10200_howto_table[mn10200_reloc_map[i].elf_reloc_val];
    }

  return NULL;
}

/* Set the howto pointer for an MN10200 ELF reloc.  */

static void
mn10200_info_to_howto (abfd, cache_ptr, dst)
     bfd *abfd;
     arelent *cache_ptr;
     Elf32_Internal_Rela *dst;
{
  unsigned int r_type;

  r_type = ELF32_R_TYPE (dst->r_info);
  BFD_ASSERT (r_type < (unsigned int) R_MN10200_MAX);
  cache_ptr->howto = &elf_mn10200_howto_table[r_type];
}

#define TARGET_LITTLE_SYM	bfd_elf32_mn10200_vec
#define TARGET_LITTLE_NAME	"elf32-mn10200"
#define ELF_ARCH		bfd_arch_mn10200
#define ELF_MACHINE_CODE	EM_CYGNUS_MN10200
#define ELF_MAXPAGESIZE		0x1000

#define elf_info_to_howto	mn10200_info_to_howto
#define elf_info_to_howto_rel	0

#define elf_symbol_leading_char '_'

#include "elf32-target.h"
