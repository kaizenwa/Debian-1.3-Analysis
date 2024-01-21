/*
   PRMTYP.I
   Define functions which set file primitive data type formats.

   $Id: prmtyp.i,v 1.1 1993/08/27 18:50:06 munro Exp $
 */
/*    Copyright (c) 1994.  The Regents of the University of California.
                    All rights reserved.  */

/*--------------------------------------------------------------------------*/

/* install_struct, file, type_name,  size, alignment, order, layout
     size--      size in bytes
     alignment-- alignment boundary in structs in bytes
     order--     1 MSB first (big-endian), -1 LSB first (little-endian)
                 2 VAX floating point (middle-endian)
		 The sign of order determines the order of "words"
		 within the type; the absolute value of order is the
		 "word" size in bytes.
     layout--    [sign_address,  exponent_address, exponent_bits,
                  mantissa_address, mantissa_bits,
		  mantissa_normalization, exponent_bias]
	These are bit addresses, assuming big-endian byte ordering
	(order=1).
	The mantissa_normalization is 0 unless the first bit of the
	mantissa (at mantissa_address) is always set, in which case
	it is 1.  In the former case, the mantissa is 1.MMMMMMM...,
	in the latter case, it is M.MMMMMM..., where the first M bit
	is always 1.
	The number is {(-1)^S * 2^(EEEE... - exponent_bias) * mantissa},
	where the mantisa is either 1.MMMM... or M.MMMM..., as above,
	S is the sign bit, and EEEE... is the exponent.
 */

/*--------------------------------------------------------------------------*/

func raw_sun_p(file)
/* DOCUMENT raw_sun_p, file
     sets FILE primitive data types to be native to Sun Sparc, HP, IBM, etc.
 */
{
  install_struct, file, "char",    1, 1, 1;
  install_struct, file, "short",   2, 2, 1;
  install_struct, file, "int",     4, 4, 1;
  install_struct, file, "long",    4, 4, 1;
  install_struct, file, "float",   4, 4, 1, [0, 1, 8,  9,23, 0,  0x7f];
  install_struct, file, "double",  8, 8, 1, [0, 1,11, 12,52, 0, 0x3ff];
  struct_align, file, 1;
}

func raw_sun3_p(file)
/* DOCUMENT raw_sun3_p, file
     sets FILE primitive data types to be native to Sun-2 or Sun-3.
 */
{
  install_struct, file, "char",    1, 1, 1;
  install_struct, file, "short",   2, 2, 1;
  install_struct, file, "int",     4, 2, 1;
  install_struct, file, "long",    4, 2, 1;
  install_struct, file, "float",   4, 2, 1, [0, 1, 8,  9,23, 0,  0x7f];
  install_struct, file, "double",  8, 2, 1, [0, 1,11, 12,52, 0, 0x3ff];
  struct_align, file, 1;
}

func raw_dec_p(file)
/* DOCUMENT raw_dec_p, file
     sets FILE primitive data types to be native to DEC (MIPS) workstations.
 */
{
  install_struct, file, "char",    1, 1, -1;
  install_struct, file, "short",   2, 2, -1;
  install_struct, file, "int",     4, 4, -1;
  install_struct, file, "long",    4, 4, -1;
  install_struct, file, "float",   4, 4, -1, [0, 1, 8,  9,23, 0,  0x7f];
  install_struct, file, "double",  8, 8, -1, [0, 1,11, 12,52, 0, 0x3ff];
  struct_align, file, 1;
}

func raw_alpha_p(file)
/* DOCUMENT raw_alpha_p, file
     sets FILE primitive data types to be native to DEC alpha workstations.
 */
{
  install_struct, file, "char",    1, 1, -1;
  install_struct, file, "short",   2, 2, -1;
  install_struct, file, "int",     4, 4, -1;
  install_struct, file, "long",    8, 8, -1;
  install_struct, file, "float",   4, 4, -1, [0, 1, 8,  9,23, 0,  0x7f];
  install_struct, file, "double",  8, 8, -1, [0, 1,11, 12,52, 0, 0x3ff];
  struct_align, file, 1;
}

func raw_cray_p(file)
/* DOCUMENT raw_cray_p, file
     sets FILE primitive data types to be native to Cray 1, XMP, and YMP.
 */
{
  install_struct, file, "char",    1, 1, 1;
  install_struct, file, "short",   8, 8, 1;
  install_struct, file, "int",     8, 8, 1;
  install_struct, file, "long",    8, 8, 1;
  install_struct, file, "float",   8, 8, 1, [0, 1,15, 16,48, 1, 0x4000];
  install_struct, file, "double",  8, 8, 1, [0, 1,15, 16,48, 1, 0x4000];
  struct_align, file, 8;
}

func raw_mac_p(file)
/* DOCUMENT raw_mac_p, file
     sets FILE primitive data types to be native to MacIntosh, 8 byte double.
 */
{
  install_struct, file, "char",    1, 1, 1;
  install_struct, file, "short",   2, 2, 1;
  install_struct, file, "int",     2, 2, 1; /* some compilers 4 byte int */
  install_struct, file, "long",    4, 2, 1;
  install_struct, file, "float",   4, 2, 1, [0, 1, 8,  9,23, 0,  0x7f];
  install_struct, file, "double",  8, 2, 1, [0, 1,11, 12,52, 0, 0x3ff];
  struct_align, file, 1;
}

func raw_macl_p(file)
/* DOCUMENT raw_macl_p, file
     sets FILE primitive data types to be native to MacIntosh, long double.
 */
{
  install_struct, file, "char",    1, 1, 1;
  install_struct, file, "short",   2, 2, 1;
  install_struct, file, "int",     2, 2, 1; /* some compilers 4 byte int */
  install_struct, file, "long",    4, 2, 1;
  install_struct, file, "float",   4, 2, 1, [0, 1, 8,  9,23, 0,   0x7f];
  install_struct, file, "double", 12, 2, 1, [0, 1,15, 32,64, 1, 0x3ffe];
  struct_align, file, 1;
}

func raw_pc_p(file)
/* DOCUMENT raw_pc_p, file
     sets FILE primitive data types to be native to IBM PC.
 */
{
  install_struct, file, "char",    1, 1, -1;
  install_struct, file, "short",   2, 2, -1;
  install_struct, file, "int",     2, 2, -1;  /* XENIX uses 4 byte int */
  install_struct, file, "long",    4, 2, -1;
  install_struct, file, "float",   4, 2, -1, [0, 1, 8,  9,23, 0,  0x7f];
  install_struct, file, "double",  8, 2, -1, [0, 1,11, 12,52, 0, 0x3ff];
  struct_align, file, 1;
}

func raw_vax_p(file)
/* DOCUMENT raw_vax_p, file
     sets FILE primitive data types to be native to VAXen, H-double, only.
 */
{
  install_struct, file, "char",    1, 1, -1;
  install_struct, file, "short",   2, 1, -1;
  install_struct, file, "int",     4, 1, -1;
  install_struct, file, "long",    4, 1, -1;
  install_struct, file, "float",   4, 1, 2, [0, 1, 8,  9,23, 0,  0x81];
  install_struct, file, "double",  8, 1, 2, [0, 1, 8,  9,55, 0,  0x81];
  struct_align, file, 1;
}

func raw_vaxg_p(file)
/* DOCUMENT raw_vaxg_p, file
     sets FILE primitive data types to be native to VAXen, G-double, only.
 */
{
  install_struct, file, "char",    1, 1, -1;
  install_struct, file, "short",   2, 1, -1;
  install_struct, file, "int",     4, 1, -1;
  install_struct, file, "long",    4, 1, -1;
  install_struct, file, "float",   4, 1, 2, [0, 1, 8,  9,23, 0,  0x81];
  install_struct, file, "double",  8, 1, 2, [0, 1,11, 12,52, 0, 0x401];
  struct_align, file, 1;
}

func raw_xdr_p(file)
/* DOCUMENT raw_xdr_p, file
     sets FILE primitive data types to be XDR (external data representation).
 */
{
  install_struct, file, "char",    1, 1, 1;
  install_struct, file, "short",   2, 2, 1;
  install_struct, file, "int",     4, 4, 1;
  install_struct, file, "long",    4, 4, 1;
  install_struct, file, "float",   4, 4, 1, [0, 1, 8,  9,23, 0,  0x7f];
  install_struct, file, "double",  8, 4, 1, [0, 1,11, 12,52, 0, 0x3ff];
  struct_align, file, 1;
}

/*--------------------------------------------------------------------------*/
