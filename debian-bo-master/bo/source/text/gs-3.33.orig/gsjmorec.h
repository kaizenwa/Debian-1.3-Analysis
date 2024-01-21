/* Copyright (C) 1994, 1995 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* gsjmorec.h */
/* "Wrapper" for Independent JPEG Group code jmorecfg.h */

#include "jmcorig.h"

/* Remove unwanted / unneeded features. */
#undef DCT_IFAST_SUPPORTED
#undef DCT_FLOAT_SUPPORTED
/*
 * Note: on machines with fast floating point, it might make more sense
 * to use the float DCT?
 */
#undef C_MULTISCAN_FILES_SUPPORTED
#undef ENTROPY_OPT_SUPPORTED
#undef INPUT_SMOOTHING_SUPPORTED
#undef D_MULTISCAN_FILES_SUPPORTED
#undef IDCT_SCALING_SUPPORTED
#undef UPSAMPLE_SCALING_SUPPORTED
#undef UPSAMPLE_MERGING_SUPPORTED
#undef QUANT_1PASS_SUPPORTED
#undef QUANT_2PASS_SUPPORTED
/*
 * The following are new for IJG version v6.
 */
#undef C_PROGRESSIVE_SUPPORTED
#undef D_PROGRESSIVE_SUPPORTED
