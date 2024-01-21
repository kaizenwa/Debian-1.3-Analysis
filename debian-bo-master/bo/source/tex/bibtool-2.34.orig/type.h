/******************************************************************************
** $Id: type.h,v 2.14 1996/02/22 21:43:51 gerd Exp gerd $
**=============================================================================
** 
** This file is part of BibTool.
** It is distributed under the GNU General Public License.
** See the file COPYING for details.
** 
** (c) 1996 Gerd Neugebauer
** 
** Net: gerd@informatik.uni-koblenz.de
** 
******************************************************************************/

#include <stdio.h>

#ifdef __STDC__
#define _ARG(A) A
#else
#define _ARG(A) ()
#endif
 void init_type _ARG((void));			   /* type.c                 */

#define T__None      0
#define T__Upper     1
#define T__Lower     2
#define T__Allowed   4
#define T__Number    8
#define T__Space    16
#define T__Extended 32

#ifdef INIT_TYPE

int allowed[257] = {
  /*0   */ T__None,
  /*1   */ T__Space,
  /*2   */ T__Space,
  /*3   */ T__Space,
  /*4   */ T__Space,
  /*5   */ T__Space,
  /*6   */ T__Space,
  /*7   */ T__Space,
  /*8   */ T__Space,
  /*9   */ T__Space,
  /*a   */ T__Space,
  /*b   */ T__Space,
  /*c   */ T__Space,
  /*d   */ T__Space,
  /*e   */ T__Space,
  /*f   */ T__Space,
  /*0   */ T__Space,
  /*1   */ T__Space,
  /*2   */ T__Space,
  /*3   */ T__Space,
  /*4   */ T__Space,
  /*5   */ T__Space,
  /*6   */ T__Space,
  /*7   */ T__Space,
  /*8   */ T__Space,
  /*9   */ T__Space,
  /*a   */ T__Space,
  /*b   */ T__Space,
  /*c   */ T__Space,
  /*d   */ T__Space,
  /*e   */ T__Space,
  /*f   */ T__Space,
  /*0   */ T__Space,
  /*1 ! */ T__Allowed,
  /*2 " */ T__None,
  /*3 # */ T__None,
  /*4 $ */ T__Allowed,
  /*5 % */ T__None,
  /*6 & */ T__Allowed,
  /*7 ' */ T__None,
  /*8 ( */ T__None,
  /*9 ) */ T__None,
  /*a * */ T__Allowed,
  /*b + */ T__Allowed,
  /*c , */ T__None,
  /*d - */ T__Allowed,
  /*e . */ T__Allowed,
  /*f / */ T__Allowed,
  /*0 0 */ T__Allowed|T__Number,
  /*1 1 */ T__Allowed|T__Number,
  /*2 2 */ T__Allowed|T__Number,
  /*3 3 */ T__Allowed|T__Number,
  /*4 4 */ T__Allowed|T__Number,
  /*5 5 */ T__Allowed|T__Number,
  /*6 6 */ T__Allowed|T__Number,
  /*7 7 */ T__Allowed|T__Number,
  /*8 8 */ T__Allowed|T__Number,
  /*9 9 */ T__Allowed|T__Number,
  /*a : */ T__Allowed,
  /*b ; */ T__Allowed,
  /*c < */ T__Allowed,
  /*d = */ T__None,
  /*e > */ T__Allowed,
  /*f ? */ T__Allowed,
  /*0 @ */ T__Allowed,
  /*1 A */ T__Allowed|T__Upper,
  /*2 B */ T__Allowed|T__Upper,
  /*3 C */ T__Allowed|T__Upper,
  /*4 D */ T__Allowed|T__Upper,
  /*5 E */ T__Allowed|T__Upper,
  /*6 F */ T__Allowed|T__Upper,
  /*7 G */ T__Allowed|T__Upper,
  /*8 H */ T__Allowed|T__Upper,
  /*9 I */ T__Allowed|T__Upper,
  /*a J */ T__Allowed|T__Upper,
  /*b K */ T__Allowed|T__Upper,
  /*c L */ T__Allowed|T__Upper,
  /*d M */ T__Allowed|T__Upper,
  /*e N */ T__Allowed|T__Upper,
  /*f O */ T__Allowed|T__Upper,
  /*0 P */ T__Allowed|T__Upper,
  /*1 Q */ T__Allowed|T__Upper,
  /*2 R */ T__Allowed|T__Upper,
  /*3 S */ T__Allowed|T__Upper,
  /*4 T */ T__Allowed|T__Upper,
  /*5 U */ T__Allowed|T__Upper,
  /*6 V */ T__Allowed|T__Upper,
  /*7 W */ T__Allowed|T__Upper,
  /*8 X */ T__Allowed|T__Upper,
  /*9 Y */ T__Allowed|T__Upper,
  /*a Z */ T__Allowed|T__Upper,
  /*b [ */ T__Allowed,
  /*c \ */ T__Allowed,
  /*d ] */ T__Allowed,
  /*e ^ */ T__Allowed,
  /*f _ */ T__Allowed,
  /*0 ` */ T__Allowed,
  /*1 a */ T__Allowed|T__Lower,
  /*2 b */ T__Allowed|T__Lower,
  /*3 c */ T__Allowed|T__Lower,
  /*4 d */ T__Allowed|T__Lower,
  /*5 e */ T__Allowed|T__Lower,
  /*6 f */ T__Allowed|T__Lower,
  /*7 g */ T__Allowed|T__Lower,
  /*8 h */ T__Allowed|T__Lower,
  /*9 i */ T__Allowed|T__Lower,
  /*a j */ T__Allowed|T__Lower,
  /*b k */ T__Allowed|T__Lower,
  /*c l */ T__Allowed|T__Lower,
  /*d m */ T__Allowed|T__Lower,
  /*e n */ T__Allowed|T__Lower,
  /*f o */ T__Allowed|T__Lower,
  /*0 p */ T__Allowed|T__Lower,
  /*1 q */ T__Allowed|T__Lower,
  /*2 r */ T__Allowed|T__Lower,
  /*3 s */ T__Allowed|T__Lower,
  /*4 t */ T__Allowed|T__Lower,
  /*5 u */ T__Allowed|T__Lower,
  /*6 v */ T__Allowed|T__Lower,
  /*7 w */ T__Allowed|T__Lower,
  /*8 x */ T__Allowed|T__Lower,
  /*9 y */ T__Allowed|T__Lower,
  /*a z */ T__Allowed|T__Lower,
  /*b { */ T__None,
  /*c | */ T__Allowed,
  /*d } */ T__None,
  /*e ~ */ T__None,
  /*f  */ T__Allowed,
  /*0  */ T__Allowed|T__Extended,
  /*1  */ T__Allowed|T__Extended,
  /*2  */ T__Allowed|T__Extended,
  /*3  */ T__Allowed|T__Extended,
  /*4  */ T__Allowed|T__Extended,
  /*5  */ T__Allowed|T__Extended,
  /*6  */ T__Allowed|T__Extended,
  /*7  */ T__Allowed|T__Extended,
  /*8  */ T__Allowed|T__Extended,
  /*9  */ T__Allowed|T__Extended,
  /*a  */ T__Allowed|T__Extended,
  /*b  */ T__Allowed|T__Extended,
  /*c  */ T__Allowed|T__Extended,
  /*d  */ T__Allowed|T__Extended,
  /*e  */ T__Allowed|T__Extended,
  /*f  */ T__Allowed|T__Extended,
  /*0  */ T__Allowed|T__Extended,
  /*1  */ T__Allowed|T__Extended,
  /*2  */ T__Allowed|T__Extended,
  /*3  */ T__Allowed|T__Extended,
  /*4  */ T__Allowed|T__Extended,
  /*5  */ T__Allowed|T__Extended,
  /*6  */ T__Allowed|T__Extended,
  /*7  */ T__Allowed|T__Extended,
  /*8  */ T__Allowed|T__Extended,
  /*9  */ T__Allowed|T__Extended,
  /*a  */ T__Allowed|T__Extended,
  /*b  */ T__Allowed|T__Extended,
  /*c  */ T__Allowed|T__Extended,
  /*d  */ T__Allowed|T__Extended,
  /*e  */ T__Allowed|T__Extended,
  /*f  */ T__Allowed|T__Extended,
  /*0  */ T__Allowed|T__Extended,
  /*1  */ T__Allowed|T__Extended,
  /*2  */ T__Allowed|T__Extended,
  /*3  */ T__Allowed|T__Extended,
  /*4  */ T__Allowed|T__Extended,
  /*5  */ T__Allowed|T__Extended,
  /*6  */ T__Allowed|T__Extended,
  /*7  */ T__Allowed|T__Extended,
  /*8  */ T__Allowed|T__Extended,
  /*9  */ T__Allowed|T__Extended,
  /*a  */ T__Allowed|T__Extended,
  /*b  */ T__Allowed|T__Extended,
  /*c  */ T__Allowed|T__Extended,
  /*d  */ T__Allowed|T__Extended,
  /*e  */ T__Allowed|T__Extended,
  /*f  */ T__Allowed|T__Extended,
  /*0  */ T__Allowed|T__Extended,
  /*1  */ T__Allowed|T__Extended,
  /*2  */ T__Allowed|T__Extended,
  /*3  */ T__Allowed|T__Extended,
  /*4  */ T__Allowed|T__Extended,
  /*5  */ T__Allowed|T__Extended,
  /*6  */ T__Allowed|T__Extended,
  /*7  */ T__Allowed|T__Extended,
  /*8  */ T__Allowed|T__Extended,
  /*9  */ T__Allowed|T__Extended,
  /*a  */ T__Allowed|T__Extended,
  /*b  */ T__Allowed|T__Extended,
  /*c  */ T__Allowed|T__Extended,
  /*d  */ T__Allowed|T__Extended,
  /*e  */ T__Allowed|T__Extended,
  /*f  */ T__Allowed|T__Extended,
  /*0  */ T__Allowed|T__Extended,
  /*1  */ T__Allowed|T__Extended,
  /*2  */ T__Allowed|T__Extended,
  /*3  */ T__Allowed|T__Extended,
  /*4  */ T__Allowed|T__Extended,
  /*5  */ T__Allowed|T__Extended,
  /*6  */ T__Allowed|T__Extended,
  /*7  */ T__Allowed|T__Extended,
  /*8  */ T__Allowed|T__Extended,
  /*9  */ T__Allowed|T__Extended,
  /*a  */ T__Allowed|T__Extended,
  /*b  */ T__Allowed|T__Extended,
  /*c  */ T__Allowed|T__Extended,
  /*d  */ T__Allowed|T__Extended,
  /*e  */ T__Allowed|T__Extended,
  /*f  */ T__Allowed|T__Extended,
  /*0  */ T__Allowed|T__Extended,
  /*1  */ T__Allowed|T__Extended,
  /*2  */ T__Allowed|T__Extended,
  /*3  */ T__Allowed|T__Extended,
  /*4  */ T__Allowed|T__Extended,
  /*5  */ T__Allowed|T__Extended,
  /*6  */ T__Allowed|T__Extended,
  /*7  */ T__Allowed|T__Extended,
  /*8  */ T__Allowed|T__Extended,
  /*9  */ T__Allowed|T__Extended,
  /*a  */ T__Allowed|T__Extended,
  /*b  */ T__Allowed|T__Extended,
  /*c  */ T__Allowed|T__Extended,
  /*d  */ T__Allowed|T__Extended,
  /*e  */ T__Allowed|T__Extended,
  /*f  */ T__Allowed|T__Extended,
  /*0  */ T__Allowed|T__Extended,
  /*1  */ T__Allowed|T__Extended,
  /*2  */ T__Allowed|T__Extended,
  /*3  */ T__Allowed|T__Extended,
  /*4  */ T__Allowed|T__Extended,
  /*5  */ T__Allowed|T__Extended,
  /*6  */ T__Allowed|T__Extended,
  /*7  */ T__Allowed|T__Extended,
  /*8  */ T__Allowed|T__Extended,
  /*9  */ T__Allowed|T__Extended,
  /*a  */ T__Allowed|T__Extended,
  /*b  */ T__Allowed|T__Extended,
  /*c  */ T__Allowed|T__Extended,
  /*d  */ T__Allowed|T__Extended,
  /*e  */ T__Allowed|T__Extended,
  /*f  */ T__Allowed|T__Extended,
  /*0  */ T__Allowed|T__Extended,
  /*1  */ T__Allowed|T__Extended,
  /*2  */ T__Allowed|T__Extended,
  /*3  */ T__Allowed|T__Extended,
  /*4  */ T__Allowed|T__Extended,
  /*5  */ T__Allowed|T__Extended,
  /*6  */ T__Allowed|T__Extended,
  /*7  */ T__Allowed|T__Extended,
  /*8  */ T__Allowed|T__Extended,
  /*9  */ T__Allowed|T__Extended,
  /*a  */ T__Allowed|T__Extended,
  /*b  */ T__Allowed|T__Extended,
  /*c  */ T__Allowed|T__Extended,
  /*d  */ T__Allowed|T__Extended,
  /*e  */ T__Allowed|T__Extended,
  /*f  */ T__Allowed|T__Extended
};
 char trans_lower[256];
 char trans_upper[256];
 char trans_id[256];

#else

 extern int  allowed[];
 extern char trans_lower[256];
 extern char trans_upper[256];
 extern char trans_id[256];
#endif

#define is_allowed(C)         (allowed[(int)C]&T__Allowed)

#define is_upper(C)           (allowed[(int)C]&T__Upper)
#define is_lower(C)           (allowed[(int)C]&T__Lower)
#define is_alpha(C)           (allowed[(int)C]&(T__Upper|T__Lower))
#define is_digit(C)           (allowed[(int)C]&T__Number)
#define is_space(C)           (allowed[(int)C]&T__Space)
#define is_extended(C)        (allowed[(int)C]&T__Extended)

#define ToLower(C)            trans_lower[(int)(C)]
#define ToUpper(C)            trans_upper[(int)(C)]


#define SYMBOL_TYPE_LOWER 0
#define SYMBOL_TYPE_UPPER 1
#define SYMBOL_TYPE_CASED 2


#ifdef __STDC__
#define _ARG(A) A
#else
#define _ARG(A) ()
#endif
 int case_cmp _ARG((char * s,char * t));	   /* type.c                 */
 void init_type _ARG((void));			   /* type.c                 */
