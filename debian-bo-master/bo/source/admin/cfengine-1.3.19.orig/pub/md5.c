/* MDDRIVER.C - test driver for MD2, MD4 and MD5
 */

/* Copyright (C) 1990-2, RSA Data Security, Inc. Created 1990. All
rights reserved.

RSA Data Security, Inc. makes no representations concerning either
the merchantability of this software or the suitability of this
software for any particular purpose. It is provided "as is"
without express or implied warranty of any kind.

These notices must be retained in any copies of any part of this
documentation and/or software.
 */



  /******************************************************************

   The return value structure has been altered by Mark to fit into
   cfengine instead of printing out strings on the console. Also
   cf appended to the names to counteract buggy library functions in
   solaris and maybe other systems which invade the name space!!!

   *******************************************************************/


/* The following makes MD default to MD5 if it has not already been
  defined with C compiler flags.
 */


#ifndef MD
#define MD 5
#endif

#include <stdio.h>
#include <string.h>
#include "global.h"
#include "../src/cf.defs.h"
#include "../src/cf.extern.h"
#if MD == 5
#include "md5.h"
#endif

/* Length of test block, number of test blocks.
 */
#define TEST_BLOCK_LEN 1000
#define TEST_BLOCK_COUNT 1000

       void MDFile PROTO_LIST ((char *));
void MDFilter PROTO_LIST ((void));
void MDPrint PROTO_LIST ((unsigned char [16]));

#if MD == 2
#define MD_CTX MD2_CTX
#define MDInit MD2Init
#define MDUpdate MD2Update
#define MDFinal MD2Final
#endif
#if MD == 4
#define MD_CTX MD4_CTX
#define MDInit MD4Init
#define MDUpdate MD4Update
#define MDFinal MD4Final
#endif
#if MD == 5
#define MD_CTX MD5_CTX
#define MDInit MD5Init
#define MDUpdate MD5Update
#define MDFinal MD5Final
#endif

/**************************************************************/

/* Digests a file and prints the result.
 */

void MDFile (filename,digest)
char *filename;
unsigned char digest[16];

{ FILE *file;
  MD_CTX context;
  int len;
  unsigned char buffer[1024];

Debug2("MDFile(%s)\n",filename);
  
if ((file = fopen (filename, "rb")) == NULL)
   {
   printf ("%s can't be opened\n", filename);
   }
else
   {
   cfMD5Init (&context);
   while (len = fread (buffer, 1, 1024, file))
      {
      cfMD5Update (&context, buffer, len);
      }
   
   cfMD5Final (digest, &context);

   fclose (file);
   }
}
