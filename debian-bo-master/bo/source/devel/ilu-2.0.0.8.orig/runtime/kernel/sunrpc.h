/*
Copyright (c) 1991-1995 Xerox Corporation.  All Rights Reserved.  

Unlimited use, reproduction, and distribution of this software is
permitted.  Any copy of this software must include both the above
copyright notice of Xerox Corporation and this paragraph.  Any
distribution of this software must comply with all applicable United
States export control laws.  This software is made available AS IS,
and XEROX CORPORATION DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED,
INCLUDING WITHOUT LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE, AND NOTWITHSTANDING ANY OTHER
PROVISION CONTAINED HEREIN, ANY LIABILITY FOR DAMAGES RESULTING FROM
THE SOFTWARE OR ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN
CONTRACT, TORT (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF
XEROX CORPORATION IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
*/
/* $Id: sunrpc.h,v 1.26 1995/08/30 06:08:23 spreitze Exp $ */
/* Last tweaked by Mike Spreitzer August 29, 1995 1:13 pm PDT */

#include <stdio.h>

#if ((defined(WIN32) || defined(WIN16)) && defined(_WINIO))
#include <winiodef.h>
#endif /* ((defined(WIN32) || defined(WIN16)) && defined(_WINIO)) */

#include <sys/types.h>

#include <fcntl.h>

enum sunrpc_subtype {
  sunrpc_nonbatching_nonconcurrent,
  sunrpc_batching_nonconcurrent,
  sunrpc_nonbatching_concurrent,
  sunrpc_batching_concurrent
  };

struct SunRPC {
  /* L1 >= {prmu}; L2, Main unconstrained */
  
  ilu_boolean batching;
  ilu_boolean concurrent;
};

typedef struct {
  /* L1 >= {prmu}; L2, Main unconstrained */

  ilu_Class sui_class;
  ilu_string sui_type_id;
  ilu_cardinal sui_pnumber, sui_version;
} sunrpcinfo;

#define SUNRPC(conn)		((struct SunRPC *)((conn)->co_protocol_data))

