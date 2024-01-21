/*
Copyright (c) 1991, 1992, 1993 Xerox Corporation.  All Rights Reserved.  

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
/* $Id: method.h,v 1.5 1994/01/20 05:02:17 janssen Exp $ */
/* Last tweaked by Mike Spreitzer September 21, 1993 11:38 am PDT */

#define method_id(method)			((method)->me_id)
#define method_name(method)			((method)->me_name)
#define method_stub_proc(method)		((method)->me_stubproc)
#define method_asynchronous(method)		((method)->me_asynchronous)
#define method_functional(method)		((method)->me_cacheable)
#define method_exception_count(method)		((method)->me_exceptionCount)
#define method_exception_vector(method)		((method)->me_exceptionVector)
