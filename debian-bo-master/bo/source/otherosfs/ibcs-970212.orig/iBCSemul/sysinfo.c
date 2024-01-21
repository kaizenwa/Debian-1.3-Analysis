/*
 *  linux/ibcs/sysinfo.c
 *
 *  Copyright (C) 1995  Eric Youngdale
 *
 */

#include <linux/config.h>

#include <linux/module.h>
#include <linux/version.h>

#include <asm/segment.h>
#ifndef KERNEL_DS
#include <linux/config.h>
#endif

#include <linux/mm.h>
#include <linux/errno.h>
#include <ibcs/ibcs.h>
#include <linux/utsname.h>

#ifdef IBCS_TRACE
#include <ibcs/trace.h>
#endif


#define SI_SYSNAME              1       /* return name of operating system */
#define SI_HOSTNAME             2       /* return name of node */
#define SI_RELEASE              3       /* return release of operating system */
#define SI_VERSION              4       /* return version field of utsname */
#define SI_MACHINE              5       /* return kind of machine */
#define SI_ARCHITECTURE         6       /* return instruction set arch */
#define SI_HW_SERIAL            7       /* return hardware serial number */
#define SI_HW_PROVIDER          8       /* return hardware manufacturer */
#define SI_SRPC_DOMAIN          9       /* return secure RPC domain */


int ibcs_sysinfo(int command, char * buf, long count) {
  char * return_string;
  static unsigned int serial_number = 0;
  char buffer[16];
  int error;
  int slen;

  return_string = NULL;

  switch(command)
    {
    case SI_SYSNAME:
      return_string = "Linux";
      break;
    case SI_HOSTNAME:
      return_string = system_utsname.sysname;
      break;
    case SI_RELEASE:
      return_string = system_utsname.release;
      break;
    case SI_MACHINE:
    case SI_ARCHITECTURE:
      return_string = system_utsname.machine;
      break;
    case SI_HW_PROVIDER:
      return_string = "Intel";
      break;
    case SI_HW_SERIAL:
      if( serial_number == 0 )
	{
	  serial_number = 0xdeadbeef; /* Use something HW specific? */
	}
      sprintf(buffer,"%8.8x", serial_number);
      return_string = buffer; /* We need to generate something here. */
      break;
    case SI_VERSION:
    case SI_SRPC_DOMAIN:
      break;
    default:
#ifdef IBCS_TRACE
      if ((ibcs_trace & TRACE_API) || ibcs_func_p->trace) {
	printk(KERN_DEBUG "iBCS2: unsupported sysinfo call %d\n", command);
      }
#endif
      return -EINVAL;
    }

  if (!return_string) return 0;

  error = verify_area(VERIFY_WRITE, buf, count);
  if (error)
    return error;

  slen = (count < strlen(return_string) + 1 ? count : 
	  strlen(return_string) + 1);
  memcpy_tofs(buf, return_string, slen);

  return slen;
}
