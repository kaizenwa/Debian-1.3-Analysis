/* supply the byte order macros for all kernels */

#include <linux/version.h>

#if LINUX_VERSION_CODE < ((2 << 16) + (1 << 8) + 10) /* 2.1.10 */

#include <endian.h>

# undef cpu_to_le32
# undef cpu_to_le16
# undef cpu_to_be32
# undef cpu_to_be16
# undef le32_to_cpu
# undef le16_to_cpu
# undef be32_to_cpu
# undef be16_to_cpu
/* don't have cpu_to_?e?? and ?e??_to_cpu macros for this old kernel version */

# if     defined (__BIG_ENDIAN) && defined (__LITTLE_ENDIAN)
/* newer libc assumed */
#  if     defined __BYTE_ORDER
#   if     __BYTE_ORDER == __BIG_ENDIAN
#    define MY_BIG_ENDIAN 1
#    define MY_LOW_ENDIAN 0
#   else
#    if     __BYTE_ORDER == __LITTLE_ENDIAN
#     define MY_LOW_ENDIAN 1
#     define MY_BIG_ENDIAN 0
#    else
#     error  __BYTE_ORDER is neither __BIG_ENDIAN nor __LITTLE_ENDIAN
#    endif
#   endif
#  else
#   error  when __BIG_ENDIAN and __LITTLE_ENDIAN are defined, __BYTE_ORDER is expected
#  endif
# else
/* old libs assumed */
#  if defined (__BIG_ENDIAN)
#   define MY_BIG_ENDIAN 1
#   define MY_LOW_ENDIAN 0
#  else
#   if defined (__LITTLE_ENDIAN)
#    define MY_LOW_ENDIAN 1
#    define MY_BIG_ENDIAN 0
#   else
#    error need either __BIG_ENDIAN or __LITTLE_ENDIAN
#   endif
#  endif
# endif



# if    MY_BIG_ENDIAN == 1
#  define cpu_to_le32(x) \
        ((unsigned long int)((((unsigned long int)(x) & 0x000000ffU) << 24) | \
                             (((unsigned long int)(x) & 0x0000ff00U) <<  8) | \
                             (((unsigned long int)(x) & 0x00ff0000U) >>  8) | \
                             (((unsigned long int)(x) & 0xff000000U) >> 24)))
#  define cpu_to_le16(x) \
        ((unsigned short int)((((unsigned short int)(x) & 0x00ffU) <<  8) | \
                              (((unsigned short int)(x) & 0xff00U) >>  8)))
#  define le32_to_cpu(x) cpu_to_le32(x)
#  define le16_to_cpu(x) cpu_to_le16(x)
#  define cpu_to_be32(x) (x)
#  define cpu_to_be16(x) (x)
#  define be32_to_cpu(x) (x)
#  define be16_to_cpu(x) (x)
# else
#  define cpu_to_be32(x) \
        ((unsigned long int)((((unsigned long int)(x) & 0x000000ffU) << 24) | \
                             (((unsigned long int)(x) & 0x0000ff00U) <<  8) | \
                             (((unsigned long int)(x) & 0x00ff0000U) >>  8) | \
                             (((unsigned long int)(x) & 0xff000000U) >> 24)))
#  define cpu_to_be16(x) \
        ((unsigned short int)((((unsigned short int)(x) & 0x00ffU) <<  8) | \
                              (((unsigned short int)(x) & 0xff00U) >>  8)))
#  define be32_to_cpu(x) cpu_to_be32(x)
#  define be16_to_cpu(x) cpu_to_be16(x)
#  define cpu_to_le32(x) (x)
#  define cpu_to_le16(x) (x)
#  define le32_to_cpu(x) (x)
#  define le16_to_cpu(x) (x)
# endif
#else
# undef __KERNEL__
# define __KERNEL__
# include <asm/byteorder.h>
# undef __KERNEL__
#endif
