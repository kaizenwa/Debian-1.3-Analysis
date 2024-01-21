/*
 * Header file defaults.h - assorted default values for character strings in
 * the volume descriptor.
 *
 * 	$Id: defaults.h,v 1.3 1997/02/23 15:50:38 eric Rel $
 */

#define  PREPARER_DEFAULT 	NULL
#define  PUBLISHER_DEFAULT	NULL
#define  APPID_DEFAULT 		NULL
#define  COPYRIGHT_DEFAULT 	NULL
#define  BIBLIO_DEFAULT 	NULL
#define  ABSTRACT_DEFAULT 	NULL
#define  VOLSET_ID_DEFAULT 	NULL
#define  VOLUME_ID_DEFAULT 	"CDROM"
#define  BOOT_CATALOG_DEFAULT   "boot.catalog"
#define  BOOT_IMAGE_DEFAULT     NULL
#ifdef __QNX__
#define  SYSTEM_ID_DEFAULT 	"QNX"
#endif

#ifdef __osf__
#define  SYSTEM_ID_DEFAULT 	"OSF"
#endif

#ifndef SYSTEM_ID_DEFAULT
#define  SYSTEM_ID_DEFAULT 	"LINUX"
#endif
