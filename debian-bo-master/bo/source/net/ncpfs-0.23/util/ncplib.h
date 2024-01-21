/*
 *  ncplib.h
 *
 *  Copyright (C) 1995, 1996 by Volker Lendecke
 *
 */

#ifndef _NCPLIB_H
#define _NCPLIB_H

#include <linux/types.h>
#include <linux/ncp.h>
#include <linux/ncp_fs.h>
#include <linux/ipx.h>
#include <sys/param.h>
#include <stdio.h>
#include <time.h>

#include "ipxlib.h"
#include "com_err.h"

#ifndef memzero
#include <string.h>
#define memzero(object) memset(&(object), 0, sizeof(object))
#endif

void
str_upper(char *name);

enum connect_state {
	NOT_CONNECTED = 0,
	CONN_PERMANENT,
	CONN_TEMPORARY
};

struct ncp_conn {

	enum connect_state is_connected;

	char server[NCP_BINDERY_NAME_LEN];
	char user  [NCP_BINDERY_NAME_LEN];

	struct ncp_fs_info i;

	/* Fields for use with permanent connections */
	int mount_fid;
	char mount_point[MAXPATHLEN];

	/* Fields for use with temporary connections */
	int ncp_sock;
	int wdog_sock;
	int wdog_pid;
	__u8 sequence;
	int completion;
	int conn_status;
	int reply_size;

	/* Fields used to setup ncp requests */
	int current_size;
	int has_subfunction;
	int verbose;
	int ncp_reply_size;

	int lock;

	char packet[NCP_PACKET_SIZE];
};

struct ncp_conn_spec {
	char server[NCP_BINDERY_NAME_LEN];
	char user[NCP_BINDERY_NAME_LEN];
	uid_t uid;
	int login_type;		/* NCP_BINDERY_USER / NCP_BINDERY_PSERVER */
	char password[NCP_BINDERY_NAME_LEN];
};

struct ncp_search_seq {
	struct nw_search_sequence s;
	int namespace;
};

struct ncp_property_info {
	__u8 property_name[16];
	__u8 property_flags;
	__u8 property_security;
	__u32 search_instance;
	__u8 value_available_flag;
	__u8 more_properties_flag;
};

/* ncp_initialize is the main entry point for user programs which want
   to connect to a NetWare Server. It looks for -S, -U, -P and -n in
   the argument list, opens the connection and removes the arguments
   from the list. It was designed after the X Windows init
   functions. */
struct ncp_conn *
ncp_initialize(int *argc, char **argv,
	       int login_necessary, long *err);

/* You can login as another object by this procedure. As a first use
   pserver comes to mind. */
struct ncp_conn *
ncp_initialize_as(int *argc, char **argv,
		  int login_necessary, int login_type, long *err);


/* Open an existing permanent connection */
struct ncp_conn *
ncp_open(const struct ncp_conn_spec *spec, long *err);

/* Open a connection on an existing mount point */
struct ncp_conn *
ncp_open_mount(const char *mount_point, long *err);

/* Find a permanent connection that fits the spec, return NULL if
 * there is none. */
char *
ncp_find_permanent(const struct ncp_conn_spec *spec);

/* Find the address of a file server */
struct sockaddr_ipx *
ncp_find_fileserver(const char *server_name, long *err);

/* Detach from a permanent connection or destroy a temporary
   connection */
long
ncp_close(struct ncp_conn *conn);

/* like getmntent, get_ncp_conn_ent scans /etc/mtab for usable
   connections */

struct ncp_conn_ent {
	char server[NCP_BINDERY_NAME_LEN];
	char user[NCP_BINDERY_NAME_LEN];
	uid_t uid;
	char mount_point[MAXPATHLEN];
};

struct ncp_conn_ent *
ncp_get_conn_ent(FILE *filep);

#define NWCLIENT (".nwclient")
#define NWC_NOPASSWORD ("-")

/* find an appropriate connection */

struct ncp_conn_spec *
ncp_find_conn_spec(const char *server, const char *user, const char *password,
		   int login_necessary, uid_t uid, long *err);

long
ncp_get_file_server_description_strings(struct ncp_conn *conn,
					char target[512]);

long
ncp_get_file_server_time(struct ncp_conn *conn, time_t *target);

long
ncp_get_connlist(struct ncp_conn *conn,
		 __u16 object_type, const char *object_name,
		 int *returned_no, __u8 conn_numbers[256]);

long
ncp_send_broadcast(struct ncp_conn *conn,
		   __u8 no_conn, const __u8 *connections,
		   const char *message);

long
ncp_get_encryption_key(struct ncp_conn *conn,
		       char *target);
long
ncp_get_bindery_object_id(struct ncp_conn *conn,
			  __u16 object_type,
			  const char *object_name,
			  struct ncp_bindery_object *target);
long
ncp_get_bindery_object_name(struct ncp_conn *conn,
			    __u32 object_id,
			    struct ncp_bindery_object *target);
long
ncp_scan_bindery_object(struct ncp_conn *conn,
			__u32 last_id, __u16 object_type, char *search_string,
			struct ncp_bindery_object *target);
long
ncp_create_bindery_object(struct ncp_conn *conn,
			  struct ncp_bindery_object *source);
long
ncp_delete_bindery_object(struct ncp_conn *conn,
			  __u16 object_type,
			  const char *object_name);

struct ncp_prop_login_control {
    __u8    AccountExpireDate[3]     __attribute__ ((packed));
    __u8    Disabled                 __attribute__ ((packed));
    __u8    PasswordExpireDate[3]    __attribute__ ((packed));
    __u8    GraceLogins              __attribute__ ((packed));
    __u16   PasswordExpireInterval   __attribute__ ((packed));
    __u8    MaxGraceLogins           __attribute__ ((packed));
    __u8    MinPasswordLength        __attribute__ ((packed));
    __u16   MaxConnections           __attribute__ ((packed));
    __u8    ConnectionTimeMask[42]   __attribute__ ((packed));
    __u8    LastLogin[6]             __attribute__ ((packed));
    __u8    RestrictionMask          __attribute__ ((packed));
    __u8    reserved                 __attribute__ ((packed));
    __u32   MaxDiskUsage             __attribute__ ((packed));
    __u16   BadLoginCount            __attribute__ ((packed));
    __u32   BadLoginCountDown        __attribute__ ((packed));
    __u8    LastIntruder[8]          __attribute__ ((packed));
};	

long
ncp_read_property_value(struct ncp_conn *conn,
			int object_type, const char *object_name,
			int segment, const char *prop_name,
			struct nw_property *target);
long
ncp_scan_property(struct ncp_conn *conn,
		  __u16 object_type, const char *object_name,
		  __u32 last_id, char *search_string,
		  struct ncp_property_info *property_info);
long
ncp_add_object_to_set(struct ncp_conn *conn,
		      __u16 object_type, const char *object_name,
		      const char *property_name,
		      __u16 member_type,
		      const char *member_name);
long
ncp_change_property_security(struct ncp_conn *conn,
			     __u16 object_type, const char *object_name,
		  	     const char *property_name,
			     __u8 property_security);
long
ncp_create_property(struct ncp_conn *conn,
		     __u16 object_type, const char *object_name,
	  	     const char *property_name,
		     __u8 property_flags, __u8 property_security);
long
ncp_delete_object_from_set(struct ncp_conn *conn,
			   __u16 object_type, const char *object_name,
			   const char *property_name,
			   __u16 member_type,
			   const char *member_name);
long 
ncp_delete_property(struct ncp_conn *conn,
		    __u16 object_type, const char *object_name,
		    const char *property_name);
long
ncp_write_property_value(struct ncp_conn *conn,
		         __u16 object_type, const char *object_name,
		         const char *property_name,
			 __u8 segment,
			 struct nw_property *property_value);
long
ncp_login_encrypted(struct ncp_conn *conn,
		    const struct ncp_bindery_object *object,
		    const unsigned char *key,
		    const unsigned char *passwd);

#define NCP_GRACE_PERIOD (0xdf)

long
ncp_login_user(struct ncp_conn *conn,
	       const unsigned char *username,
	       const unsigned char *password);
long
ncp_get_volume_info_with_number(struct ncp_conn *conn, int n,
				struct ncp_volume_info *target);

long
ncp_get_volume_number(struct ncp_conn *conn, const char *name,
		      int *target);

long
ncp_file_search_init(struct ncp_conn *conn,
		     int dir_handle, const char *path,
		     struct ncp_filesearch_info *target);

long
ncp_file_search_continue(struct ncp_conn *conn,
			 struct ncp_filesearch_info *fsinfo,
			 int attributes, const char *path,
			 struct ncp_file_info *target);

long
ncp_get_finfo(struct ncp_conn *conn,
	      int dir_handle, const char *path, const char *name,
	      struct ncp_file_info *target);

long
ncp_open_file(struct ncp_conn *conn,
	      int dir_handle, const char *path,
	      int attr, int access,
	      struct ncp_file_info *target);
long
ncp_close_file(struct ncp_conn *conn, const char *file_id);

long
ncp_create_newfile(struct ncp_conn *conn,
		   int dir_handle, const char *path,
		   int attr,
		   struct ncp_file_info *target);

long
ncp_create_file(struct ncp_conn *conn,
		int dir_handle, const char *path,
		int attr,
		struct ncp_file_info *target);

long
ncp_erase_file(struct ncp_conn *conn,
	       int dir_handle, const char *path,
	       int attr);

long
ncp_rename_file(struct ncp_conn *conn,
		int old_handle, const char *old_path,
		int attr,
		int new_handle, const char *new_path);

long
ncp_create_directory(struct ncp_conn *conn,
		     int dir_handle, const char *path,
		     int inherit_mask);

long
ncp_delete_directory(struct ncp_conn *conn,
		     int dir_handle, const char *path);

long
ncp_rename_directory(struct ncp_conn *conn,
		     int dir_handle,
		     const char *old_path, const char *new_path);

long
ncp_read(struct ncp_conn *conn, const char *file_id,
	 off_t offset, size_t count, char *target);

long
ncp_write(struct ncp_conn *conn, const char *file_id,
	  off_t offset, size_t count, const char *source);

long
ncp_copy_file(struct ncp_conn *conn,
	      const char source_file[6],
	      const char target_file[6],
	      __u32 source_offset,
	      __u32 target_offset,
	      __u32 count,
	      __u32 *copied_count);

long
ncp_do_lookup(struct ncp_conn *conn,
	      struct nw_info_struct *dir,
	      char *path,	/* may only be one component */
	      struct nw_info_struct *target);

long
ncp_modify_file_or_subdir_dos_info(struct ncp_conn *conn,
				   struct nw_info_struct *file,
				   __u32 info_mask,
				   struct nw_modify_dos_info *info);

long
ncp_del_file_or_subdir(struct ncp_conn *conn,
		       struct nw_info_struct *dir, char *name);


long
ncp_open_create_file_or_subdir(struct ncp_conn *conn,
			       struct nw_info_struct *dir, char *name,
			       int open_create_mode,
			       __u32 create_attributes,
			       int desired_acc_rights,
			       struct nw_file_info *target);

long
ncp_initialize_search(struct ncp_conn *conn,
		      const struct nw_info_struct *dir,
		      int    namespace,
		      struct ncp_search_seq *target);

long
ncp_search_for_file_or_subdir(struct ncp_conn *conn,
			      struct ncp_search_seq *seq,
			      struct nw_info_struct *target);

long
ncp_ren_or_mov_file_or_subdir(struct ncp_conn *conn,
			      struct nw_info_struct *old_dir, char *old_name,
			      struct nw_info_struct *new_dir, char *new_name);

long
ncp_create_queue_job_and_file(struct ncp_conn *conn,
			      __u32 queue_id,
			      struct queue_job *job);

long
ncp_close_file_and_start_job(struct ncp_conn *conn,
			     __u32 queue_id,
			     struct queue_job *job);

long
ncp_attach_to_queue(struct ncp_conn *conn,
		    __u32 queue_id);

long
ncp_detach_from_queue(struct ncp_conn *conn,
		      __u32 queue_id);

long
ncp_service_queue_job(struct ncp_conn *conn, __u32 queue_id, __u16 job_type,
		      struct queue_job *job);

long
ncp_finish_servicing_job(struct ncp_conn *conn, __u32 queue_id,
			 __u32 job_number, __u32 charge_info);

long
ncp_abort_servicing_job(struct ncp_conn *conn, __u32 queue_id,
			__u32 job_number);

long
ncp_get_broadcast_message(struct ncp_conn *conn, char message[256]);

long
ncp_dealloc_dir_handle(struct ncp_conn *conn, __u8 dir_handle);

#define NCP_ALLOC_PERMANENT (0x0000)
#define NCP_ALLOC_TEMPORARY (0x0001)
#define NCP_ALLOC_SPECIAL   (0x0002)

long
ncp_alloc_short_dir_handle(struct ncp_conn *conn,
			   struct nw_info_struct *dir,
			   __u16   alloc_mode,
			   __u8  *target);

#endif /* _NCPLIB_H */
