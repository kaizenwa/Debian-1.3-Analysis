
#ifdef __cplusplus
extern "C" {
#endif

#define MOUNT_MIXED 1
#define MOUNT_BINARY 2
#define MOUNT_SILENT 4

int mount (const char *, const char *, int flags);
int umount (const char *);
#ifdef __cplusplus
};
#endif
