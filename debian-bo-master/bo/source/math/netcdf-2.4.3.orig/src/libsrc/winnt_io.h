/*
 * include file to fix i/o renaming
 *
 * to be included after:  io.h, fcntl.h, sys/types.h, sys/stat.h
 */

#define open _open
#define close _close
#define read _read
#define write _write
#define lseek _lseek

#define O_APPEND _O_APPEND
#define O_BINARY _O_BINARY
#define O_CREAT  _O_CREAT
#define O_RDONLY _O_RDONLY
#define O_WRONLY _O_WRONLY
#define O_RDWR   _O_RDWR
#define O_TRUNC  _O_TRUNC
#define O_EXCL   _O_EXCL
#define O_BINARY _O_BINARY
