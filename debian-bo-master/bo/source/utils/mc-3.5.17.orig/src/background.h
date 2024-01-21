#ifndef __BACKGROUND_H
#define __BACKGROUND_H

/* Background code requires socketpair to be available */
/* Do not add the background code if it is not supported */
#ifdef USE_NETCODE
#    define WITH_BACKGROUND
#endif

/* Used for parent/child communication.  These are numbers that
 * could not possible be a routine address.
 */
enum {
    MSG_CHILD_EXITING,
};

/* First argument passed to real functions */
enum OperationMode {
    Foreground,
    Background
};

enum ReturnType {
    Return_String,
    Return_Integer
};

extern int background_wait;

int do_background (void);
int background_attention (int fd, void *xpid);
void tell_parent (int msg);
int parent_call (void *routine, int argc, ...);
int call_1s (int (*routine)(enum OperationMode, char *), char *str);

/* stubs */
void message_1s (int flags, char *title, char *str1);
void message_2s (int flags, char *title, char *str1, char *str2);
void message_3s (int flags, char *title, char *str1, char *str2, const char *str3);
void message_1s1d (int flags, char *title, char *str, int d);

#endif
