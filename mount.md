## Walkthrough of Debian 1.3 _mount_ Program

### Contents

1. Code Flow
2. Source Code Commentary

### Code Flow

```txt
```

### Source Code Commentary

#### main (mount-2.6d/mount.c:947)

```txt
Control Flow:
main <-- Here

643-684: Calls getopt_long to handle options.

727: Calls canonicalize.

728: Calls getmntfile.

729: Calls getfsspec and getfsfile.

742: Calls mount_one.

767: Calls exit(result).
```

#### canonicalize (mount-2.4/sundries.c:273)

```txt
Control Flow:
main
    canonicalize <-- Here

275: Calls xmalloc to create a buffer of size PATH_MAX + 1.

280-281: Calls realpath and returns the local variable canonical
         if it was successful.

283: Calls strcpy to copy the path argument to canonical if
     realpath was not successful.

284: Returns canonical.
```

#### realpath (mount-2.4/realpath.c:67)

```txt
Control Flow:
main
    canonicalize
        realpath <-- Here

76: Assigns the resolved_path argument to the local variable
    new_path.

82: Calls strcpy to copy the pathname into the local variable
    copy_path.

83: Assigns copy_path to the path argument.

96-99: Writes '/' in new_path and increments path/new_path.

103-105: Skips extra slashes.

109-111: Skips "." and "./".

113-124: Handles ".." and "../".

120-123: Decrements new_path until it points to the first
         character of the previous component.

         Initial State:

         /foo/bar /../
                ^ ^
                | |
                | ---- (--new_path)[0]
                | 
                ---- (--new_path)[-1]

        Final State:

        /foo/ bar/../
            ^ ^ 
            | |
            | ---- (--new_path)[0]
            |
            ---- (--new_path)[-1]

127-132: Copies the next pathname component into new_path.

136-139: Assigns ELOOP to errno and returns NULL if we called
         readlink 32 times.

142: Calls sys_readlink.

150: Null terminates the local variable link_path.

151-153: Assigns resolved_path to new_path for an absolute
         symbolic link.

156-157: Decrements new_path until we reach the previous '/'.

159-162: Assigns ENAMETOOLONG to errno and returns NULL if the
         pathname after evaluating the symbolic link is longer
         than PATH_MAX.

164: Calls strcat to append path to link_path.

165: Calls strcpy to copy link_path to the local variable
     copy_path.

166: Assigns copy_path to path.

169: Writes '/' in the new_path buffer and increments it.

172-173: Handles trailing slash.

175: Null-terminates new_path.

176: Returns resolved_path.
```

#### sys\_readlink (linux-1.2.13/fs/stat.c:191)

```txt
Control Flow:
main
    canonicalize
        realpath
            sys_readlink <-- Here
```

#### getmntfile (mount-2.4/sundries.c:194)

```txt
Control Flow:
main
    canonicalize
    getmntfile <-- Here

201: Calls rewind if F_mtab is already open.

203: Calls getmntent.

211: Returns local variable mnt.
```

#### rewind (libc-4.6.27/libio/stdio/rewind.c:5)

```txt
Control Flow:
main
    canonicalize
    getmntfile
        rewind <-- Here (F_mtab != NULL)

8: Calls CHECK_FILE.

9: Calls _IO_rewind -> _IO_seekoff.
```

#### CHECK\_FILE (libc-4.6.27/libio-4.6.26/ldouble/libioP.h:325)

```txt
Control Flow:
main
    canonicalize
    getmntfile
        rewind
            CHECK_FILE <-- Here
```

#### \_IO\_seekoff (libc-4.6.27/libio-4.6.26/ioseekoffc:28)

```txt
Control Flow:
main
    canonicalize
    getmntfile
        rewind
            CHECK_FILE
            _IO_seekoff <-- Here
```

#### getmntent (libc-4.6.27/mntent/mntent.c:6)

```txt
Control Flow:
main
    canonicalize
    getmntfile
        rewind
        getmntent <-- Here
```

#### getfsspec (mount-2.4/fstab.c:79)

```txt
Control Flow:
main
    canonicalize
    getmntfile
    getfsspec <-- Here

84: Calls setfsent.

87-89: Calls getfsent in a loop until we find the SPEC device
       in fstab.
```

#### setfsent (mount-2.4/fstab.c:17)

```txt
Control Flow:
main
    canonicalize
    getmntfile
    getfsspec
        setfsent <-- Here

22: Calls setmntent("/etc/fstab", "r").

23: return (F_fstab != NULL);
```

#### getfsent (mount-2.4/fstab.c:36)

```txt
Control Flow:
main
    canonicalize
    getmntfile
    getfsspec
        setfsent
        getfsent <-- Here

40: Calls setfsent.

45: Calls getmntent.

53-54: Breaks out of the for loop after finding the
       next entry of /etc/fstab we can process.

57: Returns the fstab entry.
```

#### getfsfile (mount-2.4/fstab.c:62)

```txt
Control Flow:
main
    canonicalize
    getmntfile
    getfsspec
    getfsfile <-- Here

67: Calls setfsent.

70-72: Calls getfsent until we find the FILE dir in
       fstab.

74: Returns the fstab entry.
```

#### mount\_one (mount-2.4/mount.c:337)

```txt
Control Flow:
main
    canonicalize
    getmntfile
    getfsspec
    getfsfile
    mount_one <-- Here

353: Calls parse_opts.
```

#### parse\_opts (mount-2.4/mount.c:180)

```txt
Control Flow:
main
    canonicalize
    getmntfile
    getfsspec
    getfsfile
    mount_one
        parse_opts <-- Here
```
