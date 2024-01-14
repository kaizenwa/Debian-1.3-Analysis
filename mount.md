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

956-996: Calls getopt_long to handle options.

1014: Calls stat -> sys_stat.

1016: Calls lock_mtab.

1018: Calls create_mtab.

1019: Calls open_mtab.

1037: Calls canonicalize.

1038: Calls getmntfile.

1039: Calls getfsspec and getfsfile.

1052: Calls mount_one.

1077: Calls exit(result).
```

#### sys\_stat (linux/fs/stat.c:110)

```txt
Control Flow:
main
    sys_stat <-- Here
```

#### lock\_mtab (mount-2.6d/sundries.c:197)

```txt
Control Flow:
main
    sys_stat
    lock_mtab <-- Here
```

#### create\_mtab (mount-2.6d/mount.c:887)

```txt
Control Flow:
main
    sys_stat
    lock_mtab
    create_mtab <-- Here
```

#### open\_mtab (mount-2.6d/sundries.c:209)

```txt
Control Flow:
main
    sys_stat
    lock_mtab
    create_mtab
    open_mtab <-- Here
```

#### canonicalize (mount-2.6d/sundries.c:399)

```txt
Control Flow:
main
    sys_stat
    lock_mtab
    create_mtab
    open_mtab
    canonicalize <-- Here

409: Calls xmalloc to create a buffer of size PATH_MAX + 1.

411-412: Calls realpath and returns the local variable canonical
         if it was successful.

414: Calls free on canonical if realpath was not successful.

284: return xstrdup(path).
```

#### realpath (mount-2.6d/realpath.c:70)

```txt
Control Flow:
main
    sys_stat
    lock_mtab
    create_mtab
    open_mtab
    canonicalize
        realpath <-- Here

79: Assigns the resolved_path argument to the local variable
    new_path.

89: Calls strcpy to copy the pathname into the local variable
    copy_path.

90: Assigns copy_path to the path argument.

96: Calls getcwd.

104-107: Writes '/' in new_path and increments path/new_path.

111-114: Skips extra slashes.

117-120: Skips "." and "./".

121-132: Handles ".." and "../".

128-130: Decrements new_path until it points to the first
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

135-141: Copies the next pathname component into new_path.

144-147: Assigns ELOOP to errno and returns NULL if we called
         readlink 32 times.

150: Calls readlink -> sys_readlink.

158: Null terminates the local variable link_path.

159-161: Assigns resolved_path to new_path for an absolute
         symbolic link.

164-165: Decrements new_path until we reach the previous '/'.

159-162: Assigns ENAMETOOLONG to errno and returns NULL if the
         pathname after evaluating the symbolic link is longer
         than PATH_MAX.

172: Calls strcat to append path to link_path.

173: Calls strcpy to copy link_path to the local variable
     copy_path.

174: Assigns copy_path to path.

177: Writes '/' in the new_path buffer and increments it.

180-181: Handles trailing slash.

183: Null-terminates new_path.

184: Returns resolved_path.
```

#### sys\_readlink (linux-1.2.13/fs/stat.c:221)

```txt
Control Flow:
main
    sys_stat
    lock_mtab
    create_mtab
    open_mtab
    canonicalize
        realpath
            sys_readlink <-- Here
```

#### getmntfile (mount-2.6d/sundries.c:311)

```txt
Control Flow:
main
    sys_stat
    lock_mtab
    create_mtab
    open_mtab
    canonicalize
    getmntfile <-- Here

312: return getmntfromfile (name, F_mtab);
```

#### getmntfromfile (mount-2.6d/sundries.c:291)

```txt
Control Flow:
main
    sys_stat
    lock_mtab
    create_mtab
    open_mtab
    canonicalize
    getmntfile
        getmntfromfile <-- Here

297: Calls rewind if F_mtab is already open.

299: Calls getmntent.

306: Returns local variable mnt.
```

#### rewind (libc4-4.6.27/libio/stdio/rewind.c:5)

```txt
Control Flow:
main
    sys_stat
    lock_mtab
    create_mtab
    open_mtab
    canonicalize
    getmntfile
        getmntfromfile
            rewind <-- Here (F_mtab != NULL)

8: Calls CHECK_FILE.

9: Calls _IO_rewind -> _IO_seekoff.
```

#### CHECK\_FILE (libc4-4.6.27/libio-4.6.26/ldouble/libioP.h:325)

```txt
Control Flow:
main
    sys_stat
    lock_mtab
    create_mtab
    open_mtab
    canonicalize
    getmntfile
        getmntfromfile
            rewind
                CHECK_FILE <-- Here
```

#### \_IO\_seekoff (libc-4.6.27/libio-4.6.26/ioseekoffc:28)

```txt
Control Flow:
main
    sys_stat
    lock_mtab
    create_mtab
    open_mtab
    canonicalize
    getmntfile
        getmntfromfile
            rewind
                CHECK_FILE
                _IO_seekoff <-- Here
```

#### getmntent (libc4-4.6.27/mntent/mntent.c:6)

```txt
Control Flow:
main
    sys_stat
    lock_mtab
    create_mtab
    open_mtab
    canonicalize
    getmntfile
        rewind
        getmntent <-- Here
```

#### getfsspec (mount-2.6d/fstab.c:79)

```txt
Control Flow:
main
    sys_stat
    lock_mtab
    create_mtab
    open_mtab
    canonicalize
    getmntfile
    getfsspec <-- Here

84: Calls setfsent.

87-89: Calls getfsent in a loop until we find the SPEC device
       in fstab.

91: Returns fstab.
```

#### setfsent (mount-2.4/fstab.c:17)

```txt
Control Flow:
main
    sys_stat
    lock_mtab
    create_mtab
    open_mtab
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
    sys_stat
    lock_mtab
    create_mtab
    open_mtab
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

#### getfsfile (mount-2.6d/fstab.c:62)

```txt
Control Flow:
main
    sys_stat
    lock_mtab
    create_mtab
    open_mtab
    canonicalize
    getmntfile
    getfsspec
    getfsfile <-- Here

67: Calls setfsent.

70-72: Calls getfsent until we find the FILE dir in
       fstab.

74: Returns the fstab entry.
```

#### mount\_one (mount-2.6d/mount.c:537)

```txt
Control Flow:
main
    sys_stat
    lock_mtab
    create_mtab
    open_mtab
    canonicalize
    getmntfile
    getfsspec
    getfsfile
    mount_one <-- Here

573: Calls parse_opts.
```

#### parse\_opts (mount-2.6d/mount.c:232)

```txt
Control Flow:
main
    sys_stat
    lock_mtab
    create_mtab
    open_mtab
    canonicalize
    getmntfile
    getfsspec
    getfsfile
    mount_one
        parse_opts <-- Here
```
