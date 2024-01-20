## Walkthrough of Debian 1.3's _fsck_ Program

### Contents

1. Code Flow
2. Source Code Commentary

### Code Flow

```txt
```

### Source Code Commentary

#### main (e2fsprogs-1.10/misc/fsck.c:751)

```txt
Control Flow:
main <-- Here

757: Calls PRS.

765: Calls malloc to allocate a string.

767: Calls strcpy to copy "/sbin:/sbin/fs.d:/sbin/fs:/etc/fs:/etc"
     to the global static variable fsck_path.

768: Calls strcat to append ":" to fsck_path.

769: Calls strcat to append oldpath to fsck_path.

705-706: Calls strcat to append the oldpath environment variable
         to fsck_path.

779: Calls fsck_device.

790: Calls wait_all.

791: Calls free on fsck_path.

726: Returns local variable status.
```

#### PRS (e2fsprogs-1.10/misc/fsck.c:647)

```txt
Control Flow:
main
    PRS <-- Here

659: Assigns "fsck" to the global variable progname.

661: Calls load_fs_info.

663-748: Processes program arguments.
```

#### load\_fs\_info (e2fsprogs-1.10/misc/fsck.c:168)

```txt
Control Flow:
main
    PRS
        load_fs_info <-- Here

180: Calls setmntent -> _IO_fopen.

185: Calls getmntent.

186-201: Allocates a fs_info structure and initializes it.

204: Calls endmntent.
```

#### \_IO\_fopen (libc4-4.6.27/libio/iofopen.c:31)

```txt
Control Flow:
main
    PRS
        load_fs_info
            _IO_fopen <-- Here
```

#### getmntent (libc4-4.6.27/mntent/mntent.c:6)

```txt
Control Flow:
main
    PRS
        load_fs_info
            _IO_fopen
            getmntent <-- Here
```

#### endmntent (libc4-4.6.27/mntent/mntent.c:70)

```txt
Control Flow:
main
    PRS
        load_fs_info
            _IO_fopen
            getmntent
            endmntent <-- Here
```

#### fsck\_device (e2fsprogs-1.10/misc/fsck.c:411)

```txt
Control Flow:
main
    PRS
    fsck_device <-- Here

421: Calls lookup.

429: Calls sprintf to write the program name into the
     local variable prog.

430: Calls execute.
```

#### lookup (e2fsprogs-1.10/misc/fsck.c:224)

```txt
Control Flow:
main
    PRS
    fsck_device
        lookup <-- Here
```

#### execute (e2fsprogs-1.10/misc/fsck.c:265)

```txt
Control Flow:
main
    PRS
    fsck_device
        lookup
        execute <-- Here

281: Calls find_fsck.

297: Calls fork.

301: Child calls execv.

305: Parent calls malloc to allocate a fsck_instance structure.

308-313: Parent initializes the fsck_instance structure and
         inserts it at the head of the instance_list.

271: Returns zero.
```

#### main (util-linux-2.5/disk-utils/fsck.minix.c:748)

```txt
Control Flow:
main
    PRS
    fsck_device
        lookup
        execute
            main <-- Here (child)
```

#### wait\_all (e2fsprogs-1.10/misc/fsck.c:342)

```txt
Control Flow:
main
    PRS
    fsck_device
    wait_all <-- Here

348: Calls wait_one.

351: Sets the current instance's exit status to the
     local variable global_status.

352: calls free_instance.

354: Returns global_status.
```

#### wait\_one (e2fsprogs-1.10/misc/fsck.c:322)

```txt
Control Flow:
main
    PRS
    fsck_device
    wait_all
        wait_one <-- Here
```

#### free\_instance (e2fsprogs-1.10/misc/fsck.c:155)

```txt
Control Flow:
main
    PRS
    fsck_device
    wait_all
        wait_one
        free_instance <-- Here
```

