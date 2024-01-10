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

697: Calls PRS.

704: Calls strcpy to copy "/sbin:/sbin/fs.d:/sbinfs:/etc/fs:/etc:"
     to the global static variable fsck_path.

705-706: Calls strcat to append the oldpath environment variable
         to fsck_path.

713: Calls fsck_device.

725: Calls wait_all.

726: Returns local variable status.
```

#### PRS (e2fsprogs-1.10/misc/fsck.c:590)

```txt
Control Flow:
main
    PRS <-- Here

604: Calls load_fs_info.
```

#### load\_fs\_info (e2fsprogs-1.10/misc/fsck.c:133)

```txt
Control Flow:
main
    PRS
        load_fs_info <-- Here

143: Calls setmntent -> _IO_fopen.

148: Calls getmntent.

163: Calls endmntent.
```

#### \_IO\_fopen (libc-4.6.27/libio-4.6.26/iofopen.c:31)

```txt
Control Flow:
main
    PRS
        load_fs_info
            _IO_fopen <-- Here
```

#### getmntent (libc-4.6.27/mntent/mntent.c:6)

```txt
Control Flow:
main
    PRS
        load_fs_info
            _IO_fopen
            getmntent <-- Here
```

#### endmntent (libc-4.6.27/mntent/mntent.c:70)

```txt
Control Flow:
main
    PRS
        load_fs_info
            _IO_fopen
            getmntent
            endmntent <-- Here
```

#### fsck\_device (e2fsprogs-1.10/misc/fsck.c:360)

```txt
Control Flow:
main
    PRS
    fsck_device <-- Here

370: Calls lookup.

377: Calls sprintf to write the program name into the
     local variable prog.

378: Calls execute.
```

#### lookup (e2fsprogs-1.10/misc/fsck.c:180)

```txt
Control Flow:
main
    PRS
    fsck_device
        lookup <-- Here
```

#### execute (e2fsprogs-1.10/misc/fsck.c:221)

```txt
Control Flow:
main
    PRS
    fsck_device
        lookup
        execute <-- Here

237: Calls find_fsck.

253: Calls fork.

257: Child calls execv.

261: Parent calls malloc to allocate a fsck_instance structure.

264-269: Parent initializes the fsck_instance structure and
         inserts it at the head of the instance_list.

271: Returns zero.
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

#### wait\_one (e2fsprogs-1.10/misc/fsck.c:278)

```txt
Control Flow:
main
    PRS
    fsck_device
    wait_all
        wait_one <-- Here
```

#### free\_instance (e2fsprogs-1.10/misc/fsck.c:120)

```txt
Control Flow:
main
    PRS
    fsck_device
    wait_all
        wait_one
        free_instance <-- Here
```

