## Walkthrough of Debian 1.3's _swapon_ Program

### Contents

1. Code Flow
2. Source Code Commentary

### Code Flow

```txt
```

### Source Code Commentary

#### main (mount-2.6d/swapon.c:119)

```txt
Control Flow:
main <-- Here

128-129: Assigns "swapon" to the global static variable program_name.

134-135: Iterates the local variable all.

165: Calls getfsent.

180: Calls swap.

192: Returns local variable status.
```

#### getfsent (mount-2.6d/fstab.c:36)

```txt
Control Flow:
main
    getfsent <-- Here

40: Calls setfsent.

45: Calls getmntent.

53-54: Breaks out of the for loop after finding the
       next entry of /etc/fstab we can process.

57: Returns the fstab entry.
```

#### setfsent (mount-2.6d/fstab.c:17)

```txt
Control Flow:
main
    getfsent
        setfsent <-- Here

22: Calls setmntent("/etc/fstab", "r") -> _IO_fopen("/etc/fstab", "r").

23: return (F_fstab != NULL);
```

#### \_IO\_fopen (libc4-4.6.27/libio/iofopen.c:31)

```txt
Control Flow:
main
    getfsent
        setfsent
            _IO_fopen <-- Here

35-36: Calls malloc to allocate memory for an _IO_FILE_plus
       structure.

39: Calls _IO_init.

40: Assigns &_IO_file_jumps to the fp->file._jumps field.

    Note: _IO_file_jumps is defined on lines 758-776 of
          libc4-4.6.27/libio/fileops.c.

41: Calls _IO_file_init.

43-44: Calls _IO_file_open and returns the _IO_FILE pointer.
```

#### \_IO\_init (libc-4.6.27/libio-4.6.26/genops.c:462)

```txt
Control Flow:
main
    getfsent
        setfsent
            _IO_fopen
                _IO_init <-- Here
```

#### \_IO\_file\_init (libc-4.2.27/libio-4.6.26/fileops.c:88)

```txt
Control Flow:
main
    getfsent
        setfsent
            _IO_fopen
                _IO_init
                _IO_file_init <-- Here

97: Calls _IO_link_in.
```

#### \_IO\_link\_in (libc-4.6.27/libio-4.6.26/genops.c:48)

```txt
Control Flow:
main
    getfsent
        setfsent
            _IO_fopen
                _IO_init
                _IO_file_init
                    _IO_link_in <-- Here

51-54: Sets the _IO_linked flag and inserts the _IO_file at
       the head of _IO_list_all.
```

#### \_IO\_file\_open (libc-4.6.27/libio-4.6.26/fileops.c:142)

```txt
Control Flow:
main
    getfsent
        setfsent
            _IO_fopen
                _IO_init
                _IO_file_init
                _IO_file_open <-- Here
```

#### getmntent (libc-4.6.27/mntent/mntent.c:6)

```txt
Control Flow:
main
    getfsent
        setfsent
        getmntent <-- Here

13-16: Calls fgets until we skip all of the comment lines.

...
```

#### swap (mount-2.6d/swapon.c:69)

```txt
Control Flow:
main
    getfsent
    swap <-- Here

88: Calls swapon -> sys_swapon.

98: Returns local variable status.
```

#### sys\_swapon (linux/mm/swapfile.c:405)

```txt
Control Flow:
main
    getfsent
    swap
        sys_swapon <-- Here
```
