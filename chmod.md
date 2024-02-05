## Walkthrough of Debian 1.3's _chmod_ Program

### Contents

1. Code Flow
2. Source Code Commentary

### Code Flow

```txt
```

### Source Code Commentary

#### main (fileutils-3.16/src/chmod.c:243)

```txt
Control Flow:
main <-- Here

251: Assigns "chmod" to global variable program_name.

252: Calls setlocale.

253: Calls bindtextdomain.

254: Calls textdomain.

262-304: Calls getopt_long to handle program arguments.

324-325: Calls mode_compile.

333: Calls strip_trailing_slashes.

334: Calls change_file_mode.

337: Calls exit.
```

#### bindtextdomain (fileutils-3.16/intl/cat-compat.c:131)

```txt
Control Flow:
main
    setlocale
    bindtextdomain <-- Here
```

#### textdomain (fileutils-3.16/intl/cat-compat.c:63)

```txt
Control Flow:
main
    setlocale
    bindtextdomain
    textdomain <-- Here
```

#### mode\_compile (fileutils-3.16/lib/modechange.c:74)

```txt
Control Flow:
main
    setlocale
    bindtextdomain
    textdomain
    mode_compile <-- Here
```

#### strip\_trailing\_slashes (fileutils-3.16/lib/stripslash.c:35)

```txt
Control Flow:
main
    setlocale
    bindtextdomain
    textdomain
    mode_compile
    strip_trailing_slashes <-- Here
```

#### change\_file\_mode (fileutils-3.16/src/chmod.c:102)

```txt
Control Flow:
main
    setlocale
    bindtextdomain
    textdomain
    mode_compile
    strip_trailing_slashes
    change_file_mode <-- Here

109: Calls sys_lstat.

130: Calls mode_adjust.

136: Calls sys_chmod.

153: Returns local variable errors.
```

#### sys\_lstat (linux/fs/stat.c:149)

```txt
Control Flow:
main
    setlocale
    bindtextdomain
    textdomain
    mode_compile
    strip_trailing_slashes
    change_file_mode
        sys_lstat <-- Here

154: Calls verify_area.

157: Calls lnamei.

160: Calls cp_old_stat.

161: Calls iput.

162: Returns zero.
```

#### lnamei (linux/fs/namei.c:289)

```txt
Control Flow:
main
    setlocale
    bindtextdomain
    textdomain
    mode_compile
    strip_trailing_slashes
    change_file_mode
        sys_lstat
            verify_area
            lnamei <-- Here
```

#### cp\_old\_stat (linux/fs/stat.c:23)

```txt
Control Flow:
main
    setlocale
    bindtextdomain
    textdomain
    mode_compile
    strip_trailing_slashes
    change_file_mode
        sys_lstat
            verify_area
            lnamei
            cp_old_stat <-- Here
```

#### mode\_adjust (fileutils-3.16/lib/modechange.c:245)

```txt
Control Flow:
main
    setlocale
    bindtextdomain
    textdomain
    mode_compile
    strip_trailing_slashes
    change_file_mode
        sys_lstat
        mode_adjust <-- Here
```

#### sys\_chmod (linux/fs/open.c:352)

```txt
Control Flow:
main
    setlocale
    bindtextdomain
    textdomain
    mode_compile
    strip_trailing_slashes
    change_file_mode
        sys_lstat
        mode_adjust
        sys_chmod <-- Here
```

