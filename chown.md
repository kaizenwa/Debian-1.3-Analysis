## Walkthrough of Debian 1.3's _chown_ Program

### Contents

1. Code Flow
2. Source Code Commentary

### Code Flow

```txt
```

### Source Code Commentary

#### main (fileutils-3.16/src/chown.c:260)

```txt
Control Flow:
main <-- Here

268: Assigns "chown" to global variable program_name.

269: Calls setlocale.

270: Calls bindtextdomain.

271: Calls textdomain.

275-301: Calls getopt_long to handle program arguments.

325: Calls parse_user_spec.

333: Calls strip_trailing_slashes.

334: Calls change_file_owner.

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

#### parse\_user\_spec (fileutils-3.16/lib/userspec.c:116)

```txt
Control Flow:
main
    setlocale
    bindtextdomain
    textdomain
    parse_user_spec <-- Here
```

#### strip\_trailing\_slashes (fileutils-3.16/lib/stripslash.c:35)

```txt
Control Flow:
main
    setlocale
    bindtextdomain
    textdomain
    parse_user_spec
    strip_trailing_slashes <-- Here
```

#### change\_file\_owner (fileutils-3.16/src/chown.c:131)

```txt
Control Flow:
main
    setlocale
    bindtextdomain
    textdomain
    parse_user_spec
    strip_trailing_slashes
    change_file_owner <-- Here

138: Calls sys_lstat.

156-157: Calls sys_chown.

171: Returns local variable errors.
```

#### sys\_lstat (linux/fs/stat.c:149)

```txt
Control Flow:
main
    setlocale
    bindtextdomain
    textdomain
    parse_user_spec
    strip_trailing_slashes
    change_file_owner
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
    parse_user_spec
    strip_trailing_slashes
    change_file_owner
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
    parse_user_spec
    strip_trailing_slashes
    change_file_owner
        sys_lstat
            verify_area
            lnamei
            cp_old_stat <-- Here
```

#### sys\_chown (linux/fs/open.c:432)

```txt
Control Flow:
main
    setlocale
    bindtextdomain
    textdomain
    parse_user_spec
    strip_trailing_slashes
    change_file_owner
        sys_lstat
        sys_chown <-- Here

438: Calls lnamei.

484: Calls iput.

485: Returns local variable error.
```
