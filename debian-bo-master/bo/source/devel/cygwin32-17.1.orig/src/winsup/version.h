/* version numbers for winsup */

/* Increment the major number when a change to the dll invalidates existing
   executables.  Increment the minor number when a change is made to the dll
   that you want to record, but that doesn't invalidate existing executables.
   These numbers are recorded in the executable and in the dll.

   These numbers aren't really the version number of the system, though
   perhaps they should be.  Think of these numbers as being akin to the
   version numbers of /usr/lib/libc.so.* on SunOS.  SunOS 4.1.3_U1 uses
   /usr/lib/libc.so.1.9.2.  */

/* FIXME: How does windows support multiple versions of a dll that uses
   shared data among many applications?  Perhaps we should add the version
   number to the shared data structure's name and/or the dll's name.
   Would that require gcc to link with -lcygwinNN?  Ick.  */

/* The major number started at 16 because the "b15" release was out when
   this file was created.  */
#define CYGWIN_DLL_VERSION_MAJOR 17
#define CYGWIN_DLL_VERSION_MINOR 2

/* String used to fetch registry values.  It should change when there are
   incompatible changes in registry usage.

   !!! Note that it is expected that CYGWIN_DLL_VERSION_MAJOR will change
   much more frequently than CYGWIN_REGISTRY_VERSION.  Don't tie the two
   together.
   FIXME: Perhaps this value should be tied to the "system's" version
   number.  Or perhaps there should be code to copy an old version numbered
   entry to the latest one if the latest one isn't found.  */

#define CYGWIN_REGISTRY_VERSION "b15.0"
