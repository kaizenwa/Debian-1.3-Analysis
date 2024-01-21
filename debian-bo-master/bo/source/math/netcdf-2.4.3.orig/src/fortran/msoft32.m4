divert(-1)

# Name of system platform (for use in comments)
define(`M4__SYSTEM', MICROSOFT32)

# Includes needed at the top of a file of C to be called from FORTRAN
define(`M4__STRING_DESCRIPTOR_INCLUDES',`')

# transformation from fortran name to name of C module
define(`NAMEF',`$1`'define(`STR_COUNT',1)')# for microsoft, just use same name

# transformation from string name to corresponding argument name
define(`STRINGF',`$1, $1len')

# extra arguments, if any, for string length
define(`STRINGX',`')

# declaration to be used for argument name descriptor
define(`STRINGD',`
    char *$1;	`$2'
    unsigned int $1`'`len';') #declare argument string with extra stringlen parameter

# declarations and initializations of canonical local variables
define(`STRINGL',`')

# C integral type equivalent to a FORTRAN INTEGER
define(`F_INTEGER',`long') 

# FORTRAN declaration for a long integer (e.g. integer*4 for Microsoft)
define(`LONG_INT',`integer*4')

# FORTRAN declaration for a short integer (e.g. integer*2)
define(`SHORT_INT',`integer*2')

# FORTRAN declaration for an integer byte (e.g. integer*1 or byte)
define(`BYTE_INT',`integer*1')

# FORTRAN declaration for single precision
define(`SINGLE_PRECISION',`real')

# FORTRAN declaration for double precision (e.g. real for a Cray)
define(`DOUBLE_PRECISION',`double precision')

# FORTRAN syntax for including a file
define(`F_INCLUDE',`$`include': "$1"')

# include declaring C interfaces, needed in FORTRAN when calling C, e.g.
# Microsoft FORTRAN needs this
define(`M4__C_INTERFACE_DECLARATIONS',
`F_INCLUDE(msoft32.int)
')

divert(0)dnl
