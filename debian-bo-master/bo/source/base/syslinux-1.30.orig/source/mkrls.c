/*
 * mkrls.c
 *
 * Converts the output of TLINK or EXE2BIN (LDLINUX.BIN) into a boot
 * sector/ldlinux .c file and a raw LDLINUX.SYS file
 *
 * Usage:
 *   mkrls ldlinux.com asmcode.c ldlinux.sys
 */

#include <stdio.h>
#include <stdlib.h>

#define BOOTSEC_AT  (0x7C00-0x0100) /* Offset in file for bootsec */
#define BOOTSEC_LEN 512             /* Must be a multiple of 8 */

main(int argc, char *argv[])
{
    unsigned char buffer[BOOTSEC_LEN];
    FILE *in, *out, *sys;
    int i, j, n;
    int sys_len;
    char *start;

    if ( argc < 4 )
        exit(1);

    in = fopen(argv[1],"rb");
    if ( in == NULL )
    {
        perror(argv[1]);
        exit(1);
    }
    fseek(in,BOOTSEC_AT,SEEK_SET);

    out = fopen(argv[2],"wt");
    if ( out == NULL )
    {
        perror(argv[2]);
        fclose(in);
        exit(1);
    }
    fputs("#include <stdio.h>\n\n", out);   /* For size_t definition */


    fread(buffer,1,BOOTSEC_LEN,in);

    start = "char boot_sector[] =\n{\n\t";
    for ( i = 0 ; i < BOOTSEC_LEN ; i += 8 )
    {
        fputs(start, out);
        for ( j = 0 ; j < 7 ; j++ )
        {
            fprintf(out, "0x%02x, ", buffer[i+j]);
        }
        fprintf(out, "0x%02x", buffer[i+7]);
        start = ",\n\t";
    }
    fprintf(out, "\n};\n");

    sys = fopen(argv[3],"wb");
    if ( sys == NULL )
    {
        perror(argv[3]);
        fclose(in);
        fclose(out);
        exit(1);
    }

    start = "\nchar ldlinux_sys[] =\n{\n\t";
    sys_len = 0;

    while ( (n = fread(buffer,1,BOOTSEC_LEN,in)) > 0 )
    {
        sys_len += n;
        fwrite(buffer,1,n,sys);
        for ( i = 0 ; i < n ; i += 8 )
        {
            fputs(start, out);
            for ( j = 0 ; j < 7 && i+j < n-1 ; j++ )
            {
                fprintf(out, "0x%02x, ", buffer[i+j]);
            }
            fprintf(out, "0x%02x", buffer[i+j]);
            start = ",\n\t";
        }
    }
    fprintf(out, "\n};\n\nsize_t ldlinux_len = %d;\n", sys_len);

    fclose(sys);
    fclose(out);
    fclose(in);

    return 0;
}


