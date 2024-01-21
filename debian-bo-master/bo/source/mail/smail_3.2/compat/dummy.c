/* @(#) dummy.c,v 1.2 1990/10/24 05:15:36 tron Exp */

/*
 * Provide a dummy reference so there is at least one item in
 * compat.a.  This will keep ld from complaining about empty libraries
 * and such.
 */
dummy_reference() {
    return;
}
