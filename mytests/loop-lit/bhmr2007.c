// Source: Dirk Beyer, Thomas A. Henzinger, Rupak Majumdar, Andrey
// Rybalchenko: "Path Invariants", PLDI 2007.

#include "assert.h"
int main() {
	int n = [0,9]; input:

    int i, n, a, b;
    i = 0; a = 0; b = 0;
	
    while (i < n) {
        if (__VERIFIER_nondet_int()) {
            a = a + 1;
            b = b + 2;
        } else {
            a = a + 2;
            b = b + 1;
        }
        i = i + 1;
    }
    assert(a + b < 3*n);
    return 0;
}
