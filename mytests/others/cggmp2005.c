// Source: A. Costan, S. Gaubert, E. Goubault, M. Martel, S. Putot: "A Policy
// Iteration Algorithm for Computing Fixed Points in Static Analysis of
// Programs", CAV 2005

#include "assert.h"

int main() {
    int i=[0,4]; input:
    int j = 10;
    while (j >= i) {
        i = i + 2;
        j = -1 + j;
    }
    assert(j < 6);
    return 0;
}
