#include "assert.h"
int main() {
    int n = [10,99], m=[10,99]; input: 
    int k = 0;
    int i,j;

    for (i = 0; i < n; i++) {
	for (j = 0; j < m; j++) {
	    k ++;
	}
    }
    assert(k >= 100);
    return 0;
}
