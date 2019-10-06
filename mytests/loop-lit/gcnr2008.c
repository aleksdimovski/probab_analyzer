// Source: Bhargav S. Gulavani, Supratik Chakraborty, Aditya V. Nori, Sriram
// K. Rajamani: "Automatically Refining Abstract Interpretations", TACAS 2008.

#include "assert.h"

int main() {
    int x = 0, y = 0, z = 0, w = 0;
	
  #if (A1) z=z+1; #endif
  #if (A2) z=z+1; #endif
  #if (A3) z=z+1; #endif	
  #if (A4) z=z+1; #endif	
	
    while (__VERIFIER_nondet_int() && y < 10000) {
	if (__VERIFIER_nondet_int()) {
	    x = x + 1;
	    y = y + 100;
	} else if (__VERIFIER_nondet_int()) {
	    if (x >= 4) {
		x = x + 1;
		y = y + 1;
	    }
	} else if (y > 10*w && z >= 100*x) {
	    y = -y;
	}
	w = w + 1;
	z = z + 10;
    }
    assert(x >= 4 && y <= 2);
    return 0;
}
