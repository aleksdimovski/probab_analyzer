//#Termination
/*
 * Date: November 2013
 * Author: heizmann@informatik.uni-freiburg.de
 *
 * 
 */

typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int x;
	int y;
	int z;
	y = __VERIFIER_nondet_int();
	
  #if (A1) y=[-32768,-2]; #endif
  #if (A2) y=[-2,0]; #endif
  #if (A3) y=[0,2]; #endif
  #if (A4) y=[2,32767]; #endif		
	
	z = __VERIFIER_nondet_int();
    if (__VERIFIER_nondet_int() != 0) {
        x = 1;
    } else {
        x = -1;
    }
    if (x > 0) {
        x = x + 1;
    } else {
        x = x - 1;
    }
    if (x > 0) {
        x = x + 1;
    } else {
        x = x - 1;
    }
    if (x > 0) {
        x = x + 1;
    } else {
        x = x - 1;
    }
    if (x > 0) {
        x = x + 1;
    } else {
        x = x - 1;
    }
    if (x > 0) {
        x = x + 1;
    } else {
        x = x - 1;
    }
    if (x > 0) {
        x = x + 1;
    } else {
        x = x - 1;
    }
    if (x > 0) {
        x = x + 1;
    } else {
        x = x - 1;
    }
    if (x > 0) {
        x = x + 1;
    } else {
        x = x - 1;
    }
    while (y<100 && z<100) {
        y = y+x;
        z = z-x;
    }
    return 0;
}
