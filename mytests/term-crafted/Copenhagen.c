/*
 * Date: 2012-02-18
 * Author: heizmann@informatik.uni-freiburg.de
 *
 */

typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);

int main()
{
    int x, y, oldx;
	x = __VERIFIER_nondet_int();
	
  #if (A1) x=[-32768,-4]; #endif
  #if (A2) x=[-4,-1]; #endif
  #if (A3) x=[0,0]; #endif	
  #if (A4) x=[1,4]; #endif		
  #if (A5) x=[4,32767]; #endif	
	
	y = __VERIFIER_nondet_int();
	while (x >= 0 && y >= 0) {
		oldx = x;
		x = y - 1;
		y = oldx - 1;
	}
	return 0;
}
