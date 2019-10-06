/*
 * Date: 2012-02-18
 * Author: leike@informatik.uni-freiburg.de
 *
 * Ranking function: f(x, c) = x + c;
 * needs the for the lower bound to be able to depend on c.
 */

typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);

int main()
{
    int c, x;
	x = __VERIFIER_nondet_int();
	
  #if (A1) x=[-32768,-4]; #endif
  #if (A2) x=[-4,-1]; #endif
  #if (A3) x=[0,0]; #endif	
  #if (A4) x=[1,4]; #endif		
  #if (A5) x=[4,32767]; #endif		
	
	c = __VERIFIER_nondet_int();
  //prevent overflows
    if(!(-65535<=x && x<=65535)) return 0;
    if(!(-65535<=c && c<=65535)) return 0;
	if (c >= 2) {
	    while (x + c >= 0) {
		    x = x - c;
		    c = c + 1;
	    }
    }
	return 0;
}
