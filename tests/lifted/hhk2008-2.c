// Source: Thomas A. Henzinger, Thibaud Hottelier, Laura Kovacs: "Valigator:
// A verification Tool with Bound and Invariant Generation", LPAR 2008

#include "assert.h"

int main() {
    int b = [-500,499]; input:

  int a=0; 	
  #if (A1) a=[-32768,-4]; #endif
  #if (A2) a=[-4,-1]; #endif
  #if (A3) a=[0,0]; #endif	
  #if (A4) a=[1,4]; #endif		
  #if (A5) a=[4,32767]; #endif		
	
    int res, cnt;
    res = a;
    cnt = b;
	
    while (cnt > 0) {
	cnt = cnt - 1;
	res = res + 1;
    }
	
    assert(res==(a + b));
    return 0;
}
