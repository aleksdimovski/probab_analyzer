#include "assert.h"

int main() {
  int n = [-32768,32767]; input:
  int n,i,k;		
	
  k = n;
  i = 0;
  while( i < n ) {
    k--;
    i = i + 2;
  }

  int j = 0;
 
  while( j < n/2 ) {    
    k--;
    j++;
  }
  assert(k > 0);
  return 0;
}
