#include "assert.h"

int main() {
  int n=[-10,9]; input:
  int k = 0;
  int i = 0;
	
  while( i < n ) {
      i++;
      k++;
  }
  int j = n;
  while( j > 0 ) {      
      j--;
      k--;
  }
  assert(k > 0);
  return 0;
}
