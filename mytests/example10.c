/*
TERMINATION

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

int main() {
  int x=[0,9], i=[0,9]; input:
	
  while (i <= 10) {
	if ([0,1]) x = x + 1;
	i = i + 1;
  }
	
  assert(x<i);
  return 0;
}