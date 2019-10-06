/*
TERMINATION

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

int main() {
  int x=[0,99], n=[0,99]; input:
  while (x < n) {  
	x = x + 1;
  }
  assert (x>90);
  return 0;
}