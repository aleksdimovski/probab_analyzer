/*
TERMINATION

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

int main() {
  int x=[-10,9];
  int n=[0,9];
  while (x <= n) {  
    assert (x>=0 && x<=n);
	x = x + 1;
  }
  return 0;
}