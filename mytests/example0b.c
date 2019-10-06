/*
TERMINATION

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

int main() {

  int x=[0,99]; input:

  if (x>90) x=x+2;
  assert(x>95);

  //return 0;
}