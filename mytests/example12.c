/*
TERMINATION

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

int main() {
  int x=[-100,99]; input:
  int y=x;
  if (y <= 0) y = -y; 
  assert (y <= 69); 
  return 0;
}