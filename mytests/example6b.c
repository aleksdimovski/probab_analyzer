/*
TERMINATION

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

int main() {
  int x=[0,9], y=[0,9]; input:
  int s = x - y;
  if (s>=2) y=y+2;
  assert(y>3);
  return 0;
}