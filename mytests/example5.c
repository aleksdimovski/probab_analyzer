/*
TERMINATION

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

int main() {
  int x=[0,9], y=[0,9]; input:
	
  if (x<=y) y=x;
  assert(y>=5);
	
  return 0;
}