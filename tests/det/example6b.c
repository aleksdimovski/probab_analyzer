/*

This is a good example; you may show it for precision loss; it is already written in the paper but commented : Aleksandar

*/

int main() {
  int x=[0,9], y=[0,9]; input:
	
  int s = x - y;
  if (s>=2) y=y+2;
	
  assert(y>3);
  return 0;
}