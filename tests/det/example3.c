
int main() {
  int x=[-9,10], z=[0,9]; input:
	
  int y=0;
	
  while (x >= 0) {
	z = z + 1;
    if (y <= 17) {y = y + 1;}
	x = x - 1;
  }
	
  assert(x<=-1);
  return 0;
}