

int main() {
  int x=[0,5], y=[0,10], u=[0,3], v=[0,10]; input:
	
  if (x==y) x=2*y;
  if (u==v) u=3*v;
  int z = x + u;
  assert(z<10);
  return 0;
}