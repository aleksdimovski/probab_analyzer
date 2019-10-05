
// this is a good example; you may show it in the journal version : Aleksandar

int main() {
  int x=[0,9], y=[0,9]; input:
	
  int s = x - y;
  if (s>=2) y=y+1;
	
  assert(y>2);
  return 0;
}