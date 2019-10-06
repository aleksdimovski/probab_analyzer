

int main() { 
  int n = [-10,9]; input:
  int a = 2;
  int i, sn=0;
	
	
  for(i=1; i<=n; i++) {
    if (i<5) sn = sn + a;
  }
  assert(sn==n*a);
}
