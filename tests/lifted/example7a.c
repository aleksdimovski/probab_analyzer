
// this is also nice example to explain in journal version 

int main() {
  int x=[0,9]; input:
  int y=0;	
  #if (A1) y=[-32768,-2]; #endif
  #if (A2) y=[-2,0]; #endif
  #if (A3) y=[0,2]; #endif	
  #if (A4) y=[2,32767]; #endif			
	
  while (x >= 0) {  
    x = x - 1;
	y = y + 1;
  }
	
  assert (y>=10);
  return 0;
}