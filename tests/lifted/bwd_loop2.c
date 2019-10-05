// loop with assert at exit


void main()
{
  int j = [0,9]; input:
  int i = 0;
	
  while (i < 100) {
    i++;
	#if (A1) j++; #endif
    #if (A2) j++; #endif
  }
	
  assert(j <= 105);
}
