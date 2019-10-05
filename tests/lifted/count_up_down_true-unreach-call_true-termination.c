extern void __VERIFIER_error() __attribute__ ((__noreturn__));

void __VERIFIER_assert(int cond) {
  if (!(cond)) {
    ERROR: __VERIFIER_error();
  }
  return;
}


int main()
{
  int n = __VERIFIER_nondet_uint();
	
  #if (A1) n=[-32768,-2]; #endif
  #if (A2) n=[-2,0]; #endif
  #if (A3) n=[0,2]; #endif	
  #if (A4) n=[2,32767]; #endif		
	
  int x=n, y=0;
  while(x>0)
  {
    x--;
    #if (A1) y++; #endif
	#if (A1) y++; #endif
  }
  assert(y==n);
}

