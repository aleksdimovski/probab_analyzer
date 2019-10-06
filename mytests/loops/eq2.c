
int main(void) {
  int w = __VERIFIER_nondet_uint();
	
  #if (A1) w=[-32768,-4]; #endif
  #if (A2) w=[-4,-1]; #endif
  #if (A3) w=[0,0]; #endif	
  #if (A4) w=[1,4]; #endif		
  #if (A5) w=[4,32767]; #endif	
	
  int x = w;
  int y = w + 1;
  int z = x + 1;
  while (__VERIFIER_nondet_uint()) {
    y++;
    z++;
  }
  assert(y == z);
  return 0;
}
