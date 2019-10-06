
int main(void) {
  int w = __VERIFIER_nondet_uint();
  #if (A1) w=[-32768,0]; #endif	
  #if (A2) w=[0,32767]; #endif
  int x = w;
		
  int y = __VERIFIER_nondet_uint();
  #if (A3) y=[-32768,0]; #endif	
  #if (A4) y=[0,32767]; #endif	
  int z = y;
  while (__VERIFIER_nondet_uint()) {
    if (__VERIFIER_nondet_uint()) {
      ++w; ++x;
    } else {
      --y; --z;
    }
  }
  assert(w == x && y == z);
  return 0;
}
