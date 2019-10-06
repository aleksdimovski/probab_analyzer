extern void __VERIFIER_error() __attribute__ ((__noreturn__));
void __VERIFIER_assert(int cond) { if(!(cond)) { ERROR: __VERIFIER_error(); } }
int main(void) {
	int x = [0,4]; input:
	int y = 10000000;
	int z=5000000;
	while(x<y){	
		if(x>=5000000)
			z--;
		x++;
	}
  	assert(z!=0);
}

