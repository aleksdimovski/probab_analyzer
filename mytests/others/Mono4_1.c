extern void __VERIFIER_error() __attribute__ ((__noreturn__));
void __VERIFIER_assert(int cond) { if(!(cond)) { ERROR: __VERIFIER_error(); } }

int main() {
    int x = [0,4]; input:
    int y = 500000;
    while(x < 1000000) {
	if (x < 500000) {
	    x = x + 1;
	} else {
	    x = x + 1;
	    y = y + 1;
	}
    }
    assert(y!=x);
    return 0;
}
