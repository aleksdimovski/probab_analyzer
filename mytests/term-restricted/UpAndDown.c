typedef enum { false, true } bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int i = [0,9]; input:
    int up;
	
    up = 0;
    
    while (0 <= i) {
        if (i == 10) {
            up = 0;
        }
        if (i == 0) {
            up = 1;
        }
        if (up == 1) {
            i = i+1;
        } else {
            i = i-1;
        }
    }
	assert(up<=1);
    
    return 0;
}
