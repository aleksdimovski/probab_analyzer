typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int x;
    int y;
    int tmp;
    int xtmp;
    x = __VERIFIER_nondet_int();
	
  #if (A1) x=[-32768,-2]; #endif
  #if (A2) x=[-2,0]; #endif
  #if (A3) x=[0,2]; #endif	
  #if (A4) x=[2,32767]; #endif
	
    y = __VERIFIER_nondet_int();
    
    while(y > 0 && x > 0) {
        tmp = y;
        xtmp = x;
        
        while(xtmp>=y && y > 0) {
            xtmp = xtmp - y;
        }
        
        y = xtmp;
        x = tmp;
    }
    
    return 0;
}