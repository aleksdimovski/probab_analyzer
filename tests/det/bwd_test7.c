// equality test, both branches
// good example for precision loss : Aleksandar

void main()
{
  int i = [0,9], t = [-5,4]; input:
	
  int k = 5;
  if (i < 9) k = 5; else k = 2;
  k += t;
	
  assert(k<=5);
}
